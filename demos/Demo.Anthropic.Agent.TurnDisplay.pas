unit Demo.Anthropic.Agent.TurnDisplay;

interface

uses
  System.SysUtils, System.Classes,
  WVPythia.Chat.Interfaces, WVPythia.ChatSession.Controller,
  WVPythia.Chat.DisplayBlocks;

type
  TAnthropicAgentTurnDisplay = class(TInterfacedObject, IPythiaTurnDisplay)
  strict private
    FBrowser: IPythiaBrowser;
    FBlocks: IPythiaDisplayBlockAggregator;

    procedure Queue(const P: TThreadProcedure);
    procedure QueueAssistantDelta(const AText: string);
    procedure StreamAssistantTextToBrowser(const AText: string);
    procedure ToolError(const ATitle, ADetail: string);
    procedure ToolOutput(const AText: string);
  public
    constructor Create(const ABrowser: IPythiaBrowser);

    function CloneDisplayBlocks: TArray<TChatDisplayBlock>;

    procedure AssistantDelta(const AText: string);
    procedure AssistantText(const AText: string;
      const CloseBlock: Boolean = False); overload;
    procedure AssistantText(const ABlockText, ABrowserText: string;
      const CloseBlock: Boolean = False); overload;
    procedure BrowserError(const AMessage: string);
    procedure ErrorStatus(const ATitle, ADetail: string);
    procedure ReasoningDelta(const AText: string);
    procedure Status(const ATitle: string); overload;
    procedure Status(const ATitle, ADetail: string); overload;
    procedure ToolResult(const AKey, ATitle, AOutput: string;
      const IsError: Boolean);
    procedure ToolResultStatus(const AKey, ATitle, AOutput: string);
    procedure ToolStatus(const ATitle: string); overload;
    procedure ToolStatus(const ATitle, ADetail: string); overload;
    procedure ToolUse(const AKey, ATitle: string;
      const NotifyBrowser: Boolean = True);
  end;

implementation

const
  WHOLE_MESSAGE_STREAM_THRESHOLD = 240;
  WHOLE_MESSAGE_STREAM_CHUNK_SIZE = 96;
  WHOLE_MESSAGE_STREAM_DELAY_MS = 8;

{$REGION 'Dev note'}
(*

  Vendor display bridge for the Managed Agents turn (Demo.Anthropic.Session.Flow).

  TAnthropicAgentTurnDisplay is the Anthropic-specific implementation of
  IPythiaTurnDisplay. TAgentTurn invokes one bridge method per session event
  (assistant delta, reasoning delta, tool_use, tool_result, status, error);
  each call fans out to two destinations:

    1. an internal IPythiaDisplayBlockAggregator that accumulates the typed
       Pythia display blocks used for persistence and replay. This is the
       source of truth for the turn snapshot exposed through
       CloneDisplayBlocks and consumed by TFinalizeData.

    2. the IPythiaBrowser, which paints the live UI while the turn streams
       (DisplayStream, Display, DisplayToolStatus, DisplayToolOutputStream,
       DisplayToolError, DisplayError).

  Threading
  ---------
  Aggregator mutations are SYNCHRONOUS and run on the caller's thread (the
  stream worker thread for the HandleEvent path). Browser calls are always
  marshalled to the UI thread via TThread.Queue, because IPythiaBrowser is
  UI-only. This split is load-bearing: HandleDone finalizes the turn from
  the stream worker thread as soon as the transport closes, so by the time
  CloneDisplayBlocks runs every block must already sit in the aggregator,
  no matter where the matching browser update lands in the UI queue.
  Without that ordering the persisted snapshot could miss the last few
  events.

  Closure capture
  ---------------
  Each queued browser call captures its title/detail/text through the
  procedure parameter list rather than through a shared outer local, so
  iterations that queue several closures in a row (typically Status calls
  emitted from a loop like TAgentTurn.UploadFolderFiles) never alias an
  outer mutable frame. See [Delphi closure-in-loop capture] for the
  underlying constraint.

  Pairing key
  -----------
  ToolUse / ToolResult / ToolResultStatus all carry AKey, the tool-use id
  resolved by TAgentTurn.ToolUseKey. The aggregator uses it to attach a
  result to the matching tool-use block instead of opening a fresh entry,
  so a single persisted block carries both the call and its output.

  Block vs browser text split
  ---------------------------
  AssistantText accepts distinct ABlockText / ABrowserText payloads. The
  block side keeps the canonical assistant text destined for replay, while
  the browser side can show a richer / decorated variant (e.g. the
  italicized interrupt notice emitted by HandleEvent on user cancellation)
  without polluting the persisted history.

  Whole-message replay
  --------------------
  Managed Agents may deliver coordinator prose as complete agent.message
  payloads, including the final answer. Large assistant payloads are replayed
  to the browser in small chunks so the live UI keeps a streaming feel, while
  the display-block accumulator still receives the original text once.

*)
{$ENDREGION}

type
  TAssistantStreamChunker = record
  public
    class function Next(const AText: string;
      var AOffset: Integer): string; static;
  end;

{ TAssistantStreamChunker }

class function TAssistantStreamChunker.Next(const AText: string;
  var AOffset: Integer): string;
var
  LowWater: Integer;
  Remaining: Integer;
  Scan: Integer;
  Take: Integer;
begin
  Result := '';
  if AOffset > Length(AText) then
    Exit;

  Remaining := Length(AText) - AOffset + 1;
  Take := Remaining;
  if Take > WHOLE_MESSAGE_STREAM_CHUNK_SIZE then
    begin
      Take := WHOLE_MESSAGE_STREAM_CHUNK_SIZE;
      LowWater := Take - 32;
      if LowWater < 24 then
        LowWater := 24;
      for Scan := Take downto LowWater do
        if CharInSet(AText[AOffset + Scan - 1],
          [#10, #13, #9, ' ', '.', ',', ';', ':', ')', ']']) then
          begin
            Take := Scan;
            Break;
          end;
    end;

  Result := Copy(AText, AOffset, Take);
  Inc(AOffset, Take);
end;

{ TAnthropicAgentTurnDisplay }

constructor TAnthropicAgentTurnDisplay.Create(const ABrowser: IPythiaBrowser);
begin
  inherited Create;
  FBrowser := ABrowser;
  FBlocks := TPythiaDisplayBlockAggregator.Create;
end;

procedure TAnthropicAgentTurnDisplay.Queue(const P: TThreadProcedure);
begin
  TThread.Queue(nil, P);
end;

function TAnthropicAgentTurnDisplay.CloneDisplayBlocks: TArray<TChatDisplayBlock>;
begin
  if Assigned(FBlocks) then
    Result := FBlocks.CloneDisplayBlocks
  else
    Result := nil;
end;

procedure TAnthropicAgentTurnDisplay.AssistantDelta(const AText: string);
begin
  if AText.IsEmpty then
    Exit;

  FBlocks.AppendAssistantDelta(AText);
  StreamAssistantTextToBrowser(AText);
end;

procedure TAnthropicAgentTurnDisplay.QueueAssistantDelta(const AText: string);
begin
  if AText.IsEmpty then
    Exit;

  Queue(
    procedure
    begin
      FBrowser.DisplayStream(AText, '', False);
    end);
end;

procedure TAnthropicAgentTurnDisplay.StreamAssistantTextToBrowser(
  const AText: string);
begin
  if AText.IsEmpty then
    Exit;

  if Length(AText) < WHOLE_MESSAGE_STREAM_THRESHOLD then
    begin
      QueueAssistantDelta(AText);
      Exit;
    end;

  var Offset := 1;
  while Offset <= Length(AText) do
    begin
      var Chunk := TAssistantStreamChunker.Next(AText, Offset);
      QueueAssistantDelta(Chunk);
      TThread.Sleep(WHOLE_MESSAGE_STREAM_DELAY_MS);
    end;
end;

procedure TAnthropicAgentTurnDisplay.AssistantText(const AText: string;
  const CloseBlock: Boolean);
begin
  AssistantText(AText, AText, CloseBlock);
end;

procedure TAnthropicAgentTurnDisplay.AssistantText(
  const ABlockText, ABrowserText: string; const CloseBlock: Boolean);
begin
  if ABlockText.IsEmpty and ABrowserText.IsEmpty then
    Exit;

  if not ABlockText.IsEmpty then
    FBlocks.AppendAssistantText(ABlockText);
  if CloseBlock then
    FBlocks.CloseCurrent;

  if not ABrowserText.IsEmpty then
    Queue(
      procedure
      begin
        FBrowser.Display(ABrowserText, False);
      end);
end;

procedure TAnthropicAgentTurnDisplay.BrowserError(const AMessage: string);
begin
  Queue(
    procedure
    begin
      FBrowser.DisplayError(AMessage);
    end);
end;

procedure TAnthropicAgentTurnDisplay.ErrorStatus(
  const ATitle, ADetail: string);
begin
  FBlocks.AppendStatus(ATitle, ADetail);
  ToolError(ATitle, ADetail);
end;

procedure TAnthropicAgentTurnDisplay.ReasoningDelta(const AText: string);
begin
  if AText.IsEmpty then
    Exit;

  FBlocks.AppendReasoningDelta(AText);
  Queue(
    procedure
    begin
      FBrowser.DisplayStream('', AText, False);
    end);
end;

procedure TAnthropicAgentTurnDisplay.Status(const ATitle: string);
begin
  Status(ATitle, '');
end;

procedure TAnthropicAgentTurnDisplay.Status(const ATitle, ADetail: string);
begin
  {--- Records a status block and marshals its browser update. The title and
       detail are taken as parameters so every queued closure captures its
       own copy: a closure built inside the upload loop must not share a
       frame with the next iteration. }
  FBlocks.AppendStatus(ATitle, ADetail);
  ToolStatus(ATitle, ADetail);
end;

procedure TAnthropicAgentTurnDisplay.ToolError(
  const ATitle, ADetail: string);
begin
  Queue(
    procedure
    begin
      FBrowser.DisplayToolError(ATitle, ADetail, False);
    end);
end;

procedure TAnthropicAgentTurnDisplay.ToolOutput(const AText: string);
begin
  if AText.Trim.IsEmpty then
    Exit;

  Queue(
    procedure
    begin
      FBrowser.DisplayToolOutputStream(AText + sLineBreak, False);
    end);
end;

procedure TAnthropicAgentTurnDisplay.ToolResult(
  const AKey, ATitle, AOutput: string; const IsError: Boolean);
begin
  FBlocks.AppendToolResult(AKey, AOutput, IsError);
  if AOutput.IsEmpty then
    Exit;

  if IsError then
    ToolError(ATitle, AOutput)
  else
    ToolOutput(AOutput);
end;

procedure TAnthropicAgentTurnDisplay.ToolResultStatus(
  const AKey, ATitle, AOutput: string);
begin
  FBlocks.AppendToolResult(AKey, AOutput, False);
  ToolStatus(ATitle, AOutput);
end;

procedure TAnthropicAgentTurnDisplay.ToolStatus(const ATitle: string);
begin
  ToolStatus(ATitle, '');
end;

procedure TAnthropicAgentTurnDisplay.ToolStatus(
  const ATitle, ADetail: string);
begin
  Queue(
    procedure
    begin
      FBrowser.DisplayToolStatus(ATitle, False);
      if not ADetail.Trim.IsEmpty then
        FBrowser.DisplayToolOutputStream(ADetail + sLineBreak, False);
    end);
end;

procedure TAnthropicAgentTurnDisplay.ToolUse(
  const AKey, ATitle: string; const NotifyBrowser: Boolean);
begin
  FBlocks.AppendToolUse(AKey, ATitle);
  if NotifyBrowser then
    ToolStatus(ATitle);
end;

end.
