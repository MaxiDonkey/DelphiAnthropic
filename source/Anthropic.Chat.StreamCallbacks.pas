unit Anthropic.Chat.StreamCallbacks;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Threading,
  Anthropic.Types, Anthropic.Chat.StreamEvents;

type
  {$REGION 'Typed routing'}

  /// <summary>
  /// Snapshot of a tool invocation block streamed by the server
  /// (tool_use / server_tool_use / mcp_tool_use). The input_json
  /// is built up incrementally from input_json_delta events.
  /// </summary>
  TToolCallSnapshot = record
    Index: Int64;
    BlockType: TContentBlockType;
    ToolId: string;
    ToolName: string;
    ServerName: string;
    InputJson: string;
    Stopped: Boolean;

    class function New(
      const AIndex: Int64;
      const ABlockType: TContentBlockType;
      const AToolId, AToolName, AServerName: string): TToolCallSnapshot; static;
  end;

  /// <summary>
  /// Snapshot of a tool-result block streamed by the server
  /// (code_execution_tool_result, mcp_tool_result, web_search_tool_result, etc.).
  /// The Text is built up incrementally from text_delta events.
  /// </summary>
  TToolResultSnapshot = record
    Index: Int64;
    BlockType: TContentBlockType;
    ToolUseId: string;
    Text: string;
    IsError: Boolean;
    Stopped: Boolean;

    class function New(
      const AIndex: Int64;
      const ABlockType: TContentBlockType;
      const AToolUseId: string): TToolResultSnapshot; static;
  end;

  {$ENDREGION}

  TEventData = record
  strict private
    class var FIndex: Integer;
  private
    FDelta: string;
    FId: string;
    FInputJson: string;
    FModel: string;
    FSignature: string;
    FStopReason: TStopReason;
    FStopSequence: string;
    FText: string;
    FThought: string;
    FRawJson: string;

    {$REGION 'Typed routing'}

    FBlockTypeKnown: Boolean;
    FCurrentBlockType: TContentBlockType;
    FCurrentBlockIndex: Int64;
    FAssistantText: string;
    FLastAssistantDelta: string;
    FLastReasoningDelta: string;
    FLastToolInputDelta: string;
    FLastToolResultDelta: string;
    FToolCalls: TArray<TToolCallSnapshot>;
    FToolResults: TArray<TToolResultSnapshot>;

    function FindToolCallIndex(const AIndex: Int64): Integer;
    function FindToolResultIndex(const AIndex: Int64): Integer;
    procedure RegisterToolCallStart(const AChunk: TStreamEvent);
    procedure RegisterToolResultStart(const AChunk: TStreamEvent);
    procedure AppendToolCallInput(const APartial: string);
    procedure AppendToolResultText(const AText: string);
    procedure MarkStopByCurrentIndex;

    {$ENDREGION}

  public
    class function Empty: TEventData; static;
    function Aggregate(const AChunk: TStreamEvent; const ErrorProc: TProc = nil): TEventData;
    property Delta: string read FDelta write FDelta;
    property Id: string read FId write FId;
    property InputJson: string read FInputJson write FInputJson;
    property Model: string read FModel write FModel;
    property RawJson: string read FRawJson write FRawJson;
    property Signature: string read FSignature write FSignature;
    property StopReason: TStopReason read FStopReason write FStopReason;
    property StopSequence: string read FStopSequence write FStopSequence;
    property Text: string read FText write FText;
    property Thought: string read FThought write FThought;
    class property Index: Integer read FIndex write FIndex;

    {$REGION 'Typed routing'}

    /// <summary>
    /// Called by IEventEngineManager before each Aggregate to inform
    /// TEventData of the currently active content block. When BlockTypeKnown
    /// is False (default), Aggregate keeps the legacy behavior unchanged.
    /// </summary>
    procedure SetActiveBlock(const ABlockType: TContentBlockType; const AIndex: Int64);
    procedure ClearActiveBlock;

    property BlockTypeKnown: Boolean read FBlockTypeKnown;
    property CurrentBlockType: TContentBlockType read FCurrentBlockType;
    property CurrentBlockIndex: Int64 read FCurrentBlockIndex;

    /// <summary>
    /// Text streamed in 'text' blocks only. Excludes text emitted by
    /// tool-result blocks (which is captured in ToolResults[].Text).
    /// Populated only when the engine drives typed routing; otherwise empty.
    /// The legacy Text property continues to aggregate ALL text_delta.
    /// </summary>
    property AssistantText: string read FAssistantText;

    /// <summary>Last assistant text delta appended (single chunk).</summary>
    property LastAssistantDelta: string read FLastAssistantDelta;

    /// <summary>Last thinking delta appended (single chunk).</summary>
    property LastReasoningDelta: string read FLastReasoningDelta;

    /// <summary>Last tool-use input_json delta appended (single chunk).</summary>
    property LastToolInputDelta: string read FLastToolInputDelta;

    /// <summary>Last tool-result text delta appended (single chunk).</summary>
    property LastToolResultDelta: string read FLastToolResultDelta;

    property ToolCalls: TArray<TToolCallSnapshot> read FToolCalls;
    property ToolResults: TArray<TToolResultSnapshot> read FToolResults;

    {$ENDREGION}

  end;

  TStreamEventCallBack = record
  private
    FSender: TObject;
    FOnMessageStart: TProc<TObject, TEventData>;
    FOnMessageDelta: TProc<TObject, TEventData>;
    FOnMessageStop: TProc<TObject, TEventData>;
    FOnContentStart: TProc<TObject, TEventData>;
    FOnContentDelta: TProc<TObject, TEventData>;
    FOnContentStop: TProc<TObject, TEventData>;
    FOnError: TProc<TObject, TEventData>;
    FOnCancellation: TProc<TObject>;
    FOnDoCancel: TFunc<Boolean>;

    {$REGION 'Typed routing'}

    FOnAssistantTextDelta: TProc<TObject, TEventData, string>;
    FOnReasoningDelta: TProc<TObject, TEventData, string>;
    FOnToolUseStart: TProc<TObject, TEventData, TToolCallSnapshot>;
    FOnToolUseInputDelta: TProc<TObject, TEventData, string>;
    FOnToolUseStop: TProc<TObject, TEventData, TToolCallSnapshot>;
    FOnToolResultStart: TProc<TObject, TEventData, TToolResultSnapshot>;
    FOnToolResultDelta: TProc<TObject, TEventData, string>;
    FOnToolResultStop: TProc<TObject, TEventData, TToolResultSnapshot>;

    {$ENDREGION}

  public
    property Sender: TObject read FSender write FSender;
    property OnMessageStart: TProc<TObject, TEventData> read FOnMessageStart write FOnMessageStart;
    property OnMessageDelta: TProc<TObject, TEventData> read FOnMessageDelta write FOnMessageDelta;
    property OnMessageStop: TProc<TObject, TEventData> read FOnMessageStop write FOnMessageStop;
    property OnContentStart: TProc<TObject, TEventData> read FOnContentStart write FOnContentStart;
    property OnContentDelta: TProc<TObject, TEventData> read FOnContentDelta write FOnContentDelta;
    property OnContentStop: TProc<TObject, TEventData> read FOnContentStop write FOnContentStop;
    property OnError: TProc<TObject, TEventData> read FOnError write FOnError;
    property OnCancellation: TProc<TObject> read FOnCancellation write FOnCancellation;
    property OnDoCancel: TFunc<Boolean> read FOnDoCancel write FOnDoCancel;

    {$REGION 'Typed routing'}

    /// <summary>Fired when a 'text' block receives a text_delta.</summary>
    property OnAssistantTextDelta: TProc<TObject, TEventData, string>
      read FOnAssistantTextDelta write FOnAssistantTextDelta;

    /// <summary>Fired when a 'thinking' block receives a thinking_delta.</summary>
    property OnReasoningDelta: TProc<TObject, TEventData, string>
      read FOnReasoningDelta write FOnReasoningDelta;

    /// <summary>Fired at content_block_start for a tool_use / server_tool_use / mcp_tool_use.</summary>
    property OnToolUseStart: TProc<TObject, TEventData, TToolCallSnapshot>
      read FOnToolUseStart write FOnToolUseStart;

    /// <summary>Fired on input_json_delta inside a tool-use block.</summary>
    property OnToolUseInputDelta: TProc<TObject, TEventData, string>
      read FOnToolUseInputDelta write FOnToolUseInputDelta;

    /// <summary>Fired at content_block_stop for a tool_use / server_tool_use / mcp_tool_use.</summary>
    property OnToolUseStop: TProc<TObject, TEventData, TToolCallSnapshot>
      read FOnToolUseStop write FOnToolUseStop;

    /// <summary>Fired at content_block_start for a *_tool_result block.</summary>
    property OnToolResultStart: TProc<TObject, TEventData, TToolResultSnapshot>
      read FOnToolResultStart write FOnToolResultStart;

    /// <summary>Fired on text_delta inside a tool_result block.</summary>
    property OnToolResultDelta: TProc<TObject, TEventData, string>
      read FOnToolResultDelta write FOnToolResultDelta;

    /// <summary>Fired at content_block_stop for a tool_result block.</summary>
    property OnToolResultStop: TProc<TObject, TEventData, TToolResultSnapshot>
      read FOnToolResultStop write FOnToolResultStop;

    {$ENDREGION}

  end;

  IStreamEventDispatcher = interface
    ['{4B0DE7D4-7E57-4A9E-8B7A-49E7E7C9B1B7}']
    function GetCallBacks: TStreamEventCallBack;

    procedure DispatchEvent(EventType: TEventType; const Buffer: TEventData);

    {$REGION 'Typed routing'}

    procedure DispatchAssistantTextDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchReasoningDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolUseStart(const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
    procedure DispatchToolUseInputDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolUseStop(const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
    procedure DispatchToolResultStart(const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);
    procedure DispatchToolResultDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolResultStop(const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);

    {$ENDREGION}

    property CallBacks: TStreamEventCallBack read GetCallBacks;
  end;

  TStreamEventDispatcher = class(TInterfacedObject, IStreamEventDispatcher)
  private
    FCallBacks: TStreamEventCallBack;

    procedure Invoke(const Proc: TProc<TObject, TEventData>; const Buffer: TEventData);

    {$REGION 'Typed routing'}

    procedure InvokeDelta(
      const Proc: TProc<TObject, TEventData, string>;
      const Buffer: TEventData;
      const ADelta: string);
    procedure InvokeToolCall(
      const Proc: TProc<TObject, TEventData, TToolCallSnapshot>;
      const Buffer: TEventData;
      const ASnapshot: TToolCallSnapshot);
    procedure InvokeToolResult(
      const Proc: TProc<TObject, TEventData, TToolResultSnapshot>;
      const Buffer: TEventData;
      const ASnapshot: TToolResultSnapshot);

    {$ENDREGION}

  public
    constructor Create(const CallBacks: TFunc<TStreamEventCallBack> = nil);

    function GetCallBacks: TStreamEventCallBack;

    procedure DispatchEvent(EventType: TEventType; const Buffer: TEventData);

    {$REGION 'Typed routing'}

    procedure DispatchAssistantTextDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchReasoningDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolUseStart(const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
    procedure DispatchToolUseInputDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolUseStop(const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
    procedure DispatchToolResultStart(const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);
    procedure DispatchToolResultDelta(const Buffer: TEventData; const ADelta: string);
    procedure DispatchToolResultStop(const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);

    {$ENDREGION}
  end;

implementation

uses
  Anthropic.Chat.Responses;

{ TToolCallSnapshot }

class function TToolCallSnapshot.New(
  const AIndex: Int64;
  const ABlockType: TContentBlockType;
  const AToolId, AToolName, AServerName: string): TToolCallSnapshot;
begin
  Result := Default(TToolCallSnapshot);
  Result.Index := AIndex;
  Result.BlockType := ABlockType;
  Result.ToolId := AToolId;
  Result.ToolName := AToolName;
  Result.ServerName := AServerName;
end;

{ TToolResultSnapshot }

class function TToolResultSnapshot.New(
  const AIndex: Int64;
  const ABlockType: TContentBlockType;
  const AToolUseId: string): TToolResultSnapshot;
begin
  Result := Default(TToolResultSnapshot);
  Result.Index := AIndex;
  Result.BlockType := ABlockType;
  Result.ToolUseId := AToolUseId;
end;

{ TEventData }

function TEventData.Aggregate(const AChunk: TStreamEvent;
  const ErrorProc: TProc): TEventData;
begin
  if not Assigned(AChunk) then
    Exit(Self);

  RawJson := RawJson + AChunk.JSONResponse;

  case AChunk.EventType of
    TEventType.message_start:
      begin
        Id := AChunk.MessageStart.Message.Id;
        Model := AChunk.MessageStart.Message.Model;
        StopReason := AChunk.MessageStart.Message.StopReason;
        StopSequence := AChunk.MessageStart.Message.StopSequence;
      end;

    TEventType.content_block_start:
      begin
        Index := AChunk.ContentBlockStart.Index;

        {--- Typed routing: when the engine has set the active block,
             register the snapshot for tool-use / tool-result blocks. }
        if FBlockTypeKnown then
          case FCurrentBlockType of
            TContentBlockType.tool_use,
            TContentBlockType.server_tool_use,
            TContentBlockType.mcp_tool_use:
              RegisterToolCallStart(AChunk);

            TContentBlockType.web_search_tool_result,
            TContentBlockType.web_fetch_tool_result,
            TContentBlockType.advisor_tool_result,
            TContentBlockType.code_execution_tool_result,
            TContentBlockType.bash_code_execution_tool_result,
            TContentBlockType.text_editor_code_execution_tool_result,
            TContentBlockType.tool_search_tool_result,
            TContentBlockType.mcp_tool_result:
              RegisterToolResultStart(AChunk);
          end;
      end;

    TEventType.content_block_stop:
      begin
        Text := Text + sLineBreak;
        Thought := Thought + sLineBreak;
        Signature := Signature + sLineBreak;
        InputJson := InputJson + sLineBreak;

        {--- Typed routing: mark active tool call/result as stopped. }
        if FBlockTypeKnown then
          MarkStopByCurrentIndex;
      end;

    TEventType.content_block_delta:
      case AChunk.ContentBlockDelta.Delta.&Type of
        TDeltaType.text_delta:
          begin
            Delta := AChunk.ContentBlockDelta.Delta.Text;
            Text := Text + Delta;

            {--- Typed routing for text_delta. }
            if FBlockTypeKnown then
              case FCurrentBlockType of
                TContentBlockType.text:
                  begin
                    FAssistantText := FAssistantText + Delta;
                    FLastAssistantDelta := Delta;
                  end;

                TContentBlockType.web_search_tool_result,
                TContentBlockType.web_fetch_tool_result,
                TContentBlockType.advisor_tool_result,
                TContentBlockType.code_execution_tool_result,
                TContentBlockType.bash_code_execution_tool_result,
                TContentBlockType.text_editor_code_execution_tool_result,
                TContentBlockType.tool_search_tool_result,
                TContentBlockType.mcp_tool_result:
                  begin
                    AppendToolResultText(Delta);
                    FLastToolResultDelta := Delta;
                  end;
              end;
          end;

        TDeltaType.thinking_delta:
          begin
            Thought := Thought + AChunk.ContentBlockDelta.Delta.Thinking;

            {--- Typed routing for thinking_delta. }
            if FBlockTypeKnown
               and (FCurrentBlockType = TContentBlockType.thinking) then
              FLastReasoningDelta := AChunk.ContentBlockDelta.Delta.Thinking;
          end;

        TDeltaType.signature_delta:
          begin
            Signature := Signature + AChunk.ContentBlockDelta.Delta.Signature;
          end;

        TDeltaType.input_json_delta:
          begin
            InputJson := InputJson + AChunk.ContentBlockDelta.Delta.PartialJson;

            {--- Typed routing for input_json_delta. }
            if FBlockTypeKnown then
              case FCurrentBlockType of
                TContentBlockType.tool_use,
                TContentBlockType.server_tool_use,
                TContentBlockType.mcp_tool_use:
                  begin
                    AppendToolCallInput(AChunk.ContentBlockDelta.Delta.PartialJson);
                    FLastToolInputDelta := AChunk.ContentBlockDelta.Delta.PartialJson;
                  end;
              end;
          end;
      end;

    TEventType.error:
      begin
        if Assigned(ErrorProc) then
          ErrorProc();
      end;
  end;
end;

class function TEventData.Empty: TEventData;
begin
  Result := Default(TEventData);
  FIndex := 0;
end;

procedure TEventData.SetActiveBlock(const ABlockType: TContentBlockType;
  const AIndex: Int64);
begin
  FBlockTypeKnown := True;
  FCurrentBlockType := ABlockType;
  FCurrentBlockIndex := AIndex;
  FLastAssistantDelta := '';
  FLastReasoningDelta := '';
  FLastToolInputDelta := '';
  FLastToolResultDelta := '';
end;

procedure TEventData.ClearActiveBlock;
begin
  FBlockTypeKnown := False;
end;

function TEventData.FindToolCallIndex(const AIndex: Int64): Integer;
begin
  for var I := Low(FToolCalls) to High(FToolCalls) do
    if FToolCalls[I].Index = AIndex then
      Exit(I);
  Result := -1;
end;

function TEventData.FindToolResultIndex(const AIndex: Int64): Integer;
begin
  for var I := Low(FToolResults) to High(FToolResults) do
    if FToolResults[I].Index = AIndex then
      Exit(I);
  Result := -1;
end;

procedure TEventData.RegisterToolCallStart(const AChunk: TStreamEvent);
var
  ToolId, ToolName, ServerName: string;
begin
  ToolId := '';
  ToolName := '';
  ServerName := '';
  if Assigned(AChunk.ContentBlockStart)
     and Assigned(AChunk.ContentBlockStart.ContentBlock) then
    begin
      ToolId := AChunk.ContentBlockStart.ContentBlock.Id;
      ToolName := AChunk.ContentBlockStart.ContentBlock.Name;
      ServerName := AChunk.ContentBlockStart.ContentBlock.ServerName;
    end;

  FToolCalls := FToolCalls + [
    TToolCallSnapshot.New(FCurrentBlockIndex, FCurrentBlockType,
      ToolId, ToolName, ServerName)];
end;

procedure TEventData.RegisterToolResultStart(const AChunk: TStreamEvent);
var
  ToolUseId: string;
begin
  ToolUseId := '';
  if Assigned(AChunk.ContentBlockStart)
     and Assigned(AChunk.ContentBlockStart.ContentBlock) then
    ToolUseId := AChunk.ContentBlockStart.ContentBlock.ToolUseId;

  FToolResults := FToolResults + [
    TToolResultSnapshot.New(FCurrentBlockIndex, FCurrentBlockType, ToolUseId)];
end;

procedure TEventData.AppendToolCallInput(const APartial: string);
var
  Idx: Integer;
begin
  Idx := FindToolCallIndex(FCurrentBlockIndex);
  if Idx >= 0 then
    FToolCalls[Idx].InputJson := FToolCalls[Idx].InputJson + APartial;
end;

procedure TEventData.AppendToolResultText(const AText: string);
var
  Idx: Integer;
begin
  Idx := FindToolResultIndex(FCurrentBlockIndex);
  if Idx >= 0 then
    FToolResults[Idx].Text := FToolResults[Idx].Text + AText;
end;

procedure TEventData.MarkStopByCurrentIndex;
var
  Idx: Integer;
begin
  Idx := FindToolCallIndex(FCurrentBlockIndex);
  if Idx >= 0 then
    FToolCalls[Idx].Stopped := True;

  Idx := FindToolResultIndex(FCurrentBlockIndex);
  if Idx >= 0 then
    FToolResults[Idx].Stopped := True;
end;

{ TStreamEventDispatcher }

constructor TStreamEventDispatcher.Create(
  const CallBacks: TFunc<TStreamEventCallBack>);
begin
  inherited Create;
  if Assigned(CallBacks) then
    FCallBacks := CallBacks()
  else
    FCallBacks := Default(TStreamEventCallBack);
end;

procedure TStreamEventDispatcher.DispatchEvent(EventType: TEventType;
  const Buffer: TEventData);
begin
  case EventType of
    TEventType.message_start:
      Invoke(FCallBacks.FOnMessageStart, Buffer);

    TEventType.message_delta:
      Invoke(FCallBacks.OnMessageDelta, Buffer);

    TEventType.message_stop:
      Invoke(FCallBacks.OnMessageStop, Buffer);

    TEventType.content_block_start:
      Invoke(FCallBacks.OnContentStart, Buffer);

    TEventType.content_block_delta:
      Invoke(FCallBacks.OnContentDelta, Buffer);

    TEventType.content_block_stop:
      Invoke(FCallBacks.OnContentStop, Buffer);

    TEventType.error:
      Invoke(FCallBacks.OnError, Buffer);
    else ;
  end;
end;

function TStreamEventDispatcher.GetCallBacks: TStreamEventCallBack;
begin
  Result := FCallBacks;
end;

procedure TStreamEventDispatcher.Invoke(const Proc: TProc<TObject, TEventData>;
  const Buffer: TEventData);
var
  LocalSender: TObject;
  LocalProc: TProc<TObject, TEventData>;
begin
  LocalSender := FCallBacks.Sender;
  LocalProc := Proc;

  if not Assigned(LocalSender) then
    LocalSender := Self;

  if Assigned(LocalProc) then
    begin
      var  Task: ITask := TTask.Run(
      procedure()
      begin
        TThread.Synchronize(nil, procedure
          begin
            LocalProc(LocalSender, Buffer);
          end)
      end);
    end;
end;

{ TStreamEventDispatcher — typed routing (added) }

procedure TStreamEventDispatcher.InvokeDelta(
  const Proc: TProc<TObject, TEventData, string>;
  const Buffer: TEventData;
  const ADelta: string);
var
  LocalSender: TObject;
  LocalProc: TProc<TObject, TEventData, string>;
begin
  LocalProc := Proc;
  if not Assigned(LocalProc) then
    Exit;

  LocalSender := FCallBacks.Sender;
  if not Assigned(LocalSender) then
    LocalSender := Self;

  var Task: ITask := TTask.Run(
    procedure()
    begin
      TThread.Synchronize(nil, procedure
        begin
          LocalProc(LocalSender, Buffer, ADelta);
        end);
    end);
end;

procedure TStreamEventDispatcher.InvokeToolCall(
  const Proc: TProc<TObject, TEventData, TToolCallSnapshot>;
  const Buffer: TEventData;
  const ASnapshot: TToolCallSnapshot);
var
  LocalSender: TObject;
  LocalProc: TProc<TObject, TEventData, TToolCallSnapshot>;
begin
  LocalProc := Proc;
  if not Assigned(LocalProc) then
    Exit;

  LocalSender := FCallBacks.Sender;
  if not Assigned(LocalSender) then
    LocalSender := Self;

  var Task: ITask := TTask.Run(
    procedure()
    begin
      TThread.Synchronize(nil, procedure
        begin
          LocalProc(LocalSender, Buffer, ASnapshot);
        end);
    end);
end;

procedure TStreamEventDispatcher.InvokeToolResult(
  const Proc: TProc<TObject, TEventData, TToolResultSnapshot>;
  const Buffer: TEventData;
  const ASnapshot: TToolResultSnapshot);
var
  LocalSender: TObject;
  LocalProc: TProc<TObject, TEventData, TToolResultSnapshot>;
begin
  LocalProc := Proc;
  if not Assigned(LocalProc) then
    Exit;

  LocalSender := FCallBacks.Sender;
  if not Assigned(LocalSender) then
    LocalSender := Self;

  var Task: ITask := TTask.Run(
    procedure()
    begin
      TThread.Synchronize(nil, procedure
        begin
          LocalProc(LocalSender, Buffer, ASnapshot);
        end);
    end);
end;

procedure TStreamEventDispatcher.DispatchAssistantTextDelta(
  const Buffer: TEventData; const ADelta: string);
begin
  InvokeDelta(FCallBacks.OnAssistantTextDelta, Buffer, ADelta);
end;

procedure TStreamEventDispatcher.DispatchReasoningDelta(
  const Buffer: TEventData; const ADelta: string);
begin
  InvokeDelta(FCallBacks.OnReasoningDelta, Buffer, ADelta);
end;

procedure TStreamEventDispatcher.DispatchToolUseStart(
  const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
begin
  InvokeToolCall(FCallBacks.OnToolUseStart, Buffer, ASnapshot);
end;

procedure TStreamEventDispatcher.DispatchToolUseInputDelta(
  const Buffer: TEventData; const ADelta: string);
begin
  InvokeDelta(FCallBacks.OnToolUseInputDelta, Buffer, ADelta);
end;

procedure TStreamEventDispatcher.DispatchToolUseStop(
  const Buffer: TEventData; const ASnapshot: TToolCallSnapshot);
begin
  InvokeToolCall(FCallBacks.OnToolUseStop, Buffer, ASnapshot);
end;

procedure TStreamEventDispatcher.DispatchToolResultStart(
  const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);
begin
  InvokeToolResult(FCallBacks.OnToolResultStart, Buffer, ASnapshot);
end;

procedure TStreamEventDispatcher.DispatchToolResultDelta(
  const Buffer: TEventData; const ADelta: string);
begin
  InvokeDelta(FCallBacks.OnToolResultDelta, Buffer, ADelta);
end;

procedure TStreamEventDispatcher.DispatchToolResultStop(
  const Buffer: TEventData; const ASnapshot: TToolResultSnapshot);
begin
  InvokeToolResult(FCallBacks.OnToolResultStop, Buffer, ASnapshot);
end;

end.
