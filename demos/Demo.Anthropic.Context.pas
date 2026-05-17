unit Demo.Anthropic.Context;

interface

{$REGION 'Dev note'}
(*

  Conversation context for the pythia-anthropic VCL demo.

  This unit exposes an injectable IContext that lets TAnthropicServices
  (Demo.Anthropic.Services.pas) prepend the in-flight conversation history
  to the messages array assembled before each streamed request.

  History source
  --------------
  History is read from IPythiaBrowser.PersistentChat. Sessions and turns are
  managed by WVPythia.ChatSession.Controller.TPersistentChat: the active
  session is exposed through CurrentChat, and each TChatTurn holds the
  prompt/response pair persisted at the end of a previous round-trip, plus
  the raw streamed response in JsonResponse.

  Tour assembly
  -------------
  Each completed turn produces one user message followed by one assistant
  message, built through the TMessages helper (Anthropic.Helpers.pas). The
  current in-flight content blocks are appended last as the trailing user
  message.

  User-side replay (rich blocks)
  ------------------------------
  The historical user message is rebuilt from TChatTurn.JsonPrompt rather
  than the bare TChatTurn.Prompt text. JsonPrompt holds the full JSON
  request that was sent for that turn, so its last "user" message can be
  walked block by block.

  Self-contained blocks are replayed verbatim:

    - text                                         (the user's prompt)
    - image with base64 / url source               (inlined / referenced)
    - document with base64 PDF / plain text / url  (inlined / referenced)

  Ephemeral file-bound blocks are explicitly DROPPED on replay:

    - container_upload                             (file_id from /v1/files)
    - image / document with a "file" source        (file_id from /v1/files)

  These blocks reference uploads whose file_id may have been invalidated
  between turns (deletion, container destruction, TTL expiration, ...).
  Replaying them as-is yields one of two failures:

    400  messages.N.content.M.file_id: Invalid file_id ``:
         must be a `file_...` ID                  (uploaded id was empty)
    404  File `file_...` not found.               (uploaded id is stale)

  Dropping them keeps the historical text intact, which is enough for the
  model to follow the conversation: any content the prior upload exposed
  to the model (file listing, parsed contents, generated artifacts, ...)
  is preserved on the assistant side via the tool_result blocks rebuilt
  by BuildAssistantContent.

  When the JsonPrompt is empty or cannot be parsed (legacy turns), the
  replay falls back to plain text via TChatTurn.Prompt.

  Assistant replay (rich blocks)
  ------------------------------
  As an example, the assistant message of each historical turn is
  reconstructed from TChatTurn.JsonResponse to carry back the following
  block types:

    - TRedactedThinkingBlockParam                  (reasoning)
    - TServerToolUseBlockParam                     (server-executed tool call)
    - TMCPToolUseBlockParam                        (MCP tool call)
    - TToolResultBlockParam                        (tool result echo)
    - TCodeExecutionToolResultBlockParam           (code-execution result echo)
    - TTextEditorCodeExecutionToolResultBlockParam (text-editor result echo)
    - TBashCodeExecutionToolResultBlockParam       (bash result echo)
    - TMCPToolResultBlockParam                     (MCP result echo)
    - TWebSearchToolResultBlockParam               (web search result echo)

  Note on tool_use / tool_result pairing
  --------------------------------------
  Anthropic's Messages API rejects any historical assistant turn that
  emits a server_tool_use block without echoing back its matching result
  block on replay, for example a text_editor_code_execution server
  tool_use must be followed by text_editor_code_execution_tool_result.
  Each newly supported server_tool_use kind therefore needs its result
  counterpart added to BuildAssistantContent, otherwise the next request
  fails with a 400:

    messages.N: <kind> tool use with id ... was found without a
    corresponding <kind>_tool_result block.

  Plain text blocks are also rebuilt from text_delta deltas so the visible
  assistant content is not lost in the process. Any other block type
  (regular tool_use, citations, container uploads, etc.) is intentionally
  skipped. This unit is a pedagogical sample, not an exhaustive replay.

  JsonResponse format
  -------------------
  TChatTurn.JsonResponse is a sequence of stream-event JSON objects
  separated by sLineBreak, exactly as accumulated by TEventData.Aggregate
  in Anthropic.Chat.StreamCallbacks. The two events that matter for
  reconstruction are:

    {"type":"content_block_start","index":N,"content_block":{...}}
    {"type":"content_block_delta","index":N,"delta":{...}}

  Each event is parsed with WVPythia.JSON.SafeReader.TJsonReader; per-block
  state is accumulated in a snapshot keyed by the block index, then emitted
  as the corresponding T<X>BlockParam (Anthropic.Chat.Request).

  Tool / MCP replay extension
  ---------------------------
  For richer tool-loop replay (interleaved tool_use / tool_result across
  turns) take inspiration from Anthropic.Context.Helper.TTurns; the
  building blocks (TPayload, TGenerationManager.Context.CreateToolUse,
  CreateToolResult) are the same.

  Beta flags aggregation
  ----------------------
  IContext.BetaExtract scans every TChatTurn.JsonPrompt of the current
  session, parses each one as JSON and collects the values found in the
  top-level "beta" array. The result is the deduplicated union of every
  beta that was activated on any prior turn, in order of first
  appearance. This is kept for diagnostics and legacy sessions; live
  request beta handling is centralized in
  Demo.Anthropic.Services.TAnthropicServices.BetaBuilder and the SDK 1.3
  TBetaHeaderManager auto-detection path.

  Container reuse (skills / code execution)
  -----------------------------------------
  When the user enabled a skill (xlsx, code execution, ...) on the first
  turn, the API returned a server-side container in the message_start
  event:

    {"type":"message_start","message":{"id":"...","container":{"id":"cont_..."}}}

  Subsequent turns must echo that container id back in their request
  payload (TChatParams.Container) so the same container, and therefore
  the same loaded skills, working directory, and execution state, is
  reused instead of provisioned again. IContext.LastContainerId scans the
  history backwards and returns the most recent non-empty container.id
  found in any prior TChatTurn.JsonResponse. The service layer reads it
  before assembling the request and merges it into TContainerParams.Id
  alongside the skills selection of the current turn.

*)

{$ENDREGION}

uses
  System.SysUtils, System.Generics.Collections, System.JSON,
  WVPythia.JSON.SafeReader,
  Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.API.JsonSafeReader,
  WVPythia.Chat.Interfaces, WVPythia.ChatSession.Controller, WVPythia.Vendors.Services;

type
  IContext = interface
    ['{9E1085A1-AA34-49CE-ADB7-A5E655D7D204}']
    function HasHistory: Boolean;
    function GetHistory: TArray<TMessageParam>;
    function LastContainerId: string;
    function BetaExtract: TArray<string>;
    function HasHistoricalCodeExecution: Boolean;

    function BuildMessages(
      const AState: TStateBuffer;
      const ACurrentContent: TArray<TContentBlockParam>): TArray<TMessageParam>;
  end;

  TBlockSnapshotKind = (
    bskUnknown,
    bskText,
    bskRedactedThinking,
    bskServerToolUse,
    bskMCPToolUse,
    bskToolResult,
    bskCodeExecutionToolResult,
    bskTextEditorCodeExecutionToolResult,
    bskBashCodeExecutionToolResult,
    bskMCPToolResult,
    bskWebSearchToolResult
  );

  TBlockSnapshot = record
    Index: Integer;
    Kind: TBlockSnapshotKind;
    BlockType: string;
    Id: string;
    Name: string;
    ServerName: string;
    Data: string;
    Text: string;
    InputJson: string;
    ToolUseId: string;
    Content: string;
    IsError: Boolean;
  end;

  TJsonResponseParser = record
  private
    class function MapKind(const ABlockType: string): TBlockSnapshotKind; static;

    class procedure HandleContentBlockStart(
      const AReader: TJsonReader;
      const ASnapshots: TDictionary<Integer, TBlockSnapshot>); static;

    class procedure HandleContentBlockDelta(
      const AReader: TJsonReader;
      const ASnapshots: TDictionary<Integer, TBlockSnapshot>); static;

    class procedure ProcessEvent(
      const AEventJson: string;
      const ASnapshots: TDictionary<Integer, TBlockSnapshot>); static;

    class function FlattenSorted(
      const ASnapshots: TDictionary<Integer, TBlockSnapshot>): TArray<TBlockSnapshot>; static;
  public
    class function Parse(const AJsonResponse: string): TArray<TBlockSnapshot>; static;
  end;

  {--- Concrete escape hatch around the abstract TContentBlockParam:
       allows injecting a raw, already-shaped content block JSON object
       (eg. a container_upload coming from a prior turn's JsonPrompt) into
       the request without going through a typed builder for every variant. }
  TRawContentBlockParam = class(TContentBlockParam);

  TAnthropicContext = class(TInterfacedObject, IContext)
  private
    FBrowser: IPythiaBrowser;

    function CurrentSession: TChatSession;
    function HistoryTurns(const AChat: TChatSession): TArray<TChatTurn>;

    function ExtractContainerIdFromJsonResponse(
      const AJsonResponse: string): string;

    function BuildContentBlockFromJson(
      const ASource: TJSONObject): TContentBlockParam;

    function BuildHistoricalUserContent(
      const ATurn: TChatTurn): TArray<TContentBlockParam>;

    function BuildAssistantContent(
      const ATurn: TChatTurn): TArray<TContentBlockParam>;

    function BuildRedactedThinkingBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildServerToolUseBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildMCPToolUseBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildCodeExecutionToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildTextEditorCodeExecutionToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildBashCodeExecutionToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildMCPToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    function BuildWebSearchToolResultBlock(
      const ASnapshot: TBlockSnapshot): TContentBlockParam;

    procedure AppendTurn(
      var AMessages: TMessages;
      const ATurn: TChatTurn);
  public
    constructor Create(const ABrowser: IPythiaBrowser);

    /// <summary>
    /// Returns True when the current session has previous chat turns available
    /// for context reconstruction.
    /// </summary>
    function HasHistory: Boolean;

    /// <summary>
    /// Rebuilds the current session history as Anthropic message parameters.
    /// Historical turns are replayed in order, preserving structured user
    /// content blocks and assistant response blocks when available.
    /// </summary>
    function GetHistory: TArray<TMessageParam>;

    /// <summary>
    /// Returns the most recent Anthropic container ID found in the current
    /// session history. Completed turns are scanned from newest to oldest, and
    /// the first non-empty container ID is returned so the next request can
    /// reuse the same server-side container.
    /// </summary>
    function LastContainerId: string;

    /// <summary>
    /// Extracts the unique beta feature flags used by previous turns in the
    /// current session. Each completed turn's persisted JSON prompt is parsed,
    /// its top-level beta array is read when present, and values are returned
    /// in order of first appearance. Kept for diagnostics and legacy sessions;
    /// the live beta-header path now relies on TBetaHeaderManager auto-detection
    /// (see Demo.Anthropic.Services.TAnthropicServices.BetaBuilder) plus
    /// HasHistoricalCodeExecution for tool re-registration decisions.
    /// </summary>
    function BetaExtract: TArray<string>;

    /// <summary>
    /// True when the current session history contains at least one
    /// server-executed code_execution variant (plain, text_editor or bash).
    /// Replaces the "beta" array scan that previously drove the
    /// code_execution tool re-registration decision in ToolsBuilder.
    /// </summary>
    function HasHistoricalCodeExecution: Boolean;

    /// <summary>
    /// Builds the Anthropic message timeline by replaying the current session
    /// history first, then appending the current user content blocks. AState is
    /// reserved for future context-window trimming or summarization decisions.
    /// </summary>
    function BuildMessages(
      const AState: TStateBuffer;
      const ACurrentContent: TArray<TContentBlockParam>): TArray<TMessageParam>;

    class function CreateInstance(const ABrowser: IPythiaBrowser): IContext; static;
  end;

implementation

{ TJsonResponseParser }

class function TJsonResponseParser.MapKind(
  const ABlockType: string): TBlockSnapshotKind;
begin
  {--- Maps the wire-level block discriminator to the local enum. Any value
       outside the example scope falls back to bskUnknown so the emitter can
       silently skip it.
  }
  if SameText(ABlockType, 'text') then
    Exit(bskText);

  if SameText(ABlockType, 'redacted_thinking') then
    Exit(bskRedactedThinking);

  if SameText(ABlockType, 'server_tool_use') then
    Exit(bskServerToolUse);

  if SameText(ABlockType, 'mcp_tool_use') then
    Exit(bskMCPToolUse);

  if SameText(ABlockType, 'tool_result') then
    Exit(bskToolResult);

  if SameText(ABlockType, 'code_execution_tool_result') then
    Exit(bskCodeExecutionToolResult);

  if SameText(ABlockType, 'text_editor_code_execution_tool_result') then
    Exit(bskTextEditorCodeExecutionToolResult);

  if SameText(ABlockType, 'bash_code_execution_tool_result') then
    Exit(bskBashCodeExecutionToolResult);

  if SameText(ABlockType, 'mcp_tool_result') then
    Exit(bskMCPToolResult);

  if SameText(ABlockType, 'web_search_tool_result') then
    Exit(bskWebSearchToolResult);

  Result := bskUnknown;
end;

class procedure TJsonResponseParser.HandleContentBlockStart(
  const AReader: TJsonReader;
  const ASnapshots: TDictionary<Integer, TBlockSnapshot>);
var
  Snapshot: TBlockSnapshot;
begin
  (*--- Seeds a snapshot from the immutable fields carried by the
        content_block_start event:

           {"type":"content_block_start","index":N,"content_block":{...}}

        Per-type relevant fields (id, name, server_name, data, tool_use_id,
        initial content/text, is_error) are eagerly read; missing values
        default to empty strings or False, which the emitter treats as
        absent.
  *)
  var Idx := AReader.AsInteger('index', -1);
  if Idx < 0 then
    Exit;

  Snapshot := Default(TBlockSnapshot);
  Snapshot.Index := Idx;
  Snapshot.BlockType := AReader.AsString('content_block.type');
  Snapshot.Kind := MapKind(Snapshot.BlockType);
  Snapshot.Id := AReader.AsString('content_block.id');
  Snapshot.Name := AReader.AsString('content_block.name');
  Snapshot.ServerName := AReader.AsString('content_block.server_name');
  Snapshot.Data := AReader.AsString('content_block.data');
  Snapshot.ToolUseId := AReader.AsString('content_block.tool_use_id');
  Snapshot.Content := AReader.AsString('content_block.content');
  Snapshot.IsError := AReader.AsBoolean('content_block.is_error', False);
  Snapshot.Text := AReader.AsString('content_block.text');

  ASnapshots.AddOrSetValue(Idx, Snapshot);
end;

class procedure TJsonResponseParser.HandleContentBlockDelta(
  const AReader: TJsonReader;
  const ASnapshots: TDictionary<Integer, TBlockSnapshot>);
var
  Snapshot: TBlockSnapshot;
begin
  (*--- Merges a content_block_delta event into the snapshot keyed by index:

           {"type":"content_block_delta","index":N,"delta":{"type":...}}

         Two delta types are aggregated here:
           - input_json_delta concatenates "partial_json" to rebuild the
             full input object of a tool_use / server_tool_use / mcp_tool_use
             block once content_block_stop has been seen.
           - text_delta concatenates "text" to rebuild the visible text of a
             text block.

         Other delta variants (thinking_delta, signature_delta, citations)
         fall outside the example scope and are deliberately ignored.
  *)

  var Idx := AReader.AsInteger('index', -1);
  if Idx < 0 then
    Exit;

  if not ASnapshots.TryGetValue(Idx, Snapshot) then
    Exit;

  var DeltaType := AReader.AsString('delta.type');

  if SameText(DeltaType, 'input_json_delta') then
    Snapshot.InputJson :=
      Snapshot.InputJson + AReader.AsString('delta.partial_json')
  else
  if SameText(DeltaType, 'text_delta') then
    Snapshot.Text :=
      Snapshot.Text + AReader.AsString('delta.text')
  else
    Exit;

  ASnapshots.AddOrSetValue(Idx, Snapshot);
end;

class procedure TJsonResponseParser.ProcessEvent(
  const AEventJson: string;
  const ASnapshots: TDictionary<Integer, TBlockSnapshot>);
begin
  {--- Single-event router: trims, parses with TJsonReader, and dispatches
       on the top-level "type" field. Malformed entries are silently
       skipped so a partially corrupted stream still produces useful
       output.
  }
  if AEventJson.Trim.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(AEventJson);
  if not Reader.IsValid then
    Exit;

  var EventType := Reader.AsString('type');

  if SameText(EventType, 'content_block_start') then
    HandleContentBlockStart(Reader, ASnapshots)
  else
  if SameText(EventType, 'content_block_delta') then
    HandleContentBlockDelta(Reader, ASnapshots);
end;

class function TJsonResponseParser.FlattenSorted(
  const ASnapshots: TDictionary<Integer, TBlockSnapshot>): TArray<TBlockSnapshot>;
begin
  {--- TDictionary does not guarantee enumeration order. Flattening through
       a sorted index list keeps the emitted blocks in the same sequence as
       in the original streamed response.
  }
  Result := [];

  var Indices := TList<Integer>.Create;
  try
    for var Key in ASnapshots.Keys do
      Indices.Add(Key);

    Indices.Sort;

    for var Key in Indices do
      Result := Result + [ASnapshots[Key]];
  finally
    Indices.Free;
  end;
end;

class function TJsonResponseParser.Parse(
  const AJsonResponse: string): TArray<TBlockSnapshot>;
begin
  {--- Splits the persisted JsonResponse on sLineBreak (TEventData.Aggregate
       always inserts that separator before each chunk) and processes each
       event individually. Empty lines, including the leading separator
       are filtered out.
  }
  Result := [];
  if AJsonResponse.Trim.IsEmpty then
    Exit;

  var Snapshots := TDictionary<Integer, TBlockSnapshot>.Create;
  try
    {--- Empty entries (the leading sLineBreak inserted by TEventData.Aggregate
         before the very first event, blank trailing tokens) are skipped by
         ProcessEvent itself, so a plain Split without options is enough.
    }
    for var Event in AJsonResponse.Split([sLineBreak]) do
      ProcessEvent(Event, Snapshots);

    Result := FlattenSorted(Snapshots);
  finally
    Snapshots.Free;
  end;
end;

{ TAnthropicContext }

constructor TAnthropicContext.Create(const ABrowser: IPythiaBrowser);
begin
  inherited Create;
  FBrowser := ABrowser;
end;

class function TAnthropicContext.CreateInstance(
  const ABrowser: IPythiaBrowser): IContext;
begin
  Result := TAnthropicContext.Create(ABrowser);
end;

function TAnthropicContext.CurrentSession: TChatSession;
begin
  {--- Resolves the active TChatSession through the browser-facing
       IPersistentChat. A nil chain is tolerated: it simply yields an empty
       history downstream.
  }
  Result := nil;
  if not Assigned(FBrowser) then
    Exit;

  var Persistent := FBrowser.PersistentChat;
  if not Assigned(Persistent) then
    Exit;

  Result := Persistent.CurrentChat;
end;

function TAnthropicContext.HistoryTurns(
  const AChat: TChatSession): TArray<TChatTurn>;
begin
  {--- Selects the turns that already carry both a prompt and a response.

       The very last entry of TChatSession.Data is the in-flight prompt
       allocated by TPersistentChat.AddPrompt right before streaming starts;
       its Response is still empty at request build time, so it is excluded
       from history replay.
  }
  Result := [];
  if not Assigned(AChat) then
    Exit;

  var Turns := AChat.Data;
  var TurnHigh := High(Turns);
  if TurnHigh < 0 then
    Exit;

  for var I := 0 to TurnHigh - 1 do
    begin
      var Turn := Turns[I];
      if not Assigned(Turn) then
        Continue;

      if Turn.Prompt.Trim.IsEmpty or Turn.Response.Trim.IsEmpty then
        Continue;

      Result := Result + [Turn];
    end;
end;

function TAnthropicContext.BuildRedactedThinkingBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Redacted thinking carries opaque server-side reasoning bytes through
       the "data" field. Replaying it tells the API the prior turn already
       produced redacted reasoning so it can be linked into the next round.
  }
  Result := TRedactedThinkingBlockParam.New
    .Data(ASnapshot.Data);
end;

function TAnthropicContext.BuildServerToolUseBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  (*--- Only synthesize an empty JSON object when no input was captured at all.
        A non-empty but invalid buffer is deliberately not coerced to "{}",
        because that would change the semantics of the replayed tool call.
  *)
  var Block := TServerToolUseBlockParam.New
    .Id(ASnapshot.Id)
    .Name(ASnapshot.Name);

  var InputJson := ASnapshot.InputJson;
  if InputJson.Trim.IsEmpty then
    InputJson := '{}';

  if TJsonCheck.IsValid(InputJson) then
    Block := Block.Input(InputJson);

  Result := Block;
end;

function TAnthropicContext.BuildMCPToolUseBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- MCP tool use mirrors server_tool_use but adds the originating MCP
       server name. The same defensive Input handling applies.
  }
  var Block := TMCPToolUseBlockParam.New
    .Id(ASnapshot.Id)
    .Name(ASnapshot.Name)
    .ServerName(ASnapshot.ServerName);

  var InputJson := ASnapshot.InputJson;
  if InputJson.Trim.IsEmpty then
    InputJson := '{}';

  if TJsonCheck.IsValid(InputJson) then
    Block := Block.Input(InputJson);

  Result := Block;
end;

function TAnthropicContext.BuildToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Tool result echo: only the textual "content" form is replayed here.
       Structured array content (TArray<TContentBlockParam>) would require
       a recursive parse and is left out of this example.
  }
  var Block := TToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId)
    .Content(ASnapshot.Content);

  if ASnapshot.IsError then
    Block := Block.IsError(True);

  Result := Block;
end;

function TAnthropicContext.BuildCodeExecutionToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Server-executed code result. SDK 1.3 registers the current
        code_execution_20260120 tool, whose historical replay must echo
        code_execution_tool_result blocks alongside the matching
        server_tool_use block.

        TCodeExecutionToolResultBlockParam exposes typed Content()
        overloads only (success / error), so the demo keeps the same raw
        JSON-preserving path used by bash and text-editor results.
  }
  var Block := TCodeExecutionToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId);

  if not ASnapshot.Content.Trim.IsEmpty then
    begin
      var Inner := TJSONObject.ParseJSONValue(ASnapshot.Content);
      if Assigned(Inner) then
        Block.Add('content', Inner);
    end;

  Result := Block;
end;

function TAnthropicContext.BuildTextEditorCodeExecutionToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Server-executed text-editor result. The Messages API rejects any
        prior assistant turn that emitted a text_editor_code_execution
        server_tool_use without echoing back its matching
        text_editor_code_execution_tool_result block on replay - the
        request would fail with:

          messages.N: text_editor_code_execution tool use with id ...
          was found without a corresponding
          text_editor_code_execution_tool_result block.

        The result inner shape is polymorphic (view / create / str_replace
        / error) and the SDK exposes typed Content() overloads only. To
        keep the example concise we capture the raw content_block.content
        JSON when the block_start arrives (already stored as
        ASnapshot.Content thanks to TJsonReader.GetPathString falling back
        to ToJSON for object nodes), parse it back into a TJSONValue and
        inject it directly through the inherited TJSONParam.Add('content',
        ...) entry point. This preserves whatever inner type the server
        produced without requiring a per-variant builder here.
  }
  var Block := TTextEditorCodeExecutionToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId);

  if not ASnapshot.Content.Trim.IsEmpty then
    begin
      var Inner := TJSONObject.ParseJSONValue(ASnapshot.Content);
      if Assigned(Inner) then
        Block.Add('content', Inner);
    end;

  Result := Block;
end;

function TAnthropicContext.BuildBashCodeExecutionToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Server-executed bash result. Same pairing constraint as the
        text-editor variant: omitting it on replay triggers a 400 with

          messages.N: bash_code_execution tool use with id ... was found
          without a corresponding bash_code_execution_tool_result block.

        TBashCodeExecutionToolResultBlockParam exposes only typed Content()
        overloads (TBashCodeExecutionResultBlockParam and the matching
        error variant), so we follow the same shortcut as the text-editor
        builder: capture the raw content_block.content JSON in
        ASnapshot.Content and inject it through the inherited
        TJSONParam.Add('content', ...) entry point. The server's original
        inner shape is preserved verbatim, which is all the API requires
        to acknowledge the prior tool_use.
  }
  var Block := TBashCodeExecutionToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId);

  if not ASnapshot.Content.Trim.IsEmpty then
    begin
      var Inner := TJSONObject.ParseJSONValue(ASnapshot.Content);
      if Assigned(Inner) then
        Block.Add('content', Inner);
    end;

  Result := Block;
end;

function TAnthropicContext.BuildMCPToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- MCP tool result. Pairs with a previously emitted mcp_tool_use:

          messages.N: mcp_tool_use with id ... was found without a
          corresponding mcp_tool_result block.

        TMCPToolResultBlockParam exposes both Content(string) and
        Content(TArray<TTextBlockParam>) overloads, but the wire shape
        coming back from the server can also be a heterogeneous JSON
        array of typed text/image content. To preserve that shape
        verbatim we try to parse the captured raw content first:

          - object / array  -> injected through Add('content', JSONValue)
                               so the original structure is replayed.
          - anything else   -> falls back to Content(string) and the leftover
                               TJSONValue (e.g. a bare TJSONString) is freed.

        is_error is a top-level field on the mcp_tool_result block (unlike
        the text-editor / bash variants where the error is conveyed via a
        dedicated inner type), so it is forwarded through IsError() when
        set on the snapshot.
  }
  var Block := TMCPToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId);

  if not ASnapshot.Content.Trim.IsEmpty then
    begin
      var Inner := TJSONObject.ParseJSONValue(ASnapshot.Content);

      if Assigned(Inner) and ((Inner is TJSONObject) or (Inner is TJSONArray)) then
        Block.Add('content', Inner)
      else
        begin
          if Assigned(Inner) then
            Inner.Free;
          Block.Content(ASnapshot.Content);
        end;
    end;

  if ASnapshot.IsError then
    Block.IsError(True);

  Result := Block;
end;

function TAnthropicContext.BuildWebSearchToolResultBlock(
  const ASnapshot: TBlockSnapshot): TContentBlockParam;
begin
  {--- Server-executed web search result. Pairs with a previously emitted
        web_search server_tool_use:

          messages.N: "web_search" tool use with id ... was found without
          a corresponding `web_search_tool_result` block.

        TWebSearchToolResultBlockParam exposes typed Content() overloads
        only (TArray<TWebSearchToolResultBlockItem> for the success path,
        TWebSearchToolRequestError for the error path) - no string
        fallback. Same shortcut as the text-editor / bash builders:
        capture the raw "content_block.content" JSON in ASnapshot.Content,
        parse it back as a TJSONValue (the wire shape is normally an
        array of search-result items) and inject it through the
        inherited TJSONParam.Add('content', ...) entry point so the
        original structure is replayed verbatim.
  }
  var Block := TWebSearchToolResultBlockParam.New
    .ToolUseId(ASnapshot.ToolUseId);

  if not ASnapshot.Content.Trim.IsEmpty then
    begin
      var Inner := TJSONObject.ParseJSONValue(ASnapshot.Content);
      if Assigned(Inner) then
        Block.Add('content', Inner);
    end;

  Result := Block;
end;

function TAnthropicContext.BuildAssistantContent(
  const ATurn: TChatTurn): TArray<TContentBlockParam>;
begin
  {--- Walks the parsed snapshots in original index order and emits the
       block subset covered by this example. Order matters because the
       Messages API expects reasoning blocks before content and tool_use
       before any subsequent tool_result.
  }
  Result := [];

  for var S in TJsonResponseParser.Parse(ATurn.JsonResponse) do
    case S.Kind of
      bskText:
        if not S.Text.Trim.IsEmpty then
          Result := Result + [TTextBlockParam.New.Text(S.Text)];

      bskRedactedThinking:
        if not S.Data.IsEmpty then
          Result := Result + [BuildRedactedThinkingBlock(S)];

      bskServerToolUse:
        Result := Result + [BuildServerToolUseBlock(S)];

      bskMCPToolUse:
        Result := Result + [BuildMCPToolUseBlock(S)];

      bskToolResult:
        Result := Result + [BuildToolResultBlock(S)];

      bskCodeExecutionToolResult:
        Result := Result + [BuildCodeExecutionToolResultBlock(S)];

      bskTextEditorCodeExecutionToolResult:
        Result := Result + [BuildTextEditorCodeExecutionToolResultBlock(S)];

      bskBashCodeExecutionToolResult:
        Result := Result + [BuildBashCodeExecutionToolResultBlock(S)];

      bskMCPToolResult:
        Result := Result + [BuildMCPToolResultBlock(S)];

      bskWebSearchToolResult:
        Result := Result + [BuildWebSearchToolResultBlock(S)];
    end;
end;

function TAnthropicContext_IsEphemeralFileBlock(
  const ABlock: TJSONObject): Boolean;
begin
  (*--- True when the block carries a file_id that may have been
        invalidated between turns. Two shapes are recognized:

          { "type": "container_upload", "file_id": "file_..." }
          { "type": "image" | "document",
            "source": { "type": "file", "file_id": "file_..." } }

        Both reference an upload returned by /v1/files; the underlying
        file may have been deleted, garbage-collected after a container
        ended, or otherwise become unreachable, in which case echoing it
        back triggers a 400 (empty id) or a 404 (stale id) on the next
        request. The caller drops such blocks during user-side replay
        to keep the historical message valid.
  *)
  if not Assigned(ABlock) then
    Exit(False);

  if Assigned(ABlock.GetValue('file_id')) then
    Exit(True);

  var SourceValue := ABlock.GetValue('source');
  if SourceValue is TJSONObject then
    begin
      var SourceType := TJSONObject(SourceValue).GetValue('type');
      if (SourceType is TJSONString) and
         SameText(TJSONString(SourceType).Value, 'file') then
        Exit(True);
    end;

  Result := False;
end;

function TAnthropicContext.BuildContentBlockFromJson(
  const ASource: TJSONObject): TContentBlockParam;
begin
  {--- Builds a content block whose JSON shape is taken verbatim from
        ASource. Each pair is cloned so ASource keeps full ownership of
        its tree; the cloned values are then handed to TJSONParam.Add
        which transfers them into the block's internal TJSONObject.

        This bypass is meant for replay only: it preserves variants for
        which this example has no typed builder (container_upload,
        image/file sources, document/file sources, custom betas, ...)
        without forcing a per-kind dispatch.
  }
  var Block := TRawContentBlockParam.Create;

  for var Pair in ASource do
    Block.Add(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);

  Result := Block;
end;

function TAnthropicContext.BuildHistoricalUserContent(
  const ATurn: TChatTurn): TArray<TContentBlockParam>;
begin
  {--- Recovers the user-visible content blocks of a completed turn from
        TChatTurn.JsonPrompt, while filtering out blocks whose file_id may
        no longer be honored by the API.

        JsonPrompt is the JSON of the request actually sent for the turn;
        its "messages" array ends with that turn's user message (every
        prior entry being older history already replayed by the
        orchestrator). We therefore walk the array from the end and pick
        the last role="user" entry. Its "content" field is then handled in
        two flavors:

          - string  : the API-compact form when the user only sent text;
                      rebuilt as a single TTextBlockParam.
          - array   : the rich form (text, base64 image / document, ...);
                      every object item is rebuilt verbatim through
                      BuildContentBlockFromJson, EXCEPT ephemeral
                      file-bound blocks (see TAnthropicContext_IsEphemeralFileBlock)
                      which are dropped to avoid 400 / 404 on the
                      follow-up request.

        An empty JsonPrompt or an unparsable payload yields an empty
        result, prompting AppendTurn to fall back to plain text via
        TChatTurn.Prompt.
  }
  Result := [];

  if ATurn.JsonPrompt.Trim.IsEmpty then
    Exit;

  var Root := TJSONObject.ParseJSONValue(ATurn.JsonPrompt);
  if not Assigned(Root) then
    Exit;
  try
    if not (Root is TJSONObject) then
      Exit;

    var MessagesValue := TJSONObject(Root).GetValue('messages');
    if not (MessagesValue is TJSONArray) then
      Exit;

    var Messages := TJSONArray(MessagesValue);

    var LastUserMsg: TJSONObject := nil;
    for var I := Messages.Count - 1 downto 0 do
      begin
        var Item := Messages.Items[I];
        if not (Item is TJSONObject) then
          Continue;

        var RoleValue := TJSONObject(Item).GetValue('role');
        if not (RoleValue is TJSONString) then
          Continue;

        if SameText(TJSONString(RoleValue).Value, 'user') then
          begin
            LastUserMsg := TJSONObject(Item);
            Break;
          end;
      end;

    if not Assigned(LastUserMsg) then
      Exit;

    var ContentValue := LastUserMsg.GetValue('content');
    if not Assigned(ContentValue) then
      Exit;

    if ContentValue is TJSONString then
      begin
        var Text := TJSONString(ContentValue).Value;
        if not Text.Trim.IsEmpty then
          Result := [TTextBlockParam.New.Text(Text)];
        Exit;
      end;

    if ContentValue is TJSONArray then
      begin
        var Items := TJSONArray(ContentValue);
        for var I := 0 to Items.Count - 1 do
          begin
            var Item := Items.Items[I];
            if not (Item is TJSONObject) then
              Continue;

            {--- Drop blocks whose file_id may no longer be honored by the
                 API on a follow-up turn (uploads from /v1/files can be
                 deleted, expire or be dropped together with a container).
                 The text content of the historical user message remains
                 intact, and tool_result blocks rebuilt on the assistant
                 side already carry whatever the model needed to derive
                 from those uploads. }
            if TAnthropicContext_IsEphemeralFileBlock(TJSONObject(Item)) then
              Continue;

            Result := Result + [BuildContentBlockFromJson(TJSONObject(Item))];
          end;
      end;
  finally
    Root.Free;
  end;
end;

procedure TAnthropicContext.AppendTurn(
  var AMessages: TMessages;
  const ATurn: TChatTurn);
begin
  {--- One past round-trip = one user message + one assistant message.

       The user side is rebuilt from TChatTurn.JsonPrompt so the original
       content blocks (container_upload, image/file, document, text, ...)
       are echoed back verbatim on replay. When JsonPrompt is empty or
       cannot be parsed (legacy turns), it falls back to plain text from
       TChatTurn.Prompt.

       The assistant side is rebuilt from the streamed JsonResponse when
       possible, and falls back to the aggregated Response text otherwise:
       for example when the JSON buffer is empty or fully redacted.
  }
  var UserContent := BuildHistoricalUserContent(ATurn);
  if Length(UserContent) > 0 then
    AMessages := AMessages.User(UserContent)
  else
    AMessages := AMessages.User(ATurn.Prompt);

  var Content := BuildAssistantContent(ATurn);
  if Length(Content) = 0 then
    AMessages := AMessages.Assistant(ATurn.Response)
  else
    AMessages := AMessages.Assistant(Content);
end;

function TAnthropicContext.HasHistory: Boolean;
begin
  Result := Length(HistoryTurns(CurrentSession)) > 0;
end;

function TAnthropicContext.GetHistory: TArray<TMessageParam>;
begin
  var Messages := Generation.MessageParts;

  for var Turn in HistoryTurns(CurrentSession) do
    AppendTurn(Messages, Turn);

  Result := Messages;
end;

function TAnthropicContext.ExtractContainerIdFromJsonResponse(
  const AJsonResponse: string): string;
begin
  (*--- Walks the persisted stream and returns the last container.id seen
        in any message_start event:

          {"type":"message_start","message":{...,"container":{"id":"cont_..."}}}

        The id is normally identical across all message_start events of a
        single turn, but later events take precedence on principle so any
        late re-provisioning recorded in the buffer wins. An empty result
        means the prior turn did not allocate a container (no skills /
        code-execution was requested or the model returned no container
        block).
  *)
  Result := '';
  if AJsonResponse.Trim.IsEmpty then
    Exit;

  for var Event in AJsonResponse.Split([sLineBreak]) do
    begin
      if Event.Trim.IsEmpty then
        Continue;

      var Reader := TJsonReader.Parse(Event);
      if not Reader.IsValid then
        Continue;

      if not SameText(Reader.AsString('type'), 'message_start') then
        Continue;

      var Id := Reader.AsString('message.container.id');
      if not Id.IsEmpty then
        Result := Id;
    end;
end;

function TAnthropicContext.LastContainerId: string;
begin
  {--- Scans completed turns from the most recent backwards and returns
        the first non-empty container.id encountered. The traversal stops
        as soon as one is found so an older turn never overrides a fresher
        provisioning. Empty when no historical turn ever allocated a
        container, the caller then provisions a fresh one without an
        explicit "id" field, exactly as on the first turn.
  }
  Result := '';

  var Turns := HistoryTurns(CurrentSession);
  for var I := High(Turns) downto Low(Turns) do
    begin
      var Id := ExtractContainerIdFromJsonResponse(Turns[I].JsonResponse);
      if not Id.IsEmpty then
        Exit(Id);
    end;
end;

function TAnthropicContext.BetaExtract: TArray<string>;
const
  BETA_PATH = 'beta';
begin
  {--- Aggregates the unique beta flags requested across every previously
        sent turn of the current session.

        TChatTurn.JsonPrompt holds the formatted JSON of the request that
        was actually sent to the API (assigned by the orchestrator from
        TManagedItemLLMResult.PromptJson on completion), so its top-level
        "beta" array (when present) reflects the betas that the model
        was opted into for that turn:

            "beta": [
              "code-execution-2025-08-25",
              "skills-2025-10-02"
            ]

        Turns whose JsonPrompt is still empty (the in-flight turn before
        its first round-trip completed) are naturally skipped. Order of
        first appearance is preserved in the result; subsequent
        occurrences of an already-seen value are dropped through a small
        seen-set so the final list contains no duplicates. Turns that do
        not carry a "beta" array (the regular case when no feature flag
        is in use) contribute nothing and are silently bypassed by the
        IsArrayNode guard.
  }
  Result := [];

  var Chat := CurrentSession;
  if not Assigned(Chat) then
    Exit;

  var Seen := TDictionary<string, Boolean>.Create;
  try
    for var Turn in Chat.Data do
      begin
        if not Assigned(Turn) then
          Continue;

        if Turn.JsonPrompt.Trim.IsEmpty then
          Continue;

        var Reader := TJsonReader.Parse(Turn.JsonPrompt);
        if not Reader.IsValid then
          Continue;

        if not Reader.IsArrayNode(BETA_PATH) then
          Continue;

        var Total := Reader.Count(BETA_PATH);
        for var I := 0 to Total - 1 do
          begin
            var Beta := Reader.AsString(Format('%s[%d]', [BETA_PATH, I]));
            if Beta.Trim.IsEmpty then
              Continue;

            if Seen.ContainsKey(Beta) then
              Continue;

            Seen.Add(Beta, True);
            Result := Result + [Beta];
          end;
      end;
  finally
    Seen.Free;
  end;
end;

function TAnthropicContext.HasHistoricalCodeExecution: Boolean;
begin
  {--- Detects whether any prior turn emitted a server_tool_use whose name
        belongs to the code_execution family. The Messages API expects the
        matching tool to remain registered on follow-up requests so the
        historical server_tool_use / *_tool_result pair stays valid; without
        re-registration the next call fails with:

          messages.N: <variant>_code_execution tool use with id ... was
          found without a corresponding tool definition.

        The previous implementation relied on the "beta" array stored in
        TChatTurn.JsonPrompt, which is only populated when the demo calls
        Params.Beta(...) explicitly. The new BetaBuilder (hybrid mode) leaves
        beta-header computation to TBetaHeaderManager whenever possible, so
        "beta" can be absent from JsonPrompt. Scanning content blocks
        directly keeps this signal independent from the beta-header
        strategy.
  }
  Result := False;

  var Chat := CurrentSession;
  if not Assigned(Chat) then
    Exit;

  for var Turn in HistoryTurns(Chat) do
    for var Snapshot in TJsonResponseParser.Parse(Turn.JsonResponse) do
      if (Snapshot.Kind = bskServerToolUse) and
         Snapshot.Name.ToLowerInvariant.Contains('code_execution') then
        Exit(True);
end;

function TAnthropicContext.BuildMessages(
  const AState: TStateBuffer;
  const ACurrentContent: TArray<TContentBlockParam>): TArray<TMessageParam>;
begin
  {--- Assembles the full timeline:
         history turns first, current user content blocks last.

       AState is accepted to keep the door open for context-window aware
       trimming (model max tokens, summarization, etc.) without changing the
       call site in TAnthropicServices.
  }
  var Messages := Generation.MessageParts;

  for var Turn in HistoryTurns(CurrentSession) do
    AppendTurn(Messages, Turn);

  Messages := Messages.User(ACurrentContent);

  Result := Messages;
end;

end.
