unit Demo.Anthropic.Session.Events;

interface

uses
  System.SysUtils, System.JSON,
  WVPythia.JSON.SafeReader;

type
  TSessionEventKind = (
    sekUnknown,
    /// <summary>
    /// assistant produced visible text
    /// </summary>
    sekAssistantText,
    /// <summary>
    /// assistant reasoning / thinking
    /// </summary>
    sekReasoning,
    /// <summary>
    /// a server-side tool was invoked
    /// </summary>
    sekToolUse,
    /// <summary>
    /// a tool produced a result
    /// </summary>
    sekToolResult,
    /// <summary>
    /// server requests a CLIENT custom-tool execution
    /// </summary>
    sekCustomToolUse,
    /// <summary>
    /// a tool needs an allow / deny decision
    /// </summary>
    sekToolConfirmationRequest,
    /// <summary>
    /// outcome evaluation event
    /// </summary>
    sekOutcome,
    /// <summary>
    /// sub-agent thread lifecycle / handoff
    /// </summary>
    sekThread,
    /// <summary>
    /// error event
    /// </summary>
    sekError,
    /// <summary>
    /// terminal: the run / turn is complete
    /// </summary>
    sekDone
  );

  TSessionEvent = record
    Kind: TSessionEventKind;
    EventId: string;
    {--- raw wire "type"}
    EventType: string;
    {--- display text (assistant text / reasoning / tool output)}
    Text: string;
    {--- raw content JSON, when content is structured}
    ContentJson: string;
    {--- raw result JSON, when result is structured}
    ResultJson: string;
    {--- raw error JSON, when present}
    ErrorJson: string;
    ToolName: string;
    ToolUseId: string;
    CustomToolUseId: string;
    MCPToolUseId: string;
    {--- raw arguments object for a (custom) tool use}
    InputJson: string;
    MCPServerName: string;
    {--- 'always_allow' / 'always_ask' when present}
    EvaluatedPermission: string;
    SessionThreadId: string;
    FromSessionThreadId: string;
    ToSessionThreadId: string;
    AgentName: string;
    FromAgentName: string;
    ToAgentName: string;
    Status: string;
    IsError: Boolean;
    StopReason: string;
    {--- stop_reason.event_ids for requires_action }
    ActionEventIds: TArray<string>;
    {--- outcome explanation}
    Explanation: string;
    {--- outcome evaluation result}
    OutcomeResult: string;
    {--- the untouched event JSON}
    RawJson: string;
  end;

  TSessionEventParser = record
  private
    class function ClassifyKind(const ASessionEvent: TSessionEvent): TSessionEventKind; static;
    class function ExtractContentText(
      const Reader: TJsonReader;
      const Path: string): string; static;
    class function ExtractDisplayText(const Reader: TJsonReader): string; static;
    class function ExtractJsonOrString(
      const Reader: TJsonReader;
      const Path: string): string; static;
  public
    /// <summary>
    /// Parses one SSE "data:" payload into a typed TSessionEvent. A payload
    /// that is not valid JSON (keep-alive, sentinel) yields a sekUnknown
    /// event the flow can safely ignore.
    /// </summary>
    class function Parse(const DataJson: string): TSessionEvent; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Typed model + parser for the managed-agents session event stream.

  THE BETA-CHURN ISOLATION POINT
  ------------------------------
  The Managed Agents surface is gated by the beta header
  managed-agents-2026-04-01. The exact wire shape of the SSE event stream
  (the precise "type" strings, whether assistant text arrives as token
  deltas or whole messages) is the part most likely to evolve. Everything
  fragile about that lives HERE and nowhere else: Demo.Anthropic.Session.Flow
  consumes only the typed TSessionEvent below.

  Wire shape
  ----------
  Each SSE "data:" payload is a JSON object shaped like the SDK's
  TSessionEvent (Anthropic.Sessions.pas) — same field set returned by
  Sessions.Events.List / .Send. Request-side event types are dotted and
  namespaced ("user.message", "user.interrupt", "user.tool_confirmation",
  "user.custom_tool_result", "user.define_outcome"); server-side types
  follow the same dotted convention.

  Classification strategy
  -----------------------
  Kind is resolved primarily by FIELD PRESENCE (robust) and only secondarily
  by "type" substring (provisional). Example: a non-empty custom_tool_use_id
  unambiguously means "the server wants the client to run a custom tool",
  regardless of the exact type string. This keeps the parser resilient to
  type-string drift; adjust ClassifyKind here if a real stream disagrees.

*)
{$ENDREGION}

{ TSessionEventParser }

class function TSessionEventParser.ExtractJsonOrString(
  const Reader: TJsonReader;
  const Path: string): string;
begin
  Result := '';
  if Path.Trim.IsEmpty then
    Exit;

  if Reader.IsStringNode(Path) then
    Exit(Reader.AsString(Path));

  Result := Reader.ExtractSubJson(Path).Trim;
end;

class function TSessionEventParser.ExtractContentText(
  const Reader: TJsonReader;
  const Path: string): string;

  procedure Append(const Value: string);
  var
    Text: string;
  begin
    Text := Value.Trim;
    if Text.IsEmpty then
      Exit;

    if not Result.IsEmpty then
      Result := Result + sLineBreak;
    Result := Result + Text;
  end;

begin
  Result := '';
  if Path.Trim.IsEmpty then
    Exit;

  if Reader.IsStringNode(Path) then
    Exit(Reader.AsString(Path));

  if Reader.IsArrayNode(Path) then
    begin
      var Total := Reader.Count(Path);
      for var I := 0 to Total - 1 do
        begin
          var ItemText := ExtractContentText(Reader, Format('%s[%d]', [Path, I]));
          Append(ItemText);
        end;
      Exit;
    end;

  if Reader.IsStringNode(Path + '.text') then
    Exit(Reader.AsString(Path + '.text'));

  if Reader.IsStringNode(Path + '.delta.text') then
    Exit(Reader.AsString(Path + '.delta.text'));

  if Reader.IsStringNode(Path + '.content') then
    Exit(Reader.AsString(Path + '.content'));

  if Reader.IsArrayNode(Path + '.content') or
     Reader.IsObjectNode(Path + '.content') then
    begin
      Result := ExtractContentText(Reader, Path + '.content');
      if not Result.Trim.IsEmpty then
        Exit;
    end;

  Append(Reader.AsString(Path + '.stdout'));
  Append(Reader.AsString(Path + '.stderr'));
  Append(Reader.AsString(Path + '.error_message'));
  Append(Reader.AsString(Path + '.message'));

  var Title := Reader.AsString(Path + '.title').Trim;
  var Url := Reader.AsString(Path + '.url').Trim;
  Append(Title);
  if not SameText(Title, Url) then
    Append(Url);

  if Result.Trim.IsEmpty then
    Result := Reader.ExtractSubJson(Path).Trim;
end;

class function TSessionEventParser.ExtractDisplayText(
  const Reader: TJsonReader): string;
begin
  Result := '';

  {--- Streaming-delta and flat-text shapes. }
  if Reader.IsStringNode('text') then
    Exit(Reader.AsString('text'));

  if Reader.IsStringNode('delta.text') then
    Exit(Reader.AsString('delta.text'));

  Result := ExtractContentText(Reader, 'content');
  if not Result.Trim.IsEmpty then
    Exit;

  Result := ExtractContentText(Reader, 'result');
  if not Result.Trim.IsEmpty then
    Exit;

  Result := ExtractContentText(Reader, 'error');
end;

class function TSessionEventParser.ClassifyKind(
  const ASessionEvent: TSessionEvent): TSessionEventKind;
begin
  {--- Field-presence rules first (robust), then "type" substrings
       (provisional). Order matters: the most specific signal wins.
  }
  var LType := ASessionEvent.EventType.ToLowerInvariant;
  var LTypeKey := LType.Replace('.', '_');
  var LStatus := ASessionEvent.Status.Trim.ToLowerInvariant;
  var LStopReason := ASessionEvent.StopReason.Trim.ToLowerInvariant;

  {--- user.* / user_* events are the client's own input (the prompt, tool
       confirmations, custom-tool results, define-outcome) echoed back by the
       session event stream. They must never be shown as assistant content,
       nor re-trigger a custom-tool execution. }
  if LType.StartsWith('user.') or LTypeKey.StartsWith('user_') then
    Exit(sekUnknown);

  if LStopReason.Contains('requires_action') then
    Exit(sekToolConfirmationRequest);

  if LTypeKey.Contains('custom_tool_result') then
    Exit(sekToolResult);

  if (not ASessionEvent.CustomToolUseId.Trim.IsEmpty) or
     LTypeKey.Contains('custom_tool_use') then
    Exit(sekCustomToolUse);

  if LTypeKey.Contains('tool_confirmation') or
     LTypeKey.Contains('requires_action') or
     LTypeKey.Contains('confirmation') then
    Exit(sekToolConfirmationRequest);

  {--- A pending tool_use awaiting an explicit decision. }
  if (not ASessionEvent.ToolUseId.Trim.IsEmpty) and
     SameText(ASessionEvent.EvaluatedPermission.Trim, 'always_ask') then
    Exit(sekToolConfirmationRequest);

  if LTypeKey.Contains('tool_result') then
    Exit(sekToolResult);

  if LTypeKey.Contains('tool_use') or LTypeKey.Contains('tool_call') then
    Exit(sekToolUse);

  if LTypeKey.Contains('thinking') or LTypeKey.Contains('reasoning') then
    Exit(sekReasoning);

  if LTypeKey.Contains('outcome') then
    Exit(sekOutcome);

  if LTypeKey.Contains('thread') or LTypeKey.Contains('handoff') then
    Exit(sekThread);

  if LTypeKey.Contains('error') or
     (not ASessionEvent.Text.Trim.IsEmpty and ASessionEvent.IsError) then
    Exit(sekError);

  {--- The session going idle is the turn's terminal signal. Words like
       completed / terminated / finished / stopped / done also appear on
       intermediate step, model-request and sub-thread events, so they are
       deliberately NOT treated as terminal. }
  if ((LStatus = 'idle') or (LStatus = 'idled')) and
     not LTypeKey.Contains('thread') and
     (LTypeKey.Contains('session') or
      (ASessionEvent.SessionThreadId.Trim.IsEmpty and
       ASessionEvent.FromSessionThreadId.Trim.IsEmpty and
       ASessionEvent.ToSessionThreadId.Trim.IsEmpty)) then
    Exit(sekDone);

  if (LType = 'session.idled') or
     (LType = 'session.idle') or
     (LType = 'session.completed') or
     (LType = 'session.finished') or
     (LType = 'session.stopped') or
     (LType = 'session.status_idled') or
     (LType = 'session.status_idle') or
     (LTypeKey = 'session_idled') or
     (LTypeKey = 'session_idle') or
     (LTypeKey = 'session_completed') or
     (LTypeKey = 'session_finished') or
     (LTypeKey = 'session_stopped') or
     (LTypeKey = 'session_done') or
     (LTypeKey = 'session_status_idle') or
     (LTypeKey = 'session_status_idled') then
    Exit(sekDone);

  if LTypeKey.Contains('text') or
     LTypeKey.Contains('message') or
     LTypeKey.Contains('content') then
    Exit(sekAssistantText);

  Result := sekUnknown;
end;

class function TSessionEventParser.Parse(const DataJson: string): TSessionEvent;
begin
  Result := Default(TSessionEvent);
  Result.Kind := sekUnknown;
  Result.RawJson := DataJson;

  if DataJson.Trim.IsEmpty then
    Exit;

  var SessionEvent := TJsonReader.Parse(DataJson);
  if not SessionEvent.IsValid then
    Exit;

  Result.EventId := SessionEvent.AsString('id');
  Result.EventType := SessionEvent.AsString('type');
  Result.ContentJson := ExtractJsonOrString(SessionEvent, 'content');
  Result.ResultJson := ExtractJsonOrString(SessionEvent, 'result');
  Result.ErrorJson := ExtractJsonOrString(SessionEvent, 'error');
  Result.ToolName := SessionEvent.AsString('name');
  Result.ToolUseId := SessionEvent.AsString('tool_use_id');
  Result.CustomToolUseId := SessionEvent.AsString('custom_tool_use_id');
  Result.MCPToolUseId := SessionEvent.AsString('mcp_tool_use_id');
  Result.MCPServerName := SessionEvent.AsString('mcp_server_name');
  Result.EvaluatedPermission := SessionEvent.AsString('evaluated_permission');
  if Result.EvaluatedPermission.Trim.IsEmpty then
    Result.EvaluatedPermission := SessionEvent.AsString('permission');
  if Result.EvaluatedPermission.Trim.IsEmpty then
    Result.EvaluatedPermission := SessionEvent.AsString('permission.type');
  if Result.EvaluatedPermission.Trim.IsEmpty then
    Result.EvaluatedPermission := SessionEvent.AsString('permission_policy');
  if Result.EvaluatedPermission.Trim.IsEmpty then
    Result.EvaluatedPermission := SessionEvent.AsString('permission_policy.type');
  Result.SessionThreadId := SessionEvent.AsString('session_thread_id');
  Result.FromSessionThreadId := SessionEvent.AsString('from_session_thread_id');
  Result.ToSessionThreadId := SessionEvent.AsString('to_session_thread_id');
  Result.AgentName := SessionEvent.AsString('agent_name');
  Result.FromAgentName := SessionEvent.AsString('from_agent_name');
  Result.ToAgentName := SessionEvent.AsString('to_agent_name');
  Result.Status := SessionEvent.AsString('status');
  Result.IsError := SessionEvent.AsBoolean('is_error', False);
  Result.StopReason := SessionEvent.AsString('stop_reason');
  if Result.StopReason.Trim.IsEmpty then
    Result.StopReason := SessionEvent.AsString('stop_reason.type');
  if Result.StopReason.Trim.IsEmpty then
    Result.StopReason := ExtractJsonOrString(SessionEvent, 'stop_reason');
  var ActionCount := SessionEvent.Count('stop_reason.event_ids');
  for var I := 0 to ActionCount - 1 do
    Result.ActionEventIds := Result.ActionEventIds +
      [SessionEvent.AsString(Format('stop_reason.event_ids[%d]', [I]))];
  Result.Explanation := SessionEvent.AsString('explanation');
  Result.OutcomeResult := SessionEvent.AsString('result');
  if Result.OutcomeResult.Trim.IsEmpty then
    Result.OutcomeResult := Result.ResultJson;
  Result.InputJson := SessionEvent.ExtractSubJson('input');
  Result.Text := ExtractDisplayText(SessionEvent);

  {--- Current managed-agent streams identify agent.custom_tool_use events by
       the event id itself. Older examples exposed a custom_tool_use_id field.
       Reply events still require custom_tool_use_id, so preserve both shapes. }
  if Result.CustomToolUseId.Trim.IsEmpty and
     Result.EventType.ToLowerInvariant.Replace('.', '_').Contains('custom_tool_use') then
    Result.CustomToolUseId := Result.EventId;

  if Result.ToolUseId.Trim.IsEmpty and
     Result.EventType.ToLowerInvariant.Replace('.', '_').Contains('tool_use') and
     not Result.EventType.ToLowerInvariant.Replace('.', '_').Contains('custom_tool_use') then
    Result.ToolUseId := Result.EventId;

  Result.Kind := ClassifyKind(Result);
end;

end.
