unit Anthropic.Headers.Beta;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Generics.Collections, System.JSON, System.Net.URLClient;

type
  TBetaHeaderManager = record
  public
    /// <summary>
    /// Builds the HTTP headers required to enable Anthropic beta features for a given request.
    /// </summary>
    /// <param name="Path">
    /// Request path or endpoint (for example: <c>/v1/messages</c> or <c>v1/files</c>).
    /// The value is normalized (query string removed, leading/trailing slashes trimmed, and an optional <c>v1/</c> prefix ignored)
    /// to apply endpoint-specific beta rules.
    /// </param>
    /// <param name="PayloadJson">
    /// Request JSON payload as a string. The payload is inspected to detect beta-dependent features such as
    /// tool types in <c>tools</c> and content items in <c>messages</c>. If empty or whitespace, no payload-based betas are inferred.
    /// </param>
    /// <returns>
    /// A <see cref="TNetHeaders"/> array containing zero or one <c>anthropic-beta</c> header.
    /// When multiple betas are required, their tokens are combined into a single header value separated by commas
    /// (for example: <c>code-execution-2025-08-25,files-api-2025-04-14</c>).
    /// </returns>
    /// <remarks>
    /// This method is stateless and evaluates only the current request.
    /// It may raise <see cref="EAnthropicBetaHeaderError"/> when the payload contains an invalid or incompatible
    /// beta configuration (for example, unsupported combinations or disallowed deferred tool settings),
    /// and may raise <see cref="Exception"/> if <paramref name="PayloadJson"/> is not valid JSON.
    /// </remarks>
    class function Build(const Path, PayloadJson: string): TNetHeaders; static;
  end;

implementation

{$REGION 'Dev note'}
(*
   UPDATE METHOD
   -------------

   1. Update beta tokens (global search for dated strings like *-20-..-..) and fix all occurrences.

   2. Update BuildToolTypeToBetaMap for any new or renamed tool type -> beta token.
      Keep GA tool types out of this map; only map features that still require
      an anthropic-beta token.

   3. Check hard-coded tool type detections (RootHasAnyToolSearchTool, RootHasAnyMcpToolset,
      RootHasManualThinkingWithTools, RootHasContainerSkills).

   4. Check detections based on messages.content[].type (container_upload, tool_search_tool_result,
      web_fetch_tool_result, etc.).

   5. Check endpoint-level beta gates (files, skills, message batches, and managed agents:
      agents/environments/sessions/vaults/memory_stores).

   6. Check payload-level beta gates such as speed=fast, task budgets, compaction,
      context editing, Agent Skills, and server tool type mappings
      (advisor, computer use, MCP, etc.).

   7. Revalidate all guards that raise errors (ValidateTextEditorMaxCharacters,
      ValidateToolSearchCompatibility, ValidateToolSearchNotDeferred, ValidateAllToolsDeferred).
*)
{$ENDREGION}

type
  EAnthropicBetaHeaderError = class(Exception);

type
  TOrderedSet = class
  private
    FSeen: TDictionary<string, Byte>;
    FList: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const S: string);
    function Count: Integer;
    function Join(const Sep: string): string;
  end;

constructor TOrderedSet.Create;
begin
  inherited Create;
  FSeen := TDictionary<string, Byte>.Create;
  FList := TList<string>.Create;
end;

destructor TOrderedSet.Destroy;
begin
  FList.Free;
  FSeen.Free;
  inherited Destroy;
end;

procedure TOrderedSet.Add(const S: string);
begin
  var K := Trim(S);
  if K.IsEmpty then
    Exit;

  if not FSeen.ContainsKey(K) then
    begin
      FSeen.Add(K, 0);
      FList.Add(K);
    end;
end;

function TOrderedSet.Count: Integer;
begin
  Result := FList.Count;
end;

function TOrderedSet.Join(const Sep: string): string;
begin
  Result := string.Join(Sep, FList.ToArray);
end;

type
  TBetaHeaderOps = record
  private
    class function StripUtf8Bom(const S: string): string; static;

    class function NormalizePath(const Path: string): string; static;
    class function IsFilesEndpoint(const NormPath: string): Boolean; static;
    class function IsMessageBatchesEndpoint(const NormPath: string): Boolean; static;
    class function IsSkillsEndpoint(const NormPath: string): Boolean; static;
    class function IsAgentsEndpoint(const NormPath: string): Boolean; static;
    class function IsEnvironmentsEndpoint(const NormPath: string): Boolean; static;
    class function IsManagedAgentsEndpoint(const NormPath: string): Boolean; static;

    class function TryGetJSONArray(const Obj: TJSONObject; const Name: string; out Arr: TJSONArray): Boolean; static;
    class function TryGetJSONObject(const Obj: TJSONObject; const Name: string; out Child: TJSONObject): Boolean; static;
    class function TryGetString(const Obj: TJSONObject; const Name: string; out S: string): Boolean; static;
    class function TryGetBool(const Obj: TJSONObject; const Name: string; out B: Boolean): Boolean; static;

    class function StartsWithI(const S, Prefix: string): Boolean; static;
    class function IsTextEditorToolType(const ToolType: string): Boolean; static;
    class procedure ValidateTextEditorMaxCharacters(const ToolObj: TJSONObject); static;

    class function IsKnownBetaToken(const S: string): Boolean; static;
    class function BuildToolTypeToBetaMap: TDictionary<string, string>; static;

    class procedure AddBetasFromSingleToolObject(const ToolObj: TJSONObject; const Betas: TOrderedSet; const Map: TDictionary<string,string>); static;

    class function RootHasTools(const Root: TJSONValue): Boolean; static;
    class function RootHasField(const Root: TJSONValue; const FieldName: string): Boolean; static;

    class function RootHasContainerSkills(const Root: TJSONValue): Boolean; static;
    class function RootHasMcpServers(const Root: TJSONValue): Boolean; static;
    class function RootHasFastMode(const Root: TJSONValue): Boolean; static;

    class function JsonContainsToolReference(const V: TJSONValue): Boolean; static;
    class function RootHasToolSearchServerToolUseInMessages(const Root: TJSONValue): Boolean; static;
    class function RootHasToolSearchSignalsInMessages(const Root: TJSONValue): Boolean; static;

    class function RootHasAnyToolSearchTool(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyToolWithInputExamples(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyToolWithEagerInputStreaming(const Root: TJSONValue): Boolean; static;
    class function RootHasManualThinkingWithTools(const Root: TJSONValue): Boolean; static;
    class function RootHasTaskBudget(const Root: TJSONValue): Boolean; static;
    class function ContextManagementEditsContainType(const Root: TJSONValue; const EditType: string): Boolean; static;
    class function RootHasCompactionEdit(const Root: TJSONValue): Boolean; static;
    class function RootHasContextEditingEdit(const Root: TJSONValue): Boolean; static;

    class procedure ValidateToolSearchCompatibility(const Root: TJSONValue); static;
    class procedure ValidateToolSearchNotDeferred(const Root: TJSONValue); static;
    class procedure ValidateAllToolsDeferred(const Root: TJSONValue); static;

    class function RootHasAnyMcpToolset(const Root: TJSONValue): Boolean; static;
    class function RootHasMcpSignalsInMessages(const Root: TJSONValue): Boolean; static;

    class procedure AddEndpointBetas(const NormPath: string; const Betas: TOrderedSet); static;
    class procedure AddBetasFromToolsAnyRoot(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddBetasFromOutputFormat(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddBetasFromContainerSkills(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddBetasFromMessagesContainerUpload(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddMcpBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddContextManagementBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddFastModeBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddTaskBudgetBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddFineGrainedToolStreamingBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddInterleavedThinkingBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
  public
    class function BuildHeaders(const Path, PayloadJson: string): TNetHeaders; static;
  end;

{ TBetaHeaderOps }

class function TBetaHeaderOps.StripUtf8Bom(const S: string): string;
begin
  Result := S;
  if not Result.IsEmpty and (Result[Low(Result)] = #$FEFF) then
    Result := Result.Substring(1);
end;

class function TBetaHeaderOps.NormalizePath(const Path: string): string;
begin
  var PathValue := Path.Trim;

  var QuerySeparator := PathValue.IndexOf('?');
  if QuerySeparator >= 0 then
    PathValue := PathValue.Substring(0, QuerySeparator);

  while not PathValue.IsEmpty and (PathValue[Low(PathValue)] = '/') do
    PathValue := PathValue.Substring(1);

  while not PathValue.IsEmpty and (PathValue[PathValue.Length] = '/') do
    PathValue := PathValue.Substring(0, PathValue.Length - 1);

  if PathValue.StartsWith('v1/', True) then
    PathValue := PathValue.Substring(3);

  Result := PathValue.ToLowerInvariant;
end;

class function TBetaHeaderOps.IsFilesEndpoint(const NormPath: string): Boolean;
begin
  Result := (NormPath = 'files') or NormPath.StartsWith('files/', True);
end;

class function TBetaHeaderOps.TryGetJSONArray(const Obj: TJSONObject; const Name: string; out Arr: TJSONArray): Boolean;
begin
  Arr := nil;
  Result := False;
  if Obj = nil then Exit;

  var V := Obj.GetValue(Name);
  if (V <> nil) and (V is TJSONArray) then
    begin
      Arr := TJSONArray(V);
      Result := True;
    end;
end;

class function TBetaHeaderOps.TryGetJSONObject(const Obj: TJSONObject; const Name: string; out Child: TJSONObject): Boolean;
begin
  Child := nil;
  Result := False;
  if Obj = nil then Exit;

  var V := Obj.GetValue(Name);
  if (V <> nil) and (V is TJSONObject) then
    begin
      Child := TJSONObject(V);
      Result := True;
    end;
end;

class function TBetaHeaderOps.TryGetString(const Obj: TJSONObject; const Name: string; out S: string): Boolean;
begin
  S := EmptyStr;
  Result := False;
  if Obj = nil then Exit;

  var V := Obj.GetValue(Name);
  if (V <> nil) and (V is TJSONString) then
    begin
      S := TJSONString(V).Value;
      Result := True;
    end;
end;

class function TBetaHeaderOps.TryGetBool(const Obj: TJSONObject; const Name: string; out B: Boolean): Boolean;
begin
  B := False;
  Result := False;
  if Obj = nil then Exit;

  var V := Obj.GetValue(Name);
  if (V <> nil) and (V is TJSONBool) then
    begin
      B := TJSONBool(V).AsBoolean;
      Result := True;
    end;
end;

class function TBetaHeaderOps.StartsWithI(const S, Prefix: string): Boolean;
begin
  Result := S.StartsWith(Prefix, True);
end;

class function TBetaHeaderOps.IsTextEditorToolType(const ToolType: string): Boolean;
begin
  Result := StartsWithI(ToolType, 'text_editor_');
end;

class procedure TBetaHeaderOps.ValidateTextEditorMaxCharacters(const ToolObj: TJSONObject);
var
  ToolType: string;
begin
  if ToolObj = nil then Exit;

  var MaxCharsVal := ToolObj.GetValue('max_characters');
  if MaxCharsVal = nil then Exit;

  if not TryGetString(ToolObj, 'type', ToolType) then Exit;

  if SameText(ToolType, 'text_editor_20250728') or SameText(ToolType, 'text_editor_20250429') then
    Exit;

  if IsTextEditorToolType(ToolType) then
    raise EAnthropicBetaHeaderError.CreateFmt(
      'Invalid text editor tool configuration: max_characters is not compatible with tool type "%s".',
      [ToolType]
    );
end;

class function TBetaHeaderOps.IsKnownBetaToken(const S: string): Boolean;
begin
  Result :=
    SameText(S, 'message-batches-2024-09-24') or
    SameText(S, 'prompt-caching-2024-07-31') or
    SameText(S, 'computer-use-2024-10-22') or
    SameText(S, 'computer-use-2025-01-24') or
    SameText(S, 'computer-use-2025-11-24') or
    SameText(S, 'pdfs-2024-09-25') or
    SameText(S, 'token-counting-2024-11-01') or
    SameText(S, 'token-efficient-tools-2025-02-19') or
    SameText(S, 'output-128k-2025-02-19') or
    SameText(S, 'files-api-2025-04-14') or
    SameText(S, 'mcp-client-2025-04-04') or
    SameText(S, 'mcp-client-2025-11-20') or
    SameText(S, 'dev-full-thinking-2025-05-14') or
    SameText(S, 'interleaved-thinking-2025-05-14') or
    SameText(S, 'fine-grained-tool-streaming-2025-05-14') or
    SameText(S, 'code-execution-2025-05-22') or
    SameText(S, 'code-execution-2025-08-25') or
    SameText(S, 'extended-cache-ttl-2025-04-11') or
    SameText(S, 'context-1m-2025-08-07') or
    SameText(S, 'context-management-2025-06-27') or
    SameText(S, 'model-context-window-exceeded-2025-08-26') or
    SameText(S, 'skills-2025-10-02') or
    SameText(S, 'fast-mode-2026-02-01') or
    SameText(S, 'structured-outputs-2025-11-13') or
    SameText(S, 'compact-2026-01-12') or
    SameText(S, 'output-300k-2026-03-24') or
    SameText(S, 'user-profiles-2026-03-24') or
    SameText(S, 'advisor-tool-2026-03-01') or
    SameText(S, 'managed-agents-2026-04-01') or
    SameText(S, 'task-budgets-2026-03-13');
end;

class function TBetaHeaderOps.IsMessageBatchesEndpoint(
  const NormPath: string): Boolean;
begin
  Result := (NormPath = 'messages/batches') or NormPath.StartsWith('messages/batches/', True);
end;

class function TBetaHeaderOps.IsSkillsEndpoint(const NormPath: string): Boolean;
begin
  Result := (NormPath = 'skills') or NormPath.StartsWith('skills/', True);
end;

class function TBetaHeaderOps.IsAgentsEndpoint(const NormPath: string): Boolean;
begin
  Result := (NormPath = 'agents') or NormPath.StartsWith('agents/', True);
end;

class function TBetaHeaderOps.IsEnvironmentsEndpoint(const NormPath: string): Boolean;
begin
  Result := (NormPath = 'environments') or NormPath.StartsWith('environments/', True);
end;

class function TBetaHeaderOps.IsManagedAgentsEndpoint(const NormPath: string): Boolean;
begin
  Result :=
    IsAgentsEndpoint(NormPath) or
    IsEnvironmentsEndpoint(NormPath) or
    (NormPath = 'sessions') or NormPath.StartsWith('sessions/', True) or
    (NormPath = 'vaults') or NormPath.StartsWith('vaults/', True) or
    (NormPath = 'memory_stores') or NormPath.StartsWith('memory_stores/', True) or
    (NormPath = 'memory-stores') or NormPath.StartsWith('memory-stores/', True);
end;

class function TBetaHeaderOps.BuildToolTypeToBetaMap: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;

  Result.Add('code_execution_20250522',         'code-execution-2025-05-22');
  { code_execution_20250825 and code_execution_20260120 are GA as tools.
    Agent Skills still require code-execution-2025-08-25 and are handled from
    container.skills, not from the generic tool type. }

  Result.Add('files_api_20250414',              'files-api-2025-04-14');

  Result.Add('computer_20251124',               'computer-use-2025-11-24');
  Result.Add('computer_20250124',               'computer-use-2025-01-24');
  Result.Add('computer_20241022',               'computer-use-2024-10-22');

  Result.Add('mcp_toolset',                     'mcp-client-2025-11-20');
  Result.Add('advisor_20260301',                'advisor-tool-2026-03-01');
end;

class procedure TBetaHeaderOps.AddBetasFromSingleToolObject(
  const ToolObj: TJSONObject; const Betas: TOrderedSet; const Map: TDictionary<string,string>);
var
  ToolType, Beta: string;
begin
  if ToolObj = nil then Exit;

  ValidateTextEditorMaxCharacters(ToolObj);

  if not TryGetString(ToolObj, 'type', ToolType) then Exit;

  if Map.TryGetValue(ToolType, Beta) then
    begin
      Betas.Add(Beta);
      Exit;
    end;

  if StartsWithI(ToolType, 'web_search_') then Exit;
  if StartsWithI(ToolType, 'web_fetch_') then Exit;
  if StartsWithI(ToolType, 'code_execution_') then Exit;
  if StartsWithI(ToolType, 'tool_search_tool_') then Exit;
  if StartsWithI(ToolType, 'memory_') then Exit;
  if IsTextEditorToolType(ToolType) then Exit;
  if StartsWithI(ToolType, 'bash_') then Exit;

  if IsKnownBetaToken(ToolType) then
    Betas.Add(ToolType);
end;

class function TBetaHeaderOps.RootHasTools(const Root: TJSONValue): Boolean;
var
  ToolsArr: TJSONArray;
begin
  Result := False;
  if Root is TJSONObject then
    Exit(TryGetJSONArray(TJSONObject(Root), 'tools', ToolsArr));

  if Root is TJSONArray then
    Exit((TJSONArray(Root).Count > 0) and (TJSONArray(Root).Items[0] is TJSONObject));
end;

class function TBetaHeaderOps.RootHasField(const Root: TJSONValue; const FieldName: string): Boolean;
begin
  Result := (Root is TJSONObject) and (TJSONObject(Root).GetValue(FieldName) <> nil);
end;

class function TBetaHeaderOps.RootHasContainerSkills(const Root: TJSONValue): Boolean;
var
  ContainerObj: TJSONObject;
  SkillsArr: TJSONArray;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var ContainerVal := TJSONObject(Root).GetValue('container');
  if (ContainerVal = nil) or not (ContainerVal is TJSONObject) then Exit;

  ContainerObj := TJSONObject(ContainerVal);
  Result := TryGetJSONArray(ContainerObj, 'skills', SkillsArr) and (SkillsArr.Count > 0);
end;

class function TBetaHeaderOps.RootHasMcpServers(const Root: TJSONValue): Boolean;
begin
  Result := RootHasField(Root, 'mcp_servers');
end;

class function TBetaHeaderOps.RootHasFastMode(const Root: TJSONValue): Boolean;
var
  ModelObj: TJSONObject;
  Speed: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);

  { Messages API: speed is a top-level body parameter. }
  if TryGetString(RootObj, 'speed', Speed) and SameText(Speed, 'fast') then
    Exit(True);

  { Managed Agents API: model may be either a string or an object containing speed. }
  if TryGetJSONObject(RootObj, 'model', ModelObj) then
    if TryGetString(ModelObj, 'speed', Speed) and SameText(Speed, 'fast') then
      Exit(True);
end;

class function TBetaHeaderOps.JsonContainsToolReference(const V: TJSONValue): Boolean;
var
  TStr: string;
begin
  Result := False;
  if V = nil then Exit;

  if V is TJSONObject then
    begin
      var Obj := TJSONObject(V);
      if TryGetString(Obj, 'type', TStr) and SameText(TStr, 'tool_reference') then
        Exit(True);

      for var I := 0 to Obj.Count - 1 do
        if JsonContainsToolReference(Obj.Pairs[I].JsonValue) then
          Exit(True);

      Exit(False);
    end;

  if V is TJSONArray then
    begin
      var Arr := TJSONArray(V);
      for var I := 0 to Arr.Count - 1 do
        if JsonContainsToolReference(Arr.Items[I]) then
          Exit(True);

      Exit(False);
    end;
end;

class function TBetaHeaderOps.RootHasToolSearchServerToolUseInMessages(const Root: TJSONValue): Boolean;
var
  Messages: TJSONArray;
  CType, NameS: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'messages', Messages) then Exit;

  for var MsgVal in Messages do
    begin
      if not (MsgVal is TJSONObject) then Continue;
      var MsgObj := TJSONObject(MsgVal);

      var ContentVal := MsgObj.GetValue('content');
      if (ContentVal = nil) or not (ContentVal is TJSONArray) then Continue;

      var ContentArr := TJSONArray(ContentVal);
      for var ItemVal in ContentArr do
        begin
          if not (ItemVal is TJSONObject) then Continue;
          var ItemObj := TJSONObject(ItemVal);

          if not TryGetString(ItemObj, 'type', CType) then Continue;

          if SameText(CType, 'server_tool_use') then
            begin
              if TryGetString(ItemObj, 'name', NameS) then
                if SameText(NameS, 'tool_search_tool_regex') or SameText(NameS, 'tool_search_tool_bm25') then
                  Exit(True);
            end;
        end;
  end;
end;

class function TBetaHeaderOps.RootHasToolSearchSignalsInMessages(const Root: TJSONValue): Boolean;
var
  Messages: TJSONArray;
  CType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  if RootHasToolSearchServerToolUseInMessages(Root) then
    Exit(True);

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'messages', Messages) then Exit;

  for var MsgVal in Messages do
    begin
      if not (MsgVal is TJSONObject) then Continue;
      var MsgObj := TJSONObject(MsgVal);

      var ContentVal := MsgObj.GetValue('content');
      if (ContentVal <> nil) and (ContentVal is TJSONArray) then
        begin
          var ContentArr := TJSONArray(ContentVal);
          for var ItemVal in ContentArr do
            begin
              if not (ItemVal is TJSONObject) then Continue;
              var ItemObj := TJSONObject(ItemVal);

              if not TryGetString(ItemObj, 'type', CType) then Continue;

              if SameText(CType, 'tool_search_tool_result') then
                Exit(True);

              if SameText(CType, 'tool_result') then
                begin
                  var ToolResultContent := ItemObj.GetValue('content');
                  if JsonContainsToolReference(ToolResultContent) then
                    Exit(True);
                end;
            end;
        end;
    end;
end;

class function TBetaHeaderOps.RootHasAnyToolSearchTool(const Root: TJSONValue): Boolean;
var
  ToolsArr: TJSONArray;
  ToolType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'tools', ToolsArr) then Exit;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;
      var ToolObj := TJSONObject(ToolVal);

      if TryGetString(ToolObj, 'type', ToolType) then
        if SameText(ToolType, 'tool_search_tool_regex_20251119') or SameText(ToolType, 'tool_search_tool_bm25_20251119') then
          Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasAnyToolWithInputExamples(const Root: TJSONValue): Boolean;
var
  ToolsArr: TJSONArray;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'tools', ToolsArr) then Exit;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;
      var ToolObj := TJSONObject(ToolVal);

      if ToolObj.GetValue('input_examples') <> nil then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasAnyToolWithEagerInputStreaming(const Root: TJSONValue): Boolean;
var
  ToolsArr: TJSONArray;
  Enabled: Boolean;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'tools', ToolsArr) then Exit;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;
      var ToolObj := TJSONObject(ToolVal);

      if TryGetBool(ToolObj, 'eager_input_streaming', Enabled) and Enabled then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasManualThinkingWithTools(const Root: TJSONValue): Boolean;
var
  ThinkingObj: TJSONObject;
  ThinkingType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;
  if not RootHasTools(Root) then Exit;

  if not TryGetJSONObject(TJSONObject(Root), 'thinking', ThinkingObj) then Exit;

  Result := TryGetString(ThinkingObj, 'type', ThinkingType) and
    SameText(ThinkingType, 'enabled');
end;

class function TBetaHeaderOps.RootHasTaskBudget(const Root: TJSONValue): Boolean;
var
  OutputConfig: TJSONObject;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  if TryGetJSONObject(TJSONObject(Root), 'output_config', OutputConfig) then
    Result := OutputConfig.GetValue('task_budget') <> nil;
end;

class function TBetaHeaderOps.ContextManagementEditsContainType(
  const Root: TJSONValue; const EditType: string): Boolean;
var
  ContextManagement: TJSONObject;
  Edits: TJSONArray;
  CurrentType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  if not TryGetJSONObject(TJSONObject(Root), 'context_management', ContextManagement) then Exit;
  if not TryGetJSONArray(ContextManagement, 'edits', Edits) then Exit;

  for var EditVal in Edits do
    begin
      if not (EditVal is TJSONObject) then Continue;
      if TryGetString(TJSONObject(EditVal), 'type', CurrentType) and SameText(CurrentType, EditType) then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasCompactionEdit(const Root: TJSONValue): Boolean;
begin
  Result := ContextManagementEditsContainType(Root, 'compact_20260112');
end;

class function TBetaHeaderOps.RootHasContextEditingEdit(const Root: TJSONValue): Boolean;
begin
  Result :=
    ContextManagementEditsContainType(Root, 'clear_tool_uses_20250919') or
    ContextManagementEditsContainType(Root, 'clear_thinking_20251015');
end;

class procedure TBetaHeaderOps.ValidateToolSearchCompatibility(const Root: TJSONValue);
begin
  if (RootHasAnyToolSearchTool(Root) or RootHasToolSearchSignalsInMessages(Root)) and RootHasAnyToolWithInputExamples(Root) then
    raise EAnthropicBetaHeaderError.Create(
      'Invalid tools configuration: tool search is not compatible with tool use examples (input_examples).'
    );
end;

class procedure TBetaHeaderOps.ValidateToolSearchNotDeferred(const Root: TJSONValue);
var
  ToolsArr: TJSONArray;
  ToolType: string;
  Deferred: Boolean;
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'tools', ToolsArr) then Exit;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;

      var ToolObj := TJSONObject(ToolVal);

      if not TryGetString(ToolObj, 'type', ToolType) then Continue;

      if SameText(ToolType, 'tool_search_tool_regex_20251119') or SameText(ToolType, 'tool_search_tool_bm25_20251119') then
        if TryGetBool(ToolObj, 'defer_loading', Deferred) and Deferred then
          raise EAnthropicBetaHeaderError.Create(
            'Invalid tools configuration: tool_search_tool_* must not set defer_loading=true.'
          );
    end;
end;

class procedure TBetaHeaderOps.ValidateAllToolsDeferred(const Root: TJSONValue);
var
  ToolsArr: TJSONArray;
  Deferred: Boolean;
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  var HasTools := TryGetJSONArray(RootObj, 'tools', ToolsArr);
  if not HasTools then Exit;
  if ToolsArr.Count = 0 then Exit;

  var HasNonDeferred := False;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;
      var ToolObj := TJSONObject(ToolVal);

      if not TryGetBool(ToolObj, 'defer_loading', Deferred) then
        begin
          HasNonDeferred := True;
          Break;
        end;

      if not Deferred then
        begin
          HasNonDeferred := True;
          Break;
        end;
    end;

  if not HasNonDeferred then
    raise EAnthropicBetaHeaderError.Create(
      'Invalid tools configuration: all tools have defer_loading=true. At least one tool must be non-deferred.'
    );
end;

class function TBetaHeaderOps.RootHasAnyMcpToolset(const Root: TJSONValue): Boolean;
var
  ToolsArr: TJSONArray;
  ToolType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'tools', ToolsArr) then Exit;

  for var ToolVal in ToolsArr do
    begin
      if not (ToolVal is TJSONObject) then Continue;
      var ToolObj := TJSONObject(ToolVal);

      if TryGetString(ToolObj, 'type', ToolType) and SameText(ToolType, 'mcp_toolset') then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasMcpSignalsInMessages(const Root: TJSONValue): Boolean;
var
  Messages: TJSONArray;
  CType: string;
begin
  Result := False;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'messages', Messages) then Exit;

  for var MsgVal in Messages do
    begin
      if not (MsgVal is TJSONObject) then Continue;
      var MsgObj := TJSONObject(MsgVal);

      var ContentVal := MsgObj.GetValue('content');
      if (ContentVal = nil) or not (ContentVal is TJSONArray) then Continue;

      var ContentArr := TJSONArray(ContentVal);
      for var ItemVal in ContentArr do
        begin
          if not (ItemVal is TJSONObject) then Continue;
          var ItemObj := TJSONObject(ItemVal);

          if TryGetString(ItemObj, 'type', CType) then
            if SameText(CType, 'mcp_tool_use') or SameText(CType, 'mcp_tool_result') then
              Exit(True);
        end;
    end;
end;

class procedure TBetaHeaderOps.AddEndpointBetas(const NormPath: string; const Betas: TOrderedSet);
begin
  if IsMessageBatchesEndpoint(NormPath) then
    Betas.Add('message-batches-2024-09-24');

  if IsSkillsEndpoint(NormPath) then
    Betas.Add('skills-2025-10-02');

  if IsManagedAgentsEndpoint(NormPath) then
    Betas.Add('managed-agents-2026-04-01');
end;

class procedure TBetaHeaderOps.AddBetasFromToolsAnyRoot(const Root: TJSONValue; const Betas: TOrderedSet);
var
  ToolsArr: TJSONArray;
begin
  var Map := BuildToolTypeToBetaMap;
  try
    if (Root is TJSONObject) and TryGetJSONArray(TJSONObject(Root), 'tools', ToolsArr) then
      begin
        for var ToolVal in ToolsArr do
          if ToolVal is TJSONObject then
            AddBetasFromSingleToolObject(TJSONObject(ToolVal), Betas, Map);
        Exit;
      end;

    if Root is TJSONArray then
      begin
        ToolsArr := TJSONArray(Root);
        for var ToolVal in ToolsArr do
          if ToolVal is TJSONObject then
            AddBetasFromSingleToolObject(TJSONObject(ToolVal), Betas, Map);
        Exit;
      end;

    if Root is TJSONObject then
      if (TJSONObject(Root).GetValue('input_schema') <> nil) or (TJSONObject(Root).GetValue('name') <> nil) then
        AddBetasFromSingleToolObject(TJSONObject(Root), Betas, Map);
  finally
    Map.Free;
  end;
end;

class procedure TBetaHeaderOps.AddBetasFromOutputFormat(const Root: TJSONValue; const Betas: TOrderedSet);
var
  OutputFormat: TJSONObject;
  OutputType: string;
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONObject(RootObj, 'output_format', OutputFormat) then Exit;

  { Legacy structured outputs shape. The current API uses output_config.format
    and does not require a beta header. }
  if TryGetString(OutputFormat, 'type', OutputType) and SameText(OutputType, 'json_schema') then
    Betas.Add('structured-outputs-2025-11-13');
end;

class procedure TBetaHeaderOps.AddBetasFromContainerSkills(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if not RootHasContainerSkills(Root) then Exit;

  Betas.Add('code-execution-2025-08-25');
  Betas.Add('skills-2025-10-02');
  Betas.Add('files-api-2025-04-14');
end;

class procedure TBetaHeaderOps.AddBetasFromMessagesContainerUpload(const Root: TJSONValue; const Betas: TOrderedSet);
var
  Messages: TJSONArray;
  ContentType: string;
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetJSONArray(RootObj, 'messages', Messages) then Exit;

  for var MsgVal in Messages do
    begin
      if not (MsgVal is TJSONObject) then Continue;
      var MsgObj := TJSONObject(MsgVal);

      var ContentVal := MsgObj.GetValue('content');
      if ContentVal = nil then Continue;

      if ContentVal is TJSONArray then
        begin
          var ContentArr := TJSONArray(ContentVal);
          for var ItemVal in ContentArr do
            begin
              if not (ItemVal is TJSONObject) then Continue;
              var ItemObj := TJSONObject(ItemVal);

              if TryGetString(ItemObj, 'type', ContentType) and SameText(ContentType, 'container_upload') then
                begin
                  Betas.Add('files-api-2025-04-14');
                  Exit;
                end;
            end;
        end;
    end;
end;

class procedure TBetaHeaderOps.AddMcpBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasMcpServers(Root) or RootHasAnyMcpToolset(Root) or RootHasMcpSignalsInMessages(Root) then
    Betas.Add('mcp-client-2025-11-20');
end;

class procedure TBetaHeaderOps.AddContextManagementBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  var CtxMgmtVal := RootObj.GetValue('context_management');
  if (CtxMgmtVal = nil) or not (CtxMgmtVal is TJSONObject) then Exit;

  if RootHasCompactionEdit(Root) then
    Betas.Add('compact-2026-01-12');

  if RootHasContextEditingEdit(Root) then
    Betas.Add('context-management-2025-06-27');
end;

class procedure TBetaHeaderOps.AddFastModeBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasFastMode(Root) then
    Betas.Add('fast-mode-2026-02-01');
end;

class procedure TBetaHeaderOps.AddTaskBudgetBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasTaskBudget(Root) then
    Betas.Add('task-budgets-2026-03-13');
end;

class procedure TBetaHeaderOps.AddFineGrainedToolStreamingBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasAnyToolWithEagerInputStreaming(Root) then
    Betas.Add('fine-grained-tool-streaming-2025-05-14');
end;

class procedure TBetaHeaderOps.AddInterleavedThinkingBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasManualThinkingWithTools(Root) then
    Betas.Add('interleaved-thinking-2025-05-14');
end;

class function TBetaHeaderOps.BuildHeaders(const Path, PayloadJson: string): TNetHeaders;
begin
  Result := [];

  var NormPath := NormalizePath(Path);
  var ManagedAgentsEndpoint := IsManagedAgentsEndpoint(NormPath);

  if IsFilesEndpoint(NormPath) then
    begin
      Result := [TNetHeader.Create('anthropic-beta', 'files-api-2025-04-14')];
      Exit;
    end;

  var Clean := StripUtf8Bom(PayloadJson).Trim;

  if Clean.IsEmpty then
    begin
      var Betas := TOrderedSet.Create;
      try
        AddEndpointBetas(NormPath, Betas);

        if Betas.Count > 0 then
          Result := [TNetHeader.Create('anthropic-beta', Betas.Join(','))];
      finally
        Betas.Free;
      end;

      Exit;
    end;

  var Root := TJSONObject.ParseJSONValue(Clean);
  if Root = nil then
    raise Exception.Create(
      'Invalid JSON payload: System.JSON parser returned nil (check comments, trailing commas, invalid quoting).'
    );

  try
    var Betas := TOrderedSet.Create;
    try
      AddEndpointBetas(NormPath, Betas);
      AddFastModeBetaIfNeeded(Root, Betas);

      if not ManagedAgentsEndpoint then
        begin
          ValidateToolSearchCompatibility(Root);
          ValidateToolSearchNotDeferred(Root);
          ValidateAllToolsDeferred(Root);

          AddBetasFromToolsAnyRoot(Root, Betas);
          AddBetasFromMessagesContainerUpload(Root, Betas);
          AddBetasFromOutputFormat(Root, Betas);
          AddBetasFromContainerSkills(Root, Betas);
          AddMcpBetasIfNeeded(Root, Betas);
          AddContextManagementBetasIfNeeded(Root, Betas);
          AddTaskBudgetBetaIfNeeded(Root, Betas);
          AddFineGrainedToolStreamingBetaIfNeeded(Root, Betas);
          AddInterleavedThinkingBetaIfNeeded(Root, Betas);
        end;

      if Betas.Count = 0 then
        Exit;

      Result := [TNetHeader.Create('anthropic-beta', Betas.Join(','))];
    finally
      Betas.Free;
    end;
  finally
    Root.Free;
  end;
end;

{ TBetaHeaderManager }

class function TBetaHeaderManager.Build(const Path, PayloadJson: string): TNetHeaders;
begin
  Result := TBetaHeaderOps.BuildHeaders(Path, PayloadJson);
end;

end.

