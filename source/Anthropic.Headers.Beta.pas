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

   2. Update BuildToolTypeToBetaMap for any new or renamed tool type → beta token.

   3. Check hard-coded tool type detections (RootHasCodeExecutionTool, RootHasAnyWebFetchTool,
      RootHasAnyToolSearchTool, RootHasAnyMcpToolset).

   4. Check detections based on messages.content[].type (container_upload, tool_search_tool_result,
      web_fetch_tool_result, etc.).

   5. Check the special /v1/files endpoint handling (IsFilesEndpoint and its forced beta token).

   6. Revalidate all guards that raise errors (ValidateTextEditorMaxCharacters,
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

    class function TryGetJSONArray(const Obj: TJSONObject; const Name: string; out Arr: TJSONArray): Boolean; static;
    class function TryGetJSONObject(const Obj: TJSONObject; const Name: string; out Child: TJSONObject): Boolean; static;
    class function TryGetString(const Obj: TJSONObject; const Name: string; out S: string): Boolean; static;
    class function TryGetBool(const Obj: TJSONObject; const Name: string; out B: Boolean): Boolean; static;

    class function StartsWithI(const S, Prefix: string): Boolean; static;
    class function IsTextEditorToolType(const ToolType: string): Boolean; static;
    class procedure ValidateTextEditorMaxCharacters(const ToolObj: TJSONObject); static;

    class function IsKnownBetaToken(const S: string): Boolean; static;
    class function BuildToolTypeToBetaMap: TDictionary<string, string>; static;

    class procedure AddAdvancedToolUseIfExamplesPresentInTool(const ToolObj: TJSONObject; const Betas: TOrderedSet); static;
    class procedure AddBetasFromSingleToolObject(const ToolObj: TJSONObject; const Betas: TOrderedSet; const Map: TDictionary<string,string>); static;

    class function RootHasTools(const Root: TJSONValue): Boolean; static;
    class function RootHasField(const Root: TJSONValue; const FieldName: string): Boolean; static;

    class function RootHasContainerReuse(const Root: TJSONValue): Boolean; static;
    class function RootHasMcpServers(const Root: TJSONValue): Boolean; static;

    class function RootHasProgrammaticSignalsInMessages(const Root: TJSONValue): Boolean; static;
    class function JsonContainsToolReference(const V: TJSONValue): Boolean; static;
    class function RootHasToolSearchServerToolUseInMessages(const Root: TJSONValue): Boolean; static;
    class function RootHasToolSearchSignalsInMessages(const Root: TJSONValue): Boolean; static;

    class function RootHasCodeExecutionTool(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyToolSearchTool(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyToolWithInputExamples(const Root: TJSONValue): Boolean; static;

    class procedure ValidateToolSearchCompatibility(const Root: TJSONValue); static;
    class procedure ValidateToolSearchNotDeferred(const Root: TJSONValue); static;
    class procedure ValidateAllToolsDeferred(const Root: TJSONValue); static;

    class function DetectBetaProvider(const Root: TJSONValue): string; static;
    class function ToolSearchBetaTokenForProvider(const Provider: string): string; static;

    class function RootHasAnyMcpToolset(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyWebSearchTool(const Root: TJSONValue): Boolean; static;
    class function RootHasWebSearchSignalsInMessages(const Root: TJSONValue): Boolean; static;
    class function RootHasAnyWebFetchTool(const Root: TJSONValue): Boolean; static;
    class function RootHasWebFetchSignalsInMessages(const Root: TJSONValue): Boolean; static;
    class function RootHasMcpSignalsInMessages(const Root: TJSONValue): Boolean; static;

    class procedure AddBetasFromToolsAnyRoot(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddBetasFromOutputFormat(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddBetasFromMessagesContainerUpload(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddFineGrainedToolStreamingIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddAdvancedToolUseIfProgrammaticCallingSignals(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddToolSearchBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddMcpBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddContextManagementBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddWebFetchBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet); static;
    class procedure AddWebSearchNotesNoBeta(const Root: TJSONValue; const Betas: TOrderedSet); static;
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
    SameText(S, 'advanced-tool-use-2025-11-20') or
    SameText(S, 'tool-search-tool-2025-10-19') or
    SameText(S, 'code-execution-2025-08-25') or
    SameText(S, 'context-management-2025-06-27') or
    SameText(S, 'files-api-2025-04-14') or
    SameText(S, 'fine-grained-tool-streaming-2025-05-14') or
    SameText(S, 'mcp-client-2025-11-20') or
    SameText(S, 'structured-outputs-2025-11-13') or
    SameText(S, 'web-fetch-2025-09-10') or
    SameText(S, 'computer-use-2025-11-24') or
    SameText(S, 'computer-use-2025-01-24') or
    SameText(S, 'computer-use-2024-10-22');
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

class function TBetaHeaderOps.BuildToolTypeToBetaMap: TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;

  Result.Add('code_execution_20250825',         'code-execution-2025-08-25');
  Result.Add('web_fetch_20250910',              'web-fetch-2025-09-10');
  Result.Add('files_api_20250414',              'files-api-2025-04-14');
  Result.Add('memory_20250818',                 'context-management-2025-06-27');

  Result.Add('computer_20251124',               'computer-use-2025-11-24');
  Result.Add('computer_20250124',               'computer-use-2025-01-24');
  Result.Add('computer_20241022',               'computer-use-2024-10-22');

  Result.Add('tool_search_tool_regex_20251119', 'advanced-tool-use-2025-11-20');
  Result.Add('tool_search_tool_bm25_20251119',  'advanced-tool-use-2025-11-20');

  Result.Add('mcp_toolset',                     'mcp-client-2025-11-20');

  Result.Add('clear_tool_uses_20250919',        'context-management-2025-06-27');
end;

class procedure TBetaHeaderOps.AddAdvancedToolUseIfExamplesPresentInTool(const ToolObj: TJSONObject; const Betas: TOrderedSet);
var
  B: Boolean;
begin
  if ToolObj = nil then Exit;

  if ToolObj.GetValue('input_examples') <> nil then
    begin
      Betas.Add('advanced-tool-use-2025-11-20');
      Exit;
    end;

  if ToolObj.GetValue('allowed_callers') <> nil then
    begin
      Betas.Add('advanced-tool-use-2025-11-20');
      Exit;
    end;

  if TryGetBool(ToolObj, 'defer_loading', B) and B then
    begin
      Betas.Add('advanced-tool-use-2025-11-20');
      Exit;
    end;
end;

class procedure TBetaHeaderOps.AddBetasFromSingleToolObject(
  const ToolObj: TJSONObject; const Betas: TOrderedSet; const Map: TDictionary<string,string>);
var
  ToolType, Beta: string;
begin
  if ToolObj = nil then Exit;

  ValidateTextEditorMaxCharacters(ToolObj);
  AddAdvancedToolUseIfExamplesPresentInTool(ToolObj, Betas);

  if not TryGetString(ToolObj, 'type', ToolType) then Exit;

  if StartsWithI(ToolType, 'web_search_') then Exit;
  if IsTextEditorToolType(ToolType) then Exit;
  if StartsWithI(ToolType, 'bash_') then Exit;

  if Map.TryGetValue(ToolType, Beta) then
    begin
      Betas.Add(Beta);
      Exit;
    end;

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

class function TBetaHeaderOps.RootHasContainerReuse(const Root: TJSONValue): Boolean;
begin
  Result := RootHasField(Root, 'container');
end;

class function TBetaHeaderOps.RootHasMcpServers(const Root: TJSONValue): Boolean;
begin
  Result := RootHasField(Root, 'mcp_servers');
end;

class function TBetaHeaderOps.RootHasProgrammaticSignalsInMessages(const Root: TJSONValue): Boolean;
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
      if (ContentVal <> nil) and (ContentVal is TJSONArray) then
        begin
          var ContentArr := TJSONArray(ContentVal);
          for var ItemVal in ContentArr do
          begin
            if not (ItemVal is TJSONObject) then Continue;
            var ItemObj := TJSONObject(ItemVal);

            if TryGetString(ItemObj, 'type', CType) then
              begin
                if SameText(CType, 'server_tool_use') then Exit(True);
                if SameText(CType, 'tool_use') and (ItemObj.GetValue('caller') <> nil) then Exit(True);
              end;
          end;
        end;
    end;
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

class function TBetaHeaderOps.RootHasCodeExecutionTool(const Root: TJSONValue): Boolean;
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

      if TryGetString(ToolObj, 'type', ToolType) and SameText(ToolType, 'code_execution_20250825') then
        Exit(True);
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

class function TBetaHeaderOps.DetectBetaProvider(const Root: TJSONValue): string;
var
  S: string;
begin
  Result := EmptyStr;
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if TryGetString(RootObj, 'beta_provider', S) then
    Result := S.Trim.ToLowerInvariant;
end;

class function TBetaHeaderOps.ToolSearchBetaTokenForProvider(const Provider: string): string;
begin
  if (Provider = 'vertex') or (Provider = 'bedrock') then
    Exit('tool-search-tool-2025-10-19');

  Result := 'advanced-tool-use-2025-11-20';
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

class function TBetaHeaderOps.RootHasAnyWebSearchTool(const Root: TJSONValue): Boolean;
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

      if TryGetString(ToolObj, 'type', ToolType) and StartsWithI(ToolType, 'web_search_') then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasWebSearchSignalsInMessages(const Root: TJSONValue): Boolean;
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
      if (ContentVal <> nil) and (ContentVal is TJSONArray) then
        begin
          var ContentArr := TJSONArray(ContentVal);
          for var ItemVal in ContentArr do
            begin
              if not (ItemVal is TJSONObject) then Continue;
              var ItemObj := TJSONObject(ItemVal);

              if TryGetString(ItemObj, 'type', CType) and SameText(CType, 'web_search_tool_result') then
                Exit(True);
            end;
        end;
    end;
end;

class function TBetaHeaderOps.RootHasAnyWebFetchTool(const Root: TJSONValue): Boolean;
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

      if TryGetString(ToolObj, 'type', ToolType) and SameText(ToolType, 'web_fetch_20250910') then
        Exit(True);
    end;
end;

class function TBetaHeaderOps.RootHasWebFetchSignalsInMessages(const Root: TJSONValue): Boolean;
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
      if (ContentVal <> nil) and (ContentVal is TJSONArray) then
        begin
          var ContentArr := TJSONArray(ContentVal);
          for var ItemVal in ContentArr do
            begin
              if not (ItemVal is TJSONObject) then Continue;
              var ItemObj := TJSONObject(ItemVal);

              if not TryGetString(ItemObj, 'type', CType) then Continue;

              if SameText(CType, 'web_fetch_tool_result') then
                Exit(True);

              if SameText(CType, 'server_tool_use') then
                if TryGetString(ItemObj, 'name', NameS) and SameText(NameS, 'web_fetch') then
                  Exit(True);
            end;
        end;
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

  if TryGetString(OutputFormat, 'type', OutputType) and SameText(OutputType, 'json_schema') then
    Betas.Add('structured-outputs-2025-11-13');
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

class procedure TBetaHeaderOps.AddFineGrainedToolStreamingIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
var
  StreamEnabled: Boolean;
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  if not TryGetBool(RootObj, 'stream', StreamEnabled) then Exit;
  if not StreamEnabled then Exit;

  if RootHasTools(Root) then
    Betas.Add('fine-grained-tool-streaming-2025-05-14');
end;

class procedure TBetaHeaderOps.AddAdvancedToolUseIfProgrammaticCallingSignals(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasContainerReuse(Root) then
    Betas.Add('advanced-tool-use-2025-11-20');

  if RootHasProgrammaticSignalsInMessages(Root) then
    Betas.Add('advanced-tool-use-2025-11-20');

  if (Root is TJSONObject) and RootHasCodeExecutionTool(Root) then
    begin
      var ToolsArr: TJSONArray;
      if TryGetJSONArray(TJSONObject(Root), 'tools', ToolsArr) and (ToolsArr.Count > 1) then
        Betas.Add('advanced-tool-use-2025-11-20');
    end;
end;

class procedure TBetaHeaderOps.AddToolSearchBetaIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
var
  Provider, Token: string;
begin
  if not (RootHasAnyToolSearchTool(Root) or RootHasToolSearchSignalsInMessages(Root)) then Exit;

  Provider := DetectBetaProvider(Root);
  Token := ToolSearchBetaTokenForProvider(Provider);
  Betas.Add(Token);
end;

class procedure TBetaHeaderOps.AddMcpBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasMcpServers(Root) or RootHasAnyMcpToolset(Root) or RootHasMcpSignalsInMessages(Root) then
    begin
      Betas.Add('mcp-client-2025-11-20');
      Betas.Add('advanced-tool-use-2025-11-20');
    end;
end;

class procedure TBetaHeaderOps.AddContextManagementBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if not (Root is TJSONObject) then Exit;

  var RootObj := TJSONObject(Root);
  var CtxMgmtVal := RootObj.GetValue('context_management');
  if (CtxMgmtVal = nil) or not (CtxMgmtVal is TJSONObject) then Exit;

  Betas.Add('context-management-2025-06-27');
end;

class procedure TBetaHeaderOps.AddWebFetchBetasIfNeeded(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasAnyWebFetchTool(Root) or RootHasWebFetchSignalsInMessages(Root) then
    Betas.Add('web-fetch-2025-09-10');
end;

class procedure TBetaHeaderOps.AddWebSearchNotesNoBeta(const Root: TJSONValue; const Betas: TOrderedSet);
begin
  if RootHasAnyWebSearchTool(Root) or RootHasWebSearchSignalsInMessages(Root) then
  begin
    { no-op (by design) }
  end;
end;

class function TBetaHeaderOps.BuildHeaders(const Path, PayloadJson: string): TNetHeaders;
begin
  Result := [];

  var NormPath := NormalizePath(Path);

  if IsFilesEndpoint(NormPath) then
    begin
      Result := [TNetHeader.Create('anthropic-beta', 'files-api-2025-04-14')];
      Exit;
    end;

  var Clean := StripUtf8Bom(PayloadJson).Trim;

  if Clean.IsEmpty then
    begin
      if IsMessageBatchesEndpoint(NormPath) then
        Result := [TNetHeader.Create('anthropic-beta', 'message-batches-2024-09-24')]
      else
      if IsSkillsEndpoint(NormPath) then
        Result := [TNetHeader.Create('anthropic-beta', 'skills-2025-10-02')];

      Exit;
    end;

  var Root := TJSONObject.ParseJSONValue(Clean);
  if Root = nil then
    raise Exception.Create(
      'Invalid JSON payload: System.JSON parser returned nil (check comments, trailing commas, invalid quoting).'
    );

  try
    ValidateToolSearchCompatibility(Root);
    ValidateToolSearchNotDeferred(Root);
    ValidateAllToolsDeferred(Root);

    var Betas := TOrderedSet.Create;
    try
      if IsMessageBatchesEndpoint(NormPath) then
        Betas.Add('message-batches-2024-09-24');

      if IsSkillsEndpoint(NormPath) then
        Betas.Add('skills-2025-10-02');


      AddBetasFromToolsAnyRoot(Root, Betas);
      AddBetasFromMessagesContainerUpload(Root, Betas);
      AddBetasFromOutputFormat(Root, Betas);
      AddFineGrainedToolStreamingIfNeeded(Root, Betas);
      AddAdvancedToolUseIfProgrammaticCallingSignals(Root, Betas);

      AddToolSearchBetaIfNeeded(Root, Betas);

      AddMcpBetasIfNeeded(Root, Betas);
      AddContextManagementBetasIfNeeded(Root, Betas);
      AddWebFetchBetasIfNeeded(Root, Betas);
      AddWebSearchNotesNoBeta(Root, Betas);

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

