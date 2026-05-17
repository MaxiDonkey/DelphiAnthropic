unit Demo.Anthropic.Helpers;

interface

uses
  System.SysUtils, System.IOUtils, System.JSON,
  WVPythia.TextFile.Helper, WVPythia.Vendors.Services, WVPythia.JSON.SafeWriter,
  WVPythia.Types.EnumWire,
  Anthropic, Anthropic.Types, Anthropic.API.JsonSafeReader;

type
  TSkillItem = record
  public
    Id: string;
    Name: string;
    FileToDownload: Boolean;
    SkillType: string;
    Version: string;
    class function Empty: TSkillItem; static; inline;
  end;

  TCustomSkill = record
  public
    ID: string;
    Name: string;
    class function Empty: TCustomSkill; static; inline;
  end;

  TSkillHelper = record
    class function ExtractCustomSkills(
      const SkillsJsonAsString: string): TArray<TCustomSkill>; static;

    class function TryToUpdateID(
      const SkillJsonAsString: string;
      const AName: string;
      const NewId: string;
      const ParamProc: TProc<string>): Boolean; static;
  end;

  TJSONArrayHelper = record
    class function ArrayOfStringToJSonArrayAsString(const Value: TArray<string>): string; static;
  end;

  TArrayUtils = record
    class function Merge(const T1, T2: TArray<string>): TArray<string>; static;
    class function ArrayRemoveDuplicates(const Value: TArray<string>): TArray<string>; static;
  end;

  TParamsGetter = record
    class function GetThinkingBudget(const AState: TStateBuffer;
      out MaxTokens: Integer): Integer; static;
    class function GetSkills(const AState: TStateBuffer): TArray<TSkillItem>; static;
    class function GetMCPNames(const AState: TStateBuffer): TArray<string>; static;
    class function TryReadMCPCard(
      const Reader: TJsonReader;
      const X: string;
      out content, pat: string): Boolean; static;
    class function CheckFilename(const Filename, Folder: string): string; static;
  end;

  TStateChecking = record
    class function HasThinking(const AState: TStateBuffer): Boolean; static;
    class function HasEffort(const Effort: string): Boolean; static;
    class function HasTopP(const AState: TStateBuffer): Boolean; static;
    class function HasSkills(const AState: TStateBuffer): Boolean; static;
    class function HasMCP(const AState: TStateBuffer): Boolean; static;
    class function HasFileToDownload(const AState: TStateBuffer): Boolean; static;
    class function HasAPIFileUsed(const AState: TStateBuffer): Boolean; static;
    class function ModelVersion47Check(const AState: TStateBuffer): Boolean; static;
    class function ModelVersion46Check(const AState: TStateBuffer): Boolean; static;
    class function MapToOutputEffort(const Effort: string; out BudgetCoef: Double): string; static;
    class function AdaptiveThinkingCheck(const AState: TStateBuffer): Boolean; static;
    class function SummarizedThinking(const AState: TStateBuffer): string; static;
  end;

  TMessageContentBuilder = record
  private
    class procedure AppendImageBlock(
      const AFullPath: string;
      var ABlocks: TArray<TContentBlockParam>); static;

    class procedure AppendDocumentBlock(
      const AFullPath: string;
      const AFileId: string;
      var ABlocks: TArray<TContentBlockParam>); static;

    class procedure AppendTextBlock(
      const AText: string;
      var ABlocks: TArray<TContentBlockParam>); static;
  public
    class function BuildContentBlocks(
      const AState: TStateBuffer): TArray<TContentBlockParam>; static;
  end;

  TThinkingBuilder = record
  public
    class function TryGetOutputConfig(
      const AState: TStateBuffer;
      out Effort: string;
      const ParamProc: TProc): Boolean; static;

    class function TryGetThinkingConfigParam(
      const AState: TStateBuffer;
      const Effort: string;
      const ParamProc: TProc): Boolean; static;

    class function GetTOutputConfig(
      const AState: TStateBuffer;
      const Effort: string): TOutputConfig; static;
  end;

  TToolsBuilder = record
    class function TryToBuild(
      const AState: TStateBuffer;
      const ParamProc: TProc): Boolean; static;
  end;

  /// <summary>
  /// Typed extraction of file_ids carried by streamed code execution result
  /// blocks. Replaces the older
  /// JSON-string-scanning approach (TAnthropicFileIdExtractor /
  /// NormalizeJsonResponse), which was sensitive to the SDK's
  /// internal RawJson concatenation format.
  /// </summary>
  TAnthropicStreamCapture = record
  public
    class procedure CaptureCodeExecutionFileIds(
      const Event: TChatStream;
      const Sink: TProc<string>); static;
  end;

  TRequestSettingsBuilder = record
  public
    class procedure ApplyMaxTokens(
      const AState: TStateBuffer;
      const ParamProc: TProc<Integer>;
      const ADefaultValue: Integer = 1000); static;

    class function TryApplySystemPrompt(
      const AState: TStateBuffer;
      const ParamProc: TProc<string>): Boolean; static;

    class function TryApplyTemperature(
      const AState: TStateBuffer;
      const ParamProc: TProc<Double>): Boolean; static;

    class function TryApplyStopSequences(
      const AState: TStateBuffer;
      const ParamProc: TProc): Boolean; static;

    class function TryApplyTopK(
      const AState: TStateBuffer;
      const ParamProc: TProc<Double>): Boolean; static;

    class function TryApplyTopP(
      const AState: TStateBuffer;
      const ParamProc: TProc<Double>): Boolean; static;

    class procedure Apply(
      const AState: TStateBuffer;
      const Params: TChatParams;
      const ADefaultMaxTokens: Integer = 1000); static;
  end;

implementation

uses
  System.Generics.Collections, System.Generics.Defaults, WVPythia.Net.MediaCodec;

{ TMessageContentBuilder }

class procedure TMessageContentBuilder.AppendImageBlock(
  const AFullPath: string;
  var ABlocks: TArray<TContentBlockParam>);
begin
  {--- Always converts a local image into an Anthropic-compatible block.
       The file is base64-encoded and tagged with its MIME type so the API
       can correctly interpret the binary payload. }
  var Block := TImageBlockParam.New
    .Source(
      TMediaCodec.EncodeBase64(AFullPath),
      TMediaCodec.GetMimeType(AFullPath)
    );

  ABlocks := ABlocks + [Block];
end;

class procedure TMessageContentBuilder.AppendDocumentBlock(
  const AFullPath: string;
  const AFileId: string;
  var ABlocks: TArray<TContentBlockParam>);
var
  LMimeType: string;
begin
  if TMediaCodec.TryResolveMimeTypeAsText(AFullPath, LMimeType) then
    begin
      {--- Plain text files are injected directly into the document block.
           This avoids unnecessary encoding and allows the model to consume
           the textual content without an extra decoding step. }
      var Block := TDocumentBlockParam.New
        .Source(
          TPlainTextSource.New
            .Data(TFileIOHelper.LoadFromFile(AFullPath))
            .MediaType('text/plain')
        );

      ABlocks := ABlocks + [Block];
      Exit;
    end;

  {--- Document handling depends on the MIME type detected locally.
       This method acts as the dispatch point between the formats actually
       supported by the Anthropic payload built here. }
  var MimeType := TMediaCodec.GetMimeType(AFullPath);

  if SameText(MimeType, 'application/pdf') then
    begin
      {--- PDFs are sent as base64-encoded binary documents.
           The media type is preserved explicitly so the backend can
           distinguish this payload from plain serialized text. }
      var Block := TDocumentBlockParam.New
        .Source(
          TBase64PDFSource.New
            .Data(TMediaCodec.EncodeBase64(AFullPath))
            .MediaType(MimeType)
        );

      ABlocks := ABlocks + [Block];
      Exit;
    end;

  if SameText(MimeType, 'application/zip') or
     SameText(MimeType, 'application/x-zip-compressed') or
     SameText(MimeType, 'application/x-tar') or
     SameText(MimeType, 'application/gzip') or
     SameText(MimeType, 'application/x-gzip') then
    begin
      var Block := TContainerUploadBlockParam.New.FileId(AFileId);

      ABlocks := ABlocks + [Block];
      Exit;
    end;

end;

class procedure TMessageContentBuilder.AppendTextBlock(
  const AText: string;
  var ABlocks: TArray<TContentBlockParam>);
begin
  {--- Free-form text is always appended last as the conversational block.
       This makes it possible to aggregate contextual attachments first and
       then add the user's instruction or question. }
  ABlocks := ABlocks + [
    TTextBlockParam.New.Text(AText)
  ];
end;

class function TMessageContentBuilder.BuildContentBlocks(
  const AState: TStateBuffer): TArray<TContentBlockParam>;
begin
  {--- Builds the multimodal user message content from the current UI state.
       The aggregation order is meaningful: images, files, results, and then
       free-form text. }
  Result := [];

  for var Item in AState.Images do
    AppendImageBlock(Item.FullPath, Result);

  for var Item in AState.Files do
    AppendDocumentBlock(Item.FullPath, Item.FileId, Result);

  {--- KnowledgeSearch files are intentionally not appended as prompt documents.
       They are expected to be indexed/vectorized and retrieved through the
       knowledge-search pipeline instead of being injected directly here. }
//  for var Item in AState.KnowledgeSearch do
//    AppendDocumentBlock(Item.FullPath, Result);

  AppendTextBlock(AState.Text, Result);
end;

{ TThinkingBuilder }

class function TThinkingBuilder.GetTOutputConfig(
  const AState: TStateBuffer;
  const Effort: string): TOutputConfig;
var
  HasStruturedOutput: Boolean;
  HasEffort: Boolean;
  BudgetCoef: Double;
begin
  var CoreParams := AState.CoreParamsState;
  HasStruturedOutput := CoreParams.StructuredOutput.Enabled;

  var Content := CoreParams.StructuredOutput.Value;

  {--- Builds a TOutputConfig object only when the requested level can be
       translated into a value supported by the SDK. Otherwise nil is returned
       so the caller can decide not to populate the field. }
  var OutputEffort := TStateChecking.MapToOutputEffort(Effort, BudgetCoef);
  HasEffort := not OutputEffort.IsEmpty;

  var IsAdaptive := TStateChecking.AdaptiveThinkingCheck(AState);
  if not IsAdaptive then
    HasEffort := False;

  if HasEffort then
    begin
      if HasStruturedOutput then
        Result := TOutputConfig.New
          .Effort(OutputEffort)
          .Format(CoreParams.StructuredOutput.Value)
      else
        Result := TOutputConfig.New
          .Effort(OutputEffort)
    end
  else
  if HasStruturedOutput then
    Result := TOutputConfig.New
      .Format(CoreParams.StructuredOutput.Value)
  else
    Result := nil;
end;

class function TThinkingBuilder.TryGetOutputConfig(
  const AState: TStateBuffer;
  out Effort: string;
  const ParamProc: TProc): Boolean;
begin
  {--- Exposes the current thinking level while applying the same logical
       guard used elsewhere: if thinking is disabled, the parameterization
       procedure is not executed. }
  Effort := AState.Thinking;
  Result := TStateChecking.HasEffort(Effort);

  if not Result then
    Exit;

  if Assigned(ParamProc) then
    ParamProc();
end;

class function TThinkingBuilder.TryGetThinkingConfigParam(
  const AState: TStateBuffer;
  const Effort: string;
  const ParamProc: TProc): Boolean;
begin
  var StructuredOutput := AState.CoreParamsState.StructuredOutput;
  var HasStructuredOutput :=
    StructuredOutput.Enabled and not StructuredOutput.Value.IsEmpty;

  Result := TStateChecking.HasEffort(Effort) or HasStructuredOutput;

  if not Result then
    Exit;

  if Assigned(ParamProc) then
    ParamProc();
end;

{ TToolsBuilder }

class function TToolsBuilder.TryToBuild(const AState: TStateBuffer;
  const ParamProc: TProc): Boolean;
begin
  {--- Enables tool construction only when the current state explicitly requested
       web search, skills or MCP. This builder acts as an activation guard to avoid
       emitting "tools" parameters when the feature is not in use. }
  Result := AState.WebSearch or
            TStateChecking.HasSkills(AState) or
            TStateChecking.HasMCP(AState);

  if not Result then
    Exit;

  if Assigned(ParamProc) then
    ParamProc();
end;

{ TRequestSettingsBuilder }

class procedure TRequestSettingsBuilder.ApplyMaxTokens(
  const AState: TStateBuffer;
  const ParamProc: TProc<Integer>;
  const ADefaultValue: Integer);
var
  Value: Integer;
begin
  {--- Resolves the final max_tokens value from the business configuration.
       If the setting is not enabled in the state, it falls back to a default
       value provided by the caller to keep the request valid. }
  var CoreParams := AState.CoreParamsState;

  if CoreParams.Settings.MaxToken.Enabled then
    Value := CoreParams.Settings.MaxToken.Value
  else
    Value := ADefaultValue;

  if Assigned(ParamProc) then
    ParamProc(Value);
end;

class function TRequestSettingsBuilder.TryApplySystemPrompt(
  const AState: TStateBuffer;
  const ParamProc: TProc<string>): Boolean;
begin
  {--- The system prompt is sent only when it is explicitly enabled and non-empty.
       The Trim check avoids sending a purely cosmetic "system" field that
       would provide no actionable instruction to the model. }
  var CoreParams := AState.CoreParamsState;

  Result := CoreParams.SystemPrompt.Enabled
    and (Trim(CoreParams.SystemPrompt.Value) <> '');

  if Result and Assigned(ParamProc) then
    ParamProc(CoreParams.SystemPrompt.Value);
end;

class function TRequestSettingsBuilder.TryApplyTemperature(
  const AState: TStateBuffer;
  const ParamProc: TProc<Double>): Boolean;
begin
  {--- When thinking is enabled, temperature is forced to 1.0 to satisfy
       the business/API constraint enforced by this project. The user setting
       is therefore used only when thinking is disabled. }
  if TStateChecking.HasTopP(AState) then
    Exit(False);

  if TStateChecking.ModelVersion47Check(AState) then
    Exit(False);

  if TStateChecking.HasThinking(AState) then
    begin
      if Assigned(ParamProc) then
        ParamProc(1.0);
      Exit(True);
    end;

  var CoreParams := AState.CoreParamsState;

  Result := CoreParams.Settings.Temperature.Enabled;
  if Result and Assigned(ParamProc) then
    ParamProc(CoreParams.Settings.Temperature.Value);
end;

class function TRequestSettingsBuilder.TryApplyTopK(const AState: TStateBuffer;
  const ParamProc: TProc<Double>): Boolean;
begin
  var CoreParams := AState.CoreParamsState;

  Result :=
    CoreParams.Sampling.TopK.Enabled and
    not TStateChecking.ModelVersion47Check(AState);

  if Result and Assigned(ParamProc) then
    ParamProc(CoreParams.Sampling.TopK.Value);
end;

class function TRequestSettingsBuilder.TryApplyTopP(const AState: TStateBuffer;
  const ParamProc: TProc<Double>): Boolean;
begin
  var CoreParams := AState.CoreParamsState;

  Result :=
    CoreParams.Sampling.TopP.Enabled and
    not TStateChecking.ModelVersion47Check(AState);

  if Result and Assigned(ParamProc) then
    ParamProc(CoreParams.Sampling.TopP.Value);
end;

class function TRequestSettingsBuilder.TryApplyStopSequences(
  const AState: TStateBuffer;
  const ParamProc: TProc): Boolean;
begin
  {--- Stop sequences are applied only when the option is enabled.
       The effective value is still read at the last moment by the caller so
       this method stays focused on the application condition only. }
  var CoreParams := AState.CoreParamsState;

  Result := CoreParams.Settings.StopString.Enabled;
  if Result and Assigned(ParamProc) then
    ParamProc();
end;

class procedure TRequestSettingsBuilder.Apply(
  const AState: TStateBuffer;
  const Params: TChatParams;
  const ADefaultMaxTokens: Integer);
begin
  {--- Central entry point that projects the UI/business state onto TChatParams.
       Each sub-builder encapsulates its own activation rule so this method
       remains readable orchestration rather than a block of conditions. }
  ApplyMaxTokens(AState,
    procedure(Value: Integer)
    begin
      Params.MaxTokens(Value);
    end,
    ADefaultMaxTokens);

  TryApplySystemPrompt(AState,
    procedure(Value: string)
    begin
      Params.System(Value);
    end);

  TryApplyTemperature(AState,
    procedure(Value: Double)
    begin
      Params.Temperature(Value);
    end);

  TryApplyStopSequences(AState,
    procedure
    begin
      {--- StopString is read intentionally at application time so it stays aligned
           with the effective configuration stored in CoreParamsState when the
           request is finally assembled. }
      var CoreParams := AState.CoreParamsState;
      Params.StopSequences(CoreParams.Settings.StopString.Value);
    end);

  TryApplyTopK(AState,
    procedure(Value: Double)
    begin
      Params.TopK(Value);
    end);

  TryApplyTopP(AState,
    procedure(Value: Double)
    begin
      Params.TopP(Value);
    end);
end;

{ TStateChecking }

class function TStateChecking.AdaptiveThinkingCheck(
  const AState: TStateBuffer): Boolean;
begin
  Result := ModelVersion46Check(AState) or ModelVersion47Check(AState);
end;

class function TStateChecking.HasAPIFileUsed(
  const AState: TStateBuffer): Boolean;
begin
  if Length(AState.Files) = 0 then
    Exit(False);

  for var Item in AState.Files do
    if not Item.FileId.IsEmpty then
      begin
        Exit(True);
      end;

  Result := False;
end;

class function TStateChecking.HasEffort(const Effort: string): Boolean;
begin
  {--- The string "none" is used as the functional sentinel for disabling
       thinking mode. Any other value is treated as an active request for
       extended reasoning. }
  Result := not SameText(Effort, 'none');
end;

class function TStateChecking.HasFileToDownload(
  const AState: TStateBuffer): Boolean;
begin
  {--- Check if skills anthropic exists }
  var Skills := TParamsGetter.GetSkills(AState);

  Result := Length(Skills) > 0;
end;

class function TStateChecking.HasMCP(const AState: TStateBuffer): Boolean;
begin
  Result := Length(AState.Integration.Mcp) > 0;
end;

class function TStateChecking.HasSkills(const AState: TStateBuffer): Boolean;
begin
  Result := Length(AState.Integration.Skills) > 0;
end;

class function TStateChecking.ModelVersion46Check(
  const AState: TStateBuffer): Boolean;
begin
  var Model := AState.Model.ToLowerInvariant;
  Result := Model.Contains('-4-6');
end;

class function TStateChecking.ModelVersion47Check(
  const AState: TStateBuffer): Boolean;
begin
  var Model := AState.Model.ToLowerInvariant;
  Result := Model.Contains('-4-7');
end;

class function TStateChecking.SummarizedThinking(
  const AState: TStateBuffer): string;
begin
  Result := 'omitted';

  if Length(AState.Custom) = 0 then
    Exit;

  {--- Check the custom table to see if summarized is active. }
  for var Item in AState.Custom do
    if SameText(Item.Name.ToLowerInvariant, 'summarized') then
      Exit('summarized');
end;

class function TStateChecking.HasThinking(const AState: TStateBuffer): Boolean;
begin
  {--- Local helper used to centralize the "thinking enabled or not" rule.
       This information affects several request parameters, especially
       temperature, which becomes constrained when thinking is enabled. }
  Result := not SameText(AState.Thinking, 'none');
end;

class function TStateChecking.HasTopP(const AState: TStateBuffer): Boolean;
begin
  Result := AState.CoreParamsState.Sampling.TopP.Enabled;
end;

class function TStateChecking.MapToOutputEffort(const Effort: string;
  out BudgetCoef: Double): string;
begin
  {--- Maps the UI/business-level effort granularity to the values expected
       by output_config.effort. The mapping is not one-to-one:
       "medium" becomes "high" and "high" becomes "max". }
  if SameText(Effort, 'low') then
    begin
      BudgetCoef := 1.3;
      Exit('low');
    end;

  if SameText(Effort, 'medium') then
    begin
      BudgetCoef := 1.5;
      Exit('high');
    end;

  if SameText(Effort, 'high') then
    begin
      BudgetCoef := 1.7;
      Exit('max');
    end;

  {--- An empty return means no usable output_config can be produced.
       The calling code relies on this behavior to skip applying anything. }
  Result := '';
end;

{ TParamsGetter }

class function TParamsGetter.CheckFilename(const Filename,
  Folder: string): string;
begin
  var Candidate := TPath.GetFileName(Filename);
  var BaseName := TPath.GetFileNameWithoutExtension(Candidate);
  var Ext := TPath.GetExtension(Candidate);
  var index := 1;

  while Length(TDirectory.GetFiles(Folder, Candidate)) > 0 do
    begin
      Candidate := Format('%s (%d)%s', [BaseName, index, Ext]);
      Inc(index);
    end;

  Result := TPath.Combine(Folder, Candidate);
end;

class function TParamsGetter.GetMCPNames(
  const AState: TStateBuffer): TArray<string>;
begin
  SetLength(Result, Length(AState.Integration.Mcp));
  for var I := Low(AState.Integration.Mcp) to High(AState.Integration.Mcp) do
    Result[i] := AState.Integration.Mcp[I].Name;
end;

class function TParamsGetter.GetSkills(
  const AState: TStateBuffer): TArray<TSkillItem>;
begin
  SetLength(Result, Length(AState.Integration.Skills));

  var index := 0;
  for var Item In AState.Integration.Skills  do
    begin
      var isAnthropicSkills :=
        SameText(Item.Name, 'xlsx') or
        SameText(Item.Name, 'pptx') or
        SameText(Item.Name, 'pdf') or
        SameText(Item.Name, 'docx');

      if isAnthropicSkills then
        begin
          Result[index] := TSkillItem.Empty;
          Result[index].Id := Item.Name;
          Result[index].Name := Item.Name;
          Result[index].FileToDownload := True;
          Result[index].SkillType := 'anthropic';
          Result[index].Version := 'latest';
        end
      else
        begin
          Result[index] := TSkillItem.Empty;
          Result[index].Id := Item.Id;
          Result[index].Name := Item.Name;
          Result[index].FileToDownload := False;
          Result[index].SkillType := 'custom';
          Result[index].Version := 'latest';
        end;

      Inc(index);
    end;
end;

class function TParamsGetter.GetThinkingBudget(
  const AState: TStateBuffer; out MaxTokens: Integer): Integer;
var
  BudgetCoef: Double;
begin
  TStateChecking.MapToOutputEffort(AState.Thinking, BudgetCoef);

  MaxTokens := Trunc(AState.CoreParamsState.Settings.MaxToken.Value * BudgetCoef);

  Result := AState.CoreParamsState.Settings.MaxToken.Value;
end;

class function TParamsGetter.TryReadMCPCard(const Reader: TJsonReader;
  const X: string; out content, pat: string): Boolean;
begin
  Content := '';
  Pat := '';

  for var I := 0 to Reader.Count('cards') - 1 do
    begin
      var CardPath := 'cards[' + I.ToString + ']';

      if SameText(X, Reader.AsString(CardPath + '.id')) or
         SameText(X, Reader.AsString(CardPath + '.name')) then
        begin
          Content := Reader.AsString(CardPath + '.content');
          Pat := Reader.AsString(CardPath + '.pat');
          Exit(True);
        end;
    end;

  Result := False;
end;


{ TJSONArrayHelper }

class function TJSONArrayHelper.ArrayOfStringToJSonArrayAsString(
  const Value: TArray<string>): string;
begin
  Result := '';

  for var Item in Value do
    if Result.IsEmpty then
      Result := Item
    else
      Result := Result + ',' + Item;

  Result := Format('[%s]', [Result]);
end;

{ TArrayUtils }

class function TArrayUtils.ArrayRemoveDuplicates(
  const Value: TArray<string>): TArray<string>;
begin
  var List := TList<string>.Create(TIStringComparer.Ordinal);
  try
    for var Item in Value do
      if not List.Contains(Item) then
        List.Add(Item);

    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class function TArrayUtils.Merge(const T1, T2: TArray<string>): TArray<string>;
begin
  Result := Copy(T1);
  var Count := Length(Result);

  for var Item2 in T2 do
    begin
      var Found := False;

      for var Item1 in T1 do
        if SameText(Item1, Item2) then
          begin
            Found := True;
            Break;
          end;

      if Found then
        Continue;

      SetLength(Result, Count + 1);
      Result[Count] := Item2;
      Inc(Count);
    end;
end;

{ TSkillItem }

class function TSkillItem.Empty: TSkillItem;
begin
  Result := Default(TSkillItem);
end;

{ TCustomSkill }

class function TCustomSkill.Empty: TCustomSkill;
begin
  Result := Default(TCustomSkill);
end;

{ TSkillHelper }

class function TSkillHelper.ExtractCustomSkills(
  const SkillsJsonAsString: string): TArray<TCustomSkill>;
begin
  SetLength(Result, 0);

  var Reader := TJsonReader.Parse(SkillsJsonAsString);
  if not Reader.IsValid then
    Exit;

  var Count := Reader.Count('cards');

  for var I := 0 to Count - 1 do
    begin
      var Path := Format('cards[%d]', [I]);

      if not SameText(Reader.AsString(Path + '.content'), 'custom') then
        Continue;

      var ID := Reader.AsString(Path + '.id');
      var Name := Reader.AsString(Path + '.name');

      if ID.IsEmpty or Name.IsEmpty then
        Continue;

      var SkillIndex := Length(Result);
      SetLength(Result, SkillIndex + 1);

      Result[SkillIndex].ID := ID;
      Result[SkillIndex].Name := Name;
    end;
end;

class function TSkillHelper.TryToUpdateID(const SkillJsonAsString, AName,
  NewId: string; const ParamProc: TProc<string>): Boolean;
var
  Writer: TJsonWriter;
begin
  Result := False;

  if AName.IsEmpty or NewId.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(SkillJsonAsString);
  if not Reader.IsValid then
    Exit;

  var CardCount := Reader.Count('cards');

  for var I := 0 to CardCount - 1 do
    begin
      var CardPath := Format('cards[%d]', [I]);

      if not SameText(Reader.AsString(CardPath + '.content'), 'custom') then
        Continue;

      if not SameText(Reader.AsString(CardPath + '.name'), AName) then
        Continue;

      Writer := TJsonWriter.Parse(SkillJsonAsString);

      Result := Writer.SetString(CardPath + '.id', NewId);
      Break;
    end;

  if Result and Assigned(ParamProc) then
    ParamProc(Writer.Format());
end;

{ TAnthropicStreamCapture }

class procedure TAnthropicStreamCapture.CaptureCodeExecutionFileIds(
  const Event: TChatStream;
  const Sink: TProc<string>);
{--- Reads file_ids out of streamed code_execution_tool_result and
     bash_code_execution_tool_result blocks using the SDK's typed model,
     instead of re-parsing the raw JSON aggregate. The SDK hydrates
     ContentBlock.ToolContent at the content_block_start event (see
     HydratePolymorphicContent), so by the time OnProgress fires for this
     event the typed graph is already populated. }
begin
  if (not Assigned(Event)) or (not Assigned(Sink)) then
    Exit;

  if Event.EventType <> TEventType.content_block_start then
    Exit;

  try
    var Block := Event.ContentBlock;
    if not Assigned(Block) then
      Exit;

    var Tool := Block.ToolContent;
    if not Assigned(Tool) then
      Exit;

    if Block.&Type = TContentBlockType.code_execution_tool_result then
      begin
        var CodeBlock := Tool.CodeExecutionToolResultBlock;
        if (not Assigned(CodeBlock)) or (not CodeBlock.HasValue) then
          Exit;

        var CodeContent := CodeBlock.Content;
        if not Assigned(CodeContent) then
          Exit;

        for var Item in CodeContent.Content do
          begin
            if not Assigned(Item) then
              Continue;
            Sink(Item.FileId);
          end;

        Exit;
      end;

    if Block.&Type = TContentBlockType.bash_code_execution_tool_result then
      begin
        var BashBlock := Tool.BashCodeExecutionToolResultBlock;
        if (not Assigned(BashBlock)) or (not BashBlock.HasValue) then
          Exit;

        var BashContent := BashBlock.Content;
        if not Assigned(BashContent) then
          Exit;

        for var Item in BashContent.Content do
          begin
            if not Assigned(Item) then
              Continue;
            Sink(Item.FileId);
          end;
      end;
  except
    {--- Defensive: a shape change in the SDK or an unexpected nil must
         never tear down the stream. Worst case we miss a file_id and
         the existing fallback (LoadFileResult) takes over. }
  end;
end;

end.
