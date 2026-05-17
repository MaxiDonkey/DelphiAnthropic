unit Anthropic.Browser.Helpers;

interface

uses
  System.SysUtils,
  WVPythia.TextFile.Helper, WVPythia.Vendors.Services,
  Anthropic, Anthropic.Types;

type
  TMessageContentBuilder = record
  private
    class procedure AppendImageBlock(
      const AFullPath: string;
      var ABlocks: TArray<TContentBlockParam>); static;

    class procedure AppendDocumentBlock(
      const AFullPath: string;
      var ABlocks: TArray<TContentBlockParam>); static;

    class procedure AppendTextBlock(
      const AText: string;
      var ABlocks: TArray<TContentBlockParam>); static;
  public
    class function BuildContentBlocks(
      const AState: TStateBuffer): TArray<TContentBlockParam>; static;
  end;

  TMessageBuilder = record
    class function GetMessages(
      const AState: TStateBuffer;
      const Content: TArray<TContentBlockParam>): TArray<TMessageParam>; overload; static;

    class function GetMessages(const AState: TStateBuffer): TArray<TMessageParam>; overload; static;
  end;

  TThinkingBuilder = record
  private
    class function IsThinkingEnabled(const Effort: string): Boolean; static;
    class function MapToOutputEffort(const Effort: string): string; static;
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

  TRequestSettingsBuilder = record
  private
    class function HasThinking(const AState: TStateBuffer): Boolean; static;
    class function HasTopP(const AState: TStateBuffer): Boolean; static;
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
  var ABlocks: TArray<TContentBlockParam>);
begin
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

  if SameText(MimeType, 'text/plain') or
     SameText(MimeType, 'application/octet-stream') or
     SameText(MimeType, 'text/html') then  //fichier markdown
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
    AppendDocumentBlock(Item.FullPath, Result);

// les fichiers KnowledgeSearch sont destinés à être vectorisés
//  for var Item in AState.KnowledgeSearch do
//    AppendDocumentBlock(Item.FullPath, Result);

  AppendTextBlock(AState.Text, Result);
end;

{ TMessageBuilder }

class function TMessageBuilder.GetMessages(
  const AState: TStateBuffer;
  const Content: TArray<TContentBlockParam>): TArray<TMessageParam>;
begin
  {--- Wraps all prepared content into a single message with the "user" role.
       This builder centralizes that convention to keep the messages array
       shape stable before it is passed into request parameters. }
  Result := [ TMessageParam.New
        .Role('user')
        .Content(Content)  //TODO ajouter l'historique à concurrence de la taille du contexte.
    ];
end;

class function TMessageBuilder.GetMessages(
  const AState: TStateBuffer): TArray<TMessageParam>;
begin
  {--- Convenience overload: rebuilds the blocks from the state and then
       delegates to the canonical GetMessages version so message array
       construction stays centralized in one place. }
  Result := GetMessages(AState, TMessageContentBuilder.BuildContentBlocks(AState));
end;

{ TThinkingBuilder }

class function TThinkingBuilder.IsThinkingEnabled(
  const Effort: string): Boolean;
begin
  {--- The string "none" is used as the functional sentinel for disabling
       thinking mode. Any other value is treated as an active request for
       extended reasoning. }
  Result := not SameText(Effort, 'none');
end;

class function TThinkingBuilder.MapToOutputEffort(
  const Effort: string): string;
begin
  {--- Maps the UI/business-level effort granularity to the values expected
       by output_config.effort. The mapping is not one-to-one:
       "medium" becomes "high" and "high" becomes "max". }
  if SameText(Effort, 'low') then
    Exit('low');

  if SameText(Effort, 'medium') then
    Exit('high');

  if SameText(Effort, 'high') then
    Exit('max');

  {--- An empty return means no usable output_config can be produced.
       The calling code relies on this behavior to skip applying anything. }
  Result := '';
end;

class function TThinkingBuilder.GetTOutputConfig(
  const AState: TStateBuffer;
  const Effort: string): TOutputConfig;
var
  HasStruturedOutput: Boolean;
  HasEffort: Boolean;
begin
  var CoreParams := AState.CoreParamsState;
  HasStruturedOutput := CoreParams.StructuredOutput.Enabled;

  var Content := CoreParams.StructuredOutput.Value;

  {--- Builds a TOutputConfig object only when the requested level can be
       translated into a value supported by the SDK. Otherwise nil is returned
       so the caller can decide not to populate the field. }
  var OutputEffort := MapToOutputEffort(Effort);
  HasEffort := not OutputEffort.IsEmpty;

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
  Result := IsThinkingEnabled(Effort);

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

  Result := IsThinkingEnabled(Effort) or HasStructuredOutput;

  if not Result then
    Exit;

  if Assigned(ParamProc) then
    ParamProc();
end;

{ TToolsBuilder }

class function TToolsBuilder.TryToBuild(const AState: TStateBuffer;
  const ParamProc: TProc): Boolean;
begin
  {--- Enables tool construction only when the current state explicitly
       requested web search. This builder acts as an activation guard to avoid
       emitting "tools" parameters when the feature is not in use. }
  Result := AState.WebSearch;
  if not Result then
    Exit;

  if Assigned(ParamProc) then
    ParamProc();
end;

{ TRequestSettingsBuilder }

class function TRequestSettingsBuilder.HasThinking(
  const AState: TStateBuffer): Boolean;
begin
  {--- Local helper used to centralize the "thinking enabled or not" rule.
       This information affects several request parameters, especially
       temperature, which becomes constrained when thinking is enabled. }
  Result := not SameText(AState.Thinking, 'none');
end;

class function TRequestSettingsBuilder.HasTopP(
  const AState: TStateBuffer): Boolean;
begin
  Result := AState.CoreParamsState.Sampling.TopP.Enabled;
end;

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
  if HasTopP(AState) then
    Exit(False);

  if HasThinking(AState) then
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

  Result := CoreParams.Sampling.TopK.Enabled;
  if Result and Assigned(ParamProc) then
    ParamProc(CoreParams.Sampling.TopK.Value);
end;

class function TRequestSettingsBuilder.TryApplyTopP(const AState: TStateBuffer;
  const ParamProc: TProc<Double>): Boolean;
begin
  var CoreParams := AState.CoreParamsState;

  Result := CoreParams.Sampling.TopP.Enabled;
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

end.
