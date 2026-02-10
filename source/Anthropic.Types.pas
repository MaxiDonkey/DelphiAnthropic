unit Anthropic.Types;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils,
  Anthropic.API.Params, Anthropic.Types.Rtti, Anthropic.Types.EnumWire;

type

  TArgsFixInterceptor = class(TJSONInterceptorStringToString)
  public
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

{$REGION 'Anthropic.Chat'}

  {$SCOPEDENUMS OFF}
  TMessageRole = (
    /// <summary>
    /// User message
    /// </summary>
    user,

    /// <summary>
    /// Assistant message
    /// </summary>
    assistant);

  TMessageRoleHelper = record helper for TMessageRole
    function ToString: string;
    class function Parse(const S: string): TMessageRole; static;
  end;

  TMessageRoleInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS OFF}
  TStopReason = (
    end_turn,
    max_tokens,
    stop_sequence,
    tool_use,
    pause_turn,
    refusal
  );

  TStopReasonHelper = record helper for TStopReason
    function ToString: string;
    class function Parse(const S: string): TStopReason; static;
  end;

  TStopReasonInterceptor = class(TJSONInterceptorStringToString)
  public
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS ON}
  TToolChoiceType = (
    /// <summary>
    /// Allows Claude to decide whether to call any provided tools or not. This is the default value.
    /// </summary>
    auto,

    /// <summary>
    /// Tells Claude that it must use one of the provided tools, but doesn’t force a particular tool.
    /// </summary>
    any,

    /// <summary>
    ///  Allows us to force Claude to always use a particular tool.
    /// </summary>
    tool,

    /// <summary>
    /// The model will not be allowed to use tools.
    /// </summary>
    none
  );

  TToolChoiceTypeHelper = record helper for TToolChoiceType
    function ToString: string;
    class function Parse(const S: string): TToolChoiceType; static;
  end;

  {$SCOPEDENUMS ON}
  TCachingType = (
    /// <summary>
    /// Cache is not used.
    /// </summary>
    nocaching,

    /// <summary>
    /// This is the only type currently defined by Anthropic.
    /// </summary>
    ephemeral
  );

  TCachingTypeHelper = record Helper for TCachingType
    function ToString: string;
    class function Parse(const S: string): TCachingType; static;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Batches'}

  {$SCOPEDENUMS OFF}
  TProcessingStatusType = (
    /// <summary>
    /// Batch of messages pending or being processed
    /// </summary>
    in_progress,

    /// <summary>
    /// Message batch processing canceled
    /// </summary>
    canceling,

    /// <summary>
    /// Batch processing is complete
    /// </summary>
    ended
  );

  TProcessingStatusTypeHelper = record helper for TProcessingStatusType
    function ToString: string;
    class function Parse(const S: string): TProcessingStatusType; static;
  end;


  TProcessingStatusInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Schema'}

  {$SCOPEDENUMS ON}
  TSchemaType = (
    /// <summary>
    /// Not specified, should not be used.
    /// </summary>
    TYPE_UNSPECIFIED,

    /// <summary>
    /// String type.
    /// </summary>
    &STRING,

    /// <summary>
    /// Number type.
    /// </summary>
    NUMBER,

    /// <summary>
    /// Integer type.
    /// </summary>
    &INTEGER,

    /// <summary>
    /// Boolean type.
    /// </summary>
    &BOOLEAN,

    /// <summary>
    /// Array type.
    /// </summary>
    &ARRAY,

    /// <summary>
    /// Object type.
    /// </summary>
    &OBJECT
  );

  TSchemaTypeHelper = record helper for TSchemaType
    function ToString: string;
    class function Parse(const S: string): TSchemaType; static;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Chat.StreamEvents'}

  {$SCOPEDENUMS ON}
  TEventType = (
    ping,
    error,
    message_start,
    content_block_start,
    content_block_delta,
    content_block_stop,
    message_delta,
    message_stop
  );

  TEventTypeHelper = record Helper for TEventType
    function ToString: string;
    class function Parse(const S: string): TEventType; static;
  end;

  TEventTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS ON}
  TDeltaType = (
    text_delta,
    input_json_delta,
    citations_delta,
    thinking_delta,
    signature_delta
  );

  TDeltaTypeHelper = record Helper for TDeltaType
    function ToString: string;
    class function Parse(const S: string): TDeltaType; static;
  end;

  TDeltaTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Chat.Request'}

  {$SCOPEDENUMS ON}
  TServiceTierType = (
    auto,
    standard_only
  );

  TServiceTierTypeHelper = record Helper for TServiceTierType
    function ToString: string;
    class function Parse(const S: string): TServiceTierType; static;
  end;

  {$SCOPEDENUMS ON}
  TContentType = (
    text,
    image,
    document,
    search_result,
    thinking,
    redacted_thinking,
    tool_use,
    tool_result,
    server_tool_use,
    web_search_tool_result
  );

  TContentTypeHelper = record Helper for TContentType
    function ToString: string;
    class function Parse(const S: string): TContentType; static;
  end;

  {$SCOPEDENUMS OFF}
  TWebSearchError = (
    query_too_long,
    invalid_tool_input,
    url_too_long,
    url_not_allowed,
    url_not_accessible,
    unsupported_content_type,
    too_many_requests,
    max_uses_exceeded,
    unavailable,
    execution_time_exceeded,
    output_file_too_large,
    file_not_found
  );

  TWebSearchErrorHelper = record Helper for TWebSearchError
    function ToString: string;
    class function Parse(const S: string): TWebSearchError; static;
  end;

  {$SCOPEDENUMS OFF}
  TBeta = (
    message_batches_2024_09_24,
    prompt_caching_2024_07_31,
    computer_use_2024_10_22,
    computer_use_2025_01_24,
    pdfs_2024_09_25,
    token_counting_2024_11_01,
    token_efficient_tools_2025_02_19,
    output_128k_2025_02_19,
    files_api_2025_04_14,
    mcp_client_2025_04_04,
    mcp_client_2025_11_20,
    dev_full_thinking_2025_05_14,
    interleaved_thinking_2025_05_14,
    code_execution_2025_05_22,
    extended_cache_ttl_2025_04_11,
    context_1m_2025_08_07,
    context_management_2025_06_27,
    model_context_window_exceeded_2025_08_26,
    skills_2025_10_02,
    fast_mode_2026_02_01,
    structured_outputs_2025_11_13  // deprecated since january 29, 2026
  );

  TBetaHelper = record Helper for TBeta
  const
    Betas: array[TBeta] of string = (
      'message-batches-2024-09-24',
      'prompt-caching-2024-07-31',
      'computer-use-2024-10-22',
      'computer-use-2025-01-24',
      'pdfs-2024-09-25',
      'token-counting-2024-11-01',
      'token-efficient-tools-2025-02-19',
      'output-128k-2025-02-19',
      'files-api-2025-04-14',
      'mcp-client-2025-04-04',
      'mcp-client-2025-11-20',
      'dev-full-thinking-2025-05-14',
      'interleaved-thinking-2025-05-14',
      'code-execution-2025-05-22',
      'extended-cache-ttl-2025-04-11',
      'context-1m-2025-08-07',
      'context-management-2025-06-27',
      'model-context-window-exceeded-2025-08-26',
      'skills-2025-10-02',
      'fast-mode-2026-02-01',
      'structured-outputs-2025-11-13'  // deprecated since january 29, 2026
    );
  public
    function ToString: string;
    class function Parse(const S: string): TBeta; static;
  end;

  {$SCOPEDENUMS OFF}
  TServerToolUseName = (
    web_search,
    web_fetch,
    code_execution,
    bash_code_execution,
    text_editor_code_execution,
    tool_search_tool_regex,
    tool_search_tool_bm25
  );

  TServerToolUseNameHelper = record Helper for TServerToolUseName
    function ToString: string;
    class function Parse(const S: string): TServerToolUseName; static;
  end;

  {$SCOPEDENUMS ON}
  TFileType = (
    text,
    image,
    pdf
  );

  TFileTypeHelper = record Helper for TFileType
    function ToString: string;
    class function Parse(const S: string): TFileType; static;
  end;

  {$SCOPEDENUMS ON}
  TThinkingType = (
    enabled,
    disabled,
    adaptive
  );

  TThinkingTypeHelper = record Helper for TThinkingType
    function ToString: string;
    class function Parse(const S: string): TThinkingType; static;
  end;

  {$SCOPEDENUMS ON}
  TSpeedType = (
    standard,
    fast
  );

  TSpeedTypeHelper = record Helper for TSpeedType
    function ToString: string;
    class function Parse(const S: string): TSpeedType; static;
  end;

{$ENDREGION}

{$REGION 'Anthropic.Chat.Responses'}

  {$SCOPEDENUMS ON}
  TContentBlockType = (
    text,
    thinking,
    redacted_thinking,
    tool_use,
    server_tool_use,
    web_search_tool_result,
    //[beta]
    web_fetch_tool_result,
    code_execution_tool_result,
    bash_code_execution_tool_result,
    text_editor_code_execution_tool_result,
    tool_search_tool_result,
    mcp_tool_use,
    mcp_tool_result,
    container_upload
  );

  TContentBlockTypeHelper = record Helper for TContentBlockType
    function ToString: string;
    class function Parse(const S: string): TContentBlockType; static;
  end;

  TContentBlockTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS ON}
  TContentSubBlockType = (
    web_search_tool_result_error,
    web_search_result,
    web_fetch_tool_result_error,
    web_fetch_result,
    code_execution_tool_result_error,
    code_execution_result,
    bash_code_execution_tool_result_error,
    bash_code_execution_result,
    text_editor_code_execution_tool_result_error,
    text_editor_code_execution_view_result,
    text_editor_code_execution_create_result,
    text_editor_code_execution_str_replace_result,
    tool_search_tool_result_error,
    tool_search_tool_search_result,
    text
  );

  TContentSubBlockTypeHelper = record Helper for TContentSubBlockType
    function ToString: string;
    class function Parse(const S: string): TContentSubBlockType; static;
  end;

  TContentSubBlockTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS OFF}
  TCitationsType = (
    char_location,
    page_location,
    content_block_location,
    web_search_result_location,
    search_result_location
  );

  TCitationsTypeHelper = record Helper for TCitationsType
    function ToString: string;
    class function Parse(const S: string): TCitationsType; static;
  end;

  TCitationsTypeInterceptor = class(TJSONInterceptorStringToString)
    function StringConverter(Data: TObject; Field: string): string; override;
    procedure StringReverter(Data: TObject; Field: string; Arg: string); override;
  end;

  {$SCOPEDENUMS ON}
  TSkillType = (
    anthropic,
    custom
  );

  TSkillTypeHelper = record Helper for TSkillType
    function ToString: string;
    class function Parse(const S: string): TSkillType; static;
  end;

  {$SCOPEDENUMS ON}
  TEffortType = (
    low,
    medium,
    high,
    max
  );

  TEffortTypeHelper = record Helper for TEffortType
    function ToString: string;
    class function Parse(const S: string): TEffortType; static;
  end;

  {$SCOPEDENUMS ON}
  TAllowedCallersType = (
    direct,
    code_execution_20250825
  );

  TAllowedCallersTypeHelper = record Helper for TAllowedCallersType
    function ToString: string;
    class function Parse(const S: string): TAllowedCallersType; static;
  end;

{$ENDREGION}

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json, Anthropic.API.JSONShield;

{ TArgsFixInterceptor }

procedure TArgsFixInterceptor.StringReverter(Data: TObject; Field, Arg: string);
begin
  RTTI
    .GetType(Data.ClassType)
    .GetField(Field)
    .SetValue(Data, TJsonPolyUnshield.Restore(Arg));
end;

{ TMessageRoleHelper }

class function TMessageRoleHelper.Parse(const S: string): TMessageRole;
begin
  Result := TEnumWire.Parse<TMessageRole>(S);
end;

function TMessageRoleHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TMessageRole>(Self);
end;

{ TStopReasonHelper }

class function TStopReasonHelper.Parse(const S: string): TStopReason;
begin
  Result := TEnumWire.Parse<TStopReason>(S);
end;

function TStopReasonHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TStopReason>(Self);
end;

{ TStopReasonInterceptor }

function TStopReasonInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TStopReason>(Data, Field);
  Result := V.ToString;
end;

procedure TStopReasonInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TStopReason.Parse(Arg);
  TRttiMemberAccess.SetValue<TStopReason>(Data, Field, V);
end;

{ TToolChoiceTypeHelper }

class function TToolChoiceTypeHelper.Parse(const S: string): TToolChoiceType;
begin
  Result := TEnumWire.Parse<TToolChoiceType>(S);
end;

function TToolChoiceTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TToolChoiceType>(Self);
end;

{ TCachingTypeHelper }

class function TCachingTypeHelper.Parse(const S: string): TCachingType;
begin
  Result := TEnumWire.Parse<TCachingType>(S);
end;

function TCachingTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TCachingType>(Self);
end;

{ TProcessingStatusTypeHelper }

class function TProcessingStatusTypeHelper.Parse(
  const S: string): TProcessingStatusType;
begin
  Result := TEnumWire.Parse<TProcessingStatusType>(S);
end;

function TProcessingStatusTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TProcessingStatusType>(Self);
end;

{ TProcessingStatusInterceptor }

function TProcessingStatusInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TProcessingStatusType>(Data, Field);
  Result := V.ToString;
end;

procedure TProcessingStatusInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TProcessingStatusType.Parse(Arg);
  TRttiMemberAccess.SetValue<TProcessingStatusType>(Data, Field, V);
end;

{ TSchemaTypeHelper }

class function TSchemaTypeHelper.Parse(const S: string): TSchemaType;
begin
 Result := TEnumWire.Parse<TSchemaType>(S);
end;

function TSchemaTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TSchemaType>(Self);
end;

{ TEventTypeHelper }

class function TEventTypeHelper.Parse(const S: string): TEventType;
begin
  Result := TEnumWire.Parse<TEventType>(S);
end;

function TEventTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TEventType>(Self);
end;

{ TEventTypeInterceptor }

function TEventTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TEventType>(Data, Field);
  Result := V.ToString;
end;

procedure TEventTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TEventType.Parse(Arg);
  TRttiMemberAccess.SetValue<TEventType>(Data, Field, V);
end;

{ TDeltaTypeHelper }

class function TDeltaTypeHelper.Parse(const S: string): TDeltaType;
begin
  Result := TEnumWire.Parse<TDeltaType>(S);
end;

function TDeltaTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TDeltaType>(Self);
end;

{ TContentBlockTypeHelper }

class function TContentBlockTypeHelper.Parse(
  const S: string): TContentBlockType;
begin
  Result := TEnumWire.Parse<TContentBlockType>(S);
end;

function TContentBlockTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TContentBlockType>(Self);
end;

{ TMessageRoleInterceptor }

function TMessageRoleInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TMessageRole>(Data, Field);
  Result := V.ToString;
end;

procedure TMessageRoleInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TMessageRole.Parse(Arg);
  TRttiMemberAccess.SetValue<TMessageRole>(Data, Field, V);
end;

{ TContentBlockTypeInterceptor }

function TContentBlockTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TContentBlockType>(Data, Field);
  Result := V.ToString;
end;

procedure TContentBlockTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TContentBlockType.Parse(Arg);
  TRttiMemberAccess.SetValue<TContentBlockType>(Data, Field, V);
end;

{ TDeltaTypeInterceptor }

function TDeltaTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TDeltaType>(Data, Field);
  Result := V.ToString;
end;

procedure TDeltaTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TDeltaType.Parse(Arg);
  TRttiMemberAccess.SetValue<TDeltaType>(Data, Field, V);
end;

{ TCitationsTypeHelper }

class function TCitationsTypeHelper.Parse(const S: string): TCitationsType;
begin
  Result := TEnumWire.Parse<TCitationsType>(S);
end;

function TCitationsTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TCitationsType>(Self);
end;

{ TCitationsTypeInterceptor }

function TCitationsTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TCitationsType>(Data, Field);
  Result := V.ToString;
end;

procedure TCitationsTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TCitationsType.Parse(Arg);
  TRttiMemberAccess.SetValue<TCitationsType>(Data, Field, V);
end;

{ TServiceTierTypeHelper }

class function TServiceTierTypeHelper.Parse(const S: string): TServiceTierType;
begin
  Result := TEnumWire.Parse<TServiceTierType>(S);
end;

function TServiceTierTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TServiceTierType>(Self);
end;

{ TContentTypeHelper }

class function TContentTypeHelper.Parse(const S: string): TContentType;
begin
  Result := TEnumWire.Parse<TContentType>(S);
end;

function TContentTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TContentType>(Self);
end;

{ TWebSearchErrorHelper }

class function TWebSearchErrorHelper.Parse(const S: string): TWebSearchError;
begin
  Result := TEnumWire.Parse<TWebSearchError>(S);
end;

function TWebSearchErrorHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TWebSearchError>(Self);
end;

{ TBetaHelper }

class function TBetaHelper.Parse(const S: string): TBeta;
begin
  Result := TEnumWire.Parse<TBeta>(S, Betas);
end;

function TBetaHelper.ToString: string;
begin
  Result := Betas[Self];
end;

{ TServerToolUseNameHelper }

class function TServerToolUseNameHelper.Parse(
  const S: string): TServerToolUseName;
begin
  Result := TEnumWire.Parse<TServerToolUseName>(S);
end;

function TServerToolUseNameHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TServerToolUseName>(Self);
end;

{ TFileTypeHelper }

class function TFileTypeHelper.Parse(const S: string): TFileType;
begin
  Result := TEnumWire.Parse<TFileType>(S);
end;

function TFileTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TFileType>(Self);
end;

{ TSkillTypeHelper }

class function TSkillTypeHelper.Parse(const S: string): TSkillType;
begin
  Result := TEnumWire.Parse<TSkillType>(S);
end;

function TSkillTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TSkillType>(Self);
end;

{ TEffortTypeHelper }

class function TEffortTypeHelper.Parse(const S: string): TEffortType;
begin
  Result := TEnumWire.Parse<TEffortType>(S);
end;

function TEffortTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TEffortType>(Self);
end;

{ TAllowedCallersTypeHelper }

class function TAllowedCallersTypeHelper.Parse(
  const S: string): TAllowedCallersType;
begin
  Result := TEnumWire.Parse<TAllowedCallersType>(S);
end;

function TAllowedCallersTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TAllowedCallersType>(Self);
end;

{ TContentSubBlockTypeHelper }

class function TContentSubBlockTypeHelper.Parse(
  const S: string): TContentSubBlockType;
begin
  Result := TEnumWire.Parse<TContentSubBlockType>(S);
end;

function TContentSubBlockTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TContentSubBlockType>(Self);
end;

{ TContentSubBlockTypeInterceptor }

function TContentSubBlockTypeInterceptor.StringConverter(Data: TObject;
  Field: string): string;
begin
  var V := TRttiMemberAccess.GetValue<TContentSubBlockType>(Data, Field);
  Result := V.ToString;
end;

procedure TContentSubBlockTypeInterceptor.StringReverter(Data: TObject; Field,
  Arg: string);
begin
  var V := TContentSubBlockType.Parse(Arg);
  TRttiMemberAccess.SetValue<TContentSubBlockType>(Data, Field, V);
end;

{ TThinkingTypeHelper }

class function TThinkingTypeHelper.Parse(const S: string): TThinkingType;
begin
  Result := TEnumWire.Parse<TThinkingType>(S);
end;

function TThinkingTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TThinkingType>(Self);
end;

{ TSpeedTypeHelper }

class function TSpeedTypeHelper.Parse(const S: string): TSpeedType;
begin
  Result := TEnumWire.Parse<TSpeedType>(S);
end;

function TSpeedTypeHelper.ToString: string;
begin
  Result := TEnumWire.ToString<TSpeedType>(Self);
end;

end.
