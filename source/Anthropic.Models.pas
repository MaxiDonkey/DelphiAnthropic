unit Anthropic.Models;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.API.Params, Anthropic.API,
  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  TListModelsParams = class(TUrlParam)
  public
    /// <summary>
    /// Number of items to return per page.
    /// </summary>
    /// <param name="Value">
    /// Defaults to 20. Ranges from 1 to 1000.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified limit.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the value is less than 1 or greater than 1000.
    /// </exception>
    function Limit(const Value: Integer): TListModelsParams;

    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results immediately after this object.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified <c>after_id</c> value.
    /// </returns>
    function AfterId(const Value: string): TListModelsParams;

    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results immediately before this object.
    /// </summary>
    /// <param name="Value">
    /// A string representing the model ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListModelsParams</c> with the specified <c>before_id</c> value.
    /// </returns>
    function BeforeId(const Value: string): TListModelsParams;
  end;

  TListModelsParamProc = TProc<TListModelsParams>;

  /// <summary>
  /// Represents a simple capability support flag returned by the Models API.
  /// </summary>
  TModelCapabilitySupport = class(TJSONFingerprint)
  private
    FSupported: Boolean;
  public
    /// <summary>
    /// Indicates whether this capability is supported by the model.
    /// </summary>
    property Supported: Boolean read FSupported write FSupported;
  end;

  /// <summary>
  /// Represents supported thinking type configurations for a model.
  /// </summary>
  TModelThinkingTypes = class(TJSONFingerprint)
  private
    FAdaptive: TModelCapabilitySupport;
    FEnabled: TModelCapabilitySupport;
  public
    /// <summary>
    /// Indicates whether thinking with type <c>adaptive</c> is supported.
    /// </summary>
    property Adaptive: TModelCapabilitySupport read FAdaptive write FAdaptive;

    /// <summary>
    /// Indicates whether thinking with type <c>enabled</c> is supported.
    /// </summary>
    property Enabled: TModelCapabilitySupport read FEnabled write FEnabled;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents thinking capability support and its available type configurations.
  /// </summary>
  TModelThinkingCapability = class(TJSONFingerprint)
  private
    FSupported: Boolean;
    FTypes: TModelThinkingTypes;
  public
    /// <summary>
    /// Indicates whether thinking is supported by the model.
    /// </summary>
    property Supported: Boolean read FSupported write FSupported;

    /// <summary>
    /// Supported thinking type configurations.
    /// </summary>
    property Types: TModelThinkingTypes read FTypes write FTypes;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents reasoning effort support and the available effort levels.
  /// </summary>
  TModelEffortCapability = class(TJSONFingerprint)
  private
    FSupported: Boolean;
    FLow: TModelCapabilitySupport;
    FMedium: TModelCapabilitySupport;
    FHigh: TModelCapabilitySupport;
    FMax: TModelCapabilitySupport;
    [JsonNameAttribute('xhigh')]
    FXHigh: TModelCapabilitySupport;
  public
    /// <summary>
    /// Indicates whether reasoning effort is supported by the model.
    /// </summary>
    property Supported: Boolean read FSupported write FSupported;

    /// <summary>
    /// Indicates whether the low effort level is supported.
    /// </summary>
    property Low: TModelCapabilitySupport read FLow write FLow;

    /// <summary>
    /// Indicates whether the medium effort level is supported.
    /// </summary>
    property Medium: TModelCapabilitySupport read FMedium write FMedium;

    /// <summary>
    /// Indicates whether the high effort level is supported.
    /// </summary>
    property High: TModelCapabilitySupport read FHigh write FHigh;

    /// <summary>
    /// Indicates whether the max effort level is supported.
    /// </summary>
    property Max: TModelCapabilitySupport read FMax write FMax;

    /// <summary>
    /// Indicates whether the xhigh effort level is supported.
    /// </summary>
    property XHigh: TModelCapabilitySupport read FXHigh write FXHigh;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents context management support and the available context management strategies.
  /// </summary>
  TModelContextManagementCapability = class(TJSONFingerprint)
  private
    FSupported: Boolean;
    [JsonNameAttribute('clear_thinking_20251015')]
    FClearThinking20251015: TModelCapabilitySupport;
    [JsonNameAttribute('clear_tool_uses_20250919')]
    FClearToolUses20250919: TModelCapabilitySupport;
    [JsonNameAttribute('compact_20260112')]
    FCompact20260112: TModelCapabilitySupport;
  public
    /// <summary>
    /// Indicates whether context management is supported by the model.
    /// </summary>
    property Supported: Boolean read FSupported write FSupported;

    /// <summary>
    /// Indicates whether the <c>clear_thinking_20251015</c> strategy is supported.
    /// </summary>
    property ClearThinking20251015: TModelCapabilitySupport read FClearThinking20251015 write FClearThinking20251015;

    /// <summary>
    /// Indicates whether the <c>clear_tool_uses_20250919</c> strategy is supported.
    /// </summary>
    property ClearToolUses20250919: TModelCapabilitySupport read FClearToolUses20250919 write FClearToolUses20250919;

    /// <summary>
    /// Indicates whether the <c>compact_20260112</c> strategy is supported.
    /// </summary>
    property Compact20260112: TModelCapabilitySupport read FCompact20260112 write FCompact20260112;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents the complete model capability information returned by the Models API.
  /// </summary>
  TModelCapabilities = class(TJSONFingerprint)
  private
    FBatch: TModelCapabilitySupport;
    FCitations: TModelCapabilitySupport;
    [JsonNameAttribute('code_execution')]
    FCodeExecution: TModelCapabilitySupport;
    [JsonNameAttribute('context_management')]
    FContextManagement: TModelContextManagementCapability;
    FEffort: TModelEffortCapability;
    [JsonNameAttribute('image_input')]
    FImageInput: TModelCapabilitySupport;
    [JsonNameAttribute('pdf_input')]
    FPdfInput: TModelCapabilitySupport;
    [JsonNameAttribute('structured_outputs')]
    FStructuredOutputs: TModelCapabilitySupport;
    FThinking: TModelThinkingCapability;
  public
    /// <summary>
    /// Whether the model supports the Batch API.
    /// </summary>
    property Batch: TModelCapabilitySupport read FBatch write FBatch;

    /// <summary>
    /// Whether the model supports citation generation.
    /// </summary>
    property Citations: TModelCapabilitySupport read FCitations write FCitations;

    /// <summary>
    /// Whether the model supports code execution tools.
    /// </summary>
    property CodeExecution: TModelCapabilitySupport read FCodeExecution write FCodeExecution;

    /// <summary>
    /// Context management support and available strategies.
    /// </summary>
    property ContextManagement: TModelContextManagementCapability read FContextManagement write FContextManagement;

    /// <summary>
    /// Effort support and available reasoning effort levels.
    /// </summary>
    property Effort: TModelEffortCapability read FEffort write FEffort;

    /// <summary>
    /// Whether the model accepts image content blocks.
    /// </summary>
    property ImageInput: TModelCapabilitySupport read FImageInput write FImageInput;

    /// <summary>
    /// Whether the model accepts PDF content blocks.
    /// </summary>
    property PdfInput: TModelCapabilitySupport read FPdfInput write FPdfInput;

    /// <summary>
    /// Whether the model supports structured output, JSON mode, or strict tool schemas.
    /// </summary>
    property StructuredOutputs: TModelCapabilitySupport read FStructuredOutputs write FStructuredOutputs;

    /// <summary>
    /// Thinking capability and supported type configurations.
    /// </summary>
    property Thinking: TModelThinkingCapability read FThinking write FThinking;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents a model descriptor returned by the Models API.
  /// </summary>
  TModel = class(TJSONFingerprint)
  private
    FType: string;
    FId: string;
    FCapabilities: TModelCapabilities;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('display_name')]
    FDisplayName: string;
    [JsonNameAttribute('max_input_tokens')]
    FMaxInputTokens: Int64;
    [JsonNameAttribute('max_tokens')]
    FMaxTokens: Int64;
  public
    /// <summary>
    /// For Models, this is always "model".
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Unique model identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Model capability information.
    /// </summary>
    property Capabilities: TModelCapabilities read FCapabilities write FCapabilities;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which the model was released.
    /// May be set to an epoch value if the release date is unknown.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// A human-readable name for the model.
    /// </summary>
    property DisplayName: string read FDisplayName write FDisplayName;

    /// <summary>
    /// Maximum input context window size in tokens for this model.
    /// </summary>
    property MaxInputTokens: Int64 read FMaxInputTokens write FMaxInputTokens;

    /// <summary>
    /// Maximum value for the <c>max_tokens</c> parameter when using this model.
    /// </summary>
    property MaxTokens: Int64 read FMaxTokens write FMaxTokens;

    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TModels</c> class represents a collection of model entities retrieved from the Anthropic API.
  /// It encapsulates the list of models and additional metadata for pagination purposes.
  /// </summary>
  /// <remarks>
  /// This class is designed to manage and store a list of <c>TModel</c> objects, along with
  /// information to facilitate navigation through paginated results.
  /// <para>
  /// The <c>Data</c> property contains the array of models retrieved in the current query.
  /// The <c>HasMore</c> property indicates whether there are additional results beyond the current page.
  /// The <c>FirstId</c> and <c>LastId</c> properties provide cursors for navigating to the previous
  /// and next pages of results, respectively.
  /// </para>
  /// <para>
  /// When an instance of this class is destroyed, it ensures proper memory cleanup
  /// by freeing the individual <c>TModel</c> objects in the <c>Data</c> array.
  /// </para>
  /// </remarks>
  TModels = class(TJSONFingerprint)
  private
    FData: TArray<TModel>;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('first_id')]
    FFirstId: string;
    [JsonNameAttribute('last_id')]
    FLastId: string;
  public
    /// <summary>
    /// The array of models retrieved in the current query.
    /// </summary>
    property Data: TArray<TModel> read FData write FData;

    /// <summary>
    /// Indicates if there are more results in the requested page direction.
    /// </summary>
    property HasMore: Boolean read FHasMore write FHasMore;

    /// <summary>
    /// First ID in the data list. Can be used as the before_id for the previous page.
    /// </summary>
    property FirstId: string read FFirstId write FFirstId;

    /// <summary>
    /// Last ID in the data list. Can be used as the after_id for the next page.
    /// </summary>
    property LastId: string read FLastId write FLastId;

    destructor Destroy; override;
  end;

  /// <summary>
  /// Defines an asynchronous callback handler for model retrieval responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsynModel</c> is a type alias for <c>TAsynCallBack&lt;TModel&gt;</c>, specialized for handling
  /// model metadata returned by the Models endpoints.
  /// </para>
  /// <para>
  /// It binds the generic asynchronous callback infrastructure to the Models domain by fixing the
  /// callback payload type to <c>TModel</c>.
  /// </para>
  /// <para>
  /// This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TModel&gt;</c>.
  /// </para>
  /// <para>
  /// Use <c>TAsynModel</c> when registering callbacks that retrieve a specific model by ID or alias
  /// without blocking the calling thread.
  /// </para>
  /// </remarks>
  TAsynModel = TAsynCallBack<TModel>;

  /// <summary>
  /// Defines a promise-based callback handler for model retrieval responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseModel</c> is a type alias for <c>TPromiseCallback&lt;TModel&gt;</c>, specialized for
  /// consuming model metadata returned by the Models endpoints.
  /// </para>
  /// <para>
  /// It binds the generic promise-based callback infrastructure to the Models domain by fixing the
  /// callback payload type to <c>TModel</c>.
  /// </para>
  /// <para>
  /// This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TModel&gt;</c>.
  /// </para>
  /// <para>
  /// Use <c>TPromiseModel</c> when retrieving a specific model through promise-based abstractions
  /// instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseModel = TPromiseCallback<TModel>;

  /// <summary>
  /// Defines an asynchronous callback handler for model list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TAsynModels</c> is a type alias for <c>TAsynCallBack&lt;TModels&gt;</c>, specialized for handling
  /// paginated collections of models returned by the Models listing endpoint.
  /// </para>
  /// <para>
  /// It binds the generic asynchronous callback infrastructure to the Models domain by fixing the
  /// callback payload type to <c>TModels</c>.
  /// </para>
  /// <para>
  /// This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TModels&gt;</c>.
  /// </para>
  /// <para>
  /// Use <c>TAsynModels</c> when registering callbacks that list available models without blocking
  /// the calling thread.
  /// </para>
  /// </remarks>
  TAsynModels = TAsynCallBack<TModels>;

  /// <summary>
  /// Defines a promise-based callback handler for model list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// <c>TPromiseModels</c> is a type alias for <c>TPromiseCallback&lt;TModels&gt;</c>, specialized for
  /// consuming paginated collections of models returned by the Models listing endpoint.
  /// </para>
  /// <para>
  /// It binds the generic promise-based callback infrastructure to the Models domain by fixing the
  /// callback payload type to <c>TModels</c>.
  /// </para>
  /// <para>
  /// This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TModels&gt;</c>.
  /// </para>
  /// <para>
  /// Use <c>TPromiseModels</c> when listing available models through promise-based abstractions
  /// instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseModels = TPromiseCallback<TModels>;

  TAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function List: TModels; overload; virtual; abstract;

    function List(const ParamProc: TListModelsParamProc): TModels; overload; virtual; abstract;

    function Retrieve(const ModelId: string): TModel; virtual; abstract;
  end;

  TAsynchronousSupport = class(TAbstractSupport)
  protected
    procedure AsynList(CallBacks: TFunc<TAsynModels>); overload;

    procedure AsynList(ParamProc: TListModelsParamProc; CallBacks: TFunc<TAsynModels>); overload;

    procedure AsynRetrieve(const ModelId: string; CallBacks: TFunc<TAsynModel>);
  end;

  TModelsRoute = class(TAsynchronousSupport)
    /// <summary>
    /// Retrieves the list of available models.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method performs a blocking request to the Models listing endpoint and returns a
    /// <c>TModels</c> collection containing the available models.
    /// </para>
    /// <para>
    /// Models are returned in reverse chronological order, with the most recently released models
    /// listed first.
    /// </para>
    /// <para>
    /// The returned object includes both the model data and pagination metadata, such as
    /// <c>HasMore</c>, <c>FirstId</c>, and <c>LastId</c>.
    /// </para>
    /// <para>
    /// For paginated access or non-blocking usage, prefer the overloaded <c>List</c> method with
    /// parameters, <c>AsynList</c>, or <c>AsyncAwaitList</c>.
    /// </para>
    /// </remarks>
    function List: TModels; overload; override;

    /// <summary>
    /// Retrieves a paginated list of available models using query parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method performs a blocking request to the Models listing endpoint and returns a
    /// <c>TModels</c> collection filtered and paginated according to the provided parameters.
    /// </para>
    /// <para>
    /// The <c>ParamProc</c> callback is used to configure pagination options such as <c>limit</c>,
    /// <c>after_id</c>, and <c>before_id</c>.
    /// </para>
    /// <para>
    /// The returned object includes both the model data and pagination metadata, enabling cursor-
    /// based navigation through result pages.
    /// </para>
    /// <para>
    /// For non-blocking usage, prefer <c>AsynList</c> or <c>AsyncAwaitList</c>.
    /// </para>
    /// </remarks>
    function List(const ParamProc: TListModelsParamProc): TModels; overload; override;

    /// <summary>
    /// Retrieves metadata for a specific model.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method performs a blocking request to the Models retrieval endpoint and returns a
    /// <c>TModel</c> instance describing the specified model.
    /// </para>
    /// <para>
    /// The <c>ModelId</c> parameter may be a concrete model identifier or a model alias, which will
    /// be resolved to the corresponding model ID by the API.
    /// </para>
    /// <para>
    /// On success, the returned object contains the model identifier, display name, release time,
    /// token limits, capabilities, and type metadata as provided by the API.
    /// </para>
    /// <para>
    /// For non-blocking usage, prefer <c>AsynRetrieve</c> or <c>AsyncAwaitRetrieve</c>.
    /// </para>
    /// </remarks>
    function Retrieve(const ModelId: string): TModel; override;

    /// <summary>
    /// Retrieves the list of available models using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method wraps the asynchronous model listing workflow into a
    /// <c>TPromise&lt;TModels&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// The returned promise resolves with a <c>TModels</c> collection containing the available
    /// models and pagination metadata.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseModels> = nil): TPromise<TModels>; overload;

    /// <summary>
    /// Retrieves a paginated list of available models using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method wraps the asynchronous model listing workflow into a
    /// <c>TPromise&lt;TModels&gt;</c>, enabling async/await-style consumption with pagination support.
    /// </para>
    /// <para>
    /// The <c>ParamProc</c> callback is used to configure pagination options such as <c>limit</c>,
    /// <c>after_id</c>, and <c>before_id</c>.
    /// </para>
    /// <para>
    /// The optional <c>Callbacks</c> factory allows observation of lifecycle events while the
    /// paginated model collection is returned through the promise result.
    /// </para>
    /// <para>
    /// The returned promise resolves with a <c>TModels</c> collection filtered according to the
    /// provided parameters or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const ParamProc: TListModelsParamProc;
      const Callbacks: TFunc<TPromiseModels> = nil): TPromise<TModels>; overload;

    /// <summary>
    /// Retrieves metadata for a specific model using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This method wraps the asynchronous model retrieval workflow into a
    /// <c>TPromise&lt;TModel&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// The <c>ModelId</c> parameter may be a concrete model identifier or a model alias, which will
    /// be resolved to the corresponding model ID by the API.
    /// </para>
    /// <para>
    /// The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// The returned promise resolves with a <c>TModel</c> instance describing the requested model
    /// or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitRetrieve(
      const ModelId: string;
      const Callbacks: TFunc<TPromiseModel> = nil): TPromise<TModel>;
  end;

implementation

{ TModelThinkingTypes }

destructor TModelThinkingTypes.Destroy;
begin
  FAdaptive.Free;
  FEnabled.Free;
  inherited;
end;

{ TModelThinkingCapability }

destructor TModelThinkingCapability.Destroy;
begin
  FTypes.Free;
  inherited;
end;

{ TModelEffortCapability }

destructor TModelEffortCapability.Destroy;
begin
  FLow.Free;
  FMedium.Free;
  FHigh.Free;
  FMax.Free;
  FXHigh.Free;
  inherited;
end;

{ TModelContextManagementCapability }

destructor TModelContextManagementCapability.Destroy;
begin
  FClearThinking20251015.Free;
  FClearToolUses20250919.Free;
  FCompact20260112.Free;
  inherited;
end;

{ TModelCapabilities }

destructor TModelCapabilities.Destroy;
begin
  FBatch.Free;
  FCitations.Free;
  FCodeExecution.Free;
  FContextManagement.Free;
  FEffort.Free;
  FImageInput.Free;
  FPdfInput.Free;
  FStructuredOutputs.Free;
  FThinking.Free;
  inherited;
end;

{ TModel }

destructor TModel.Destroy;
begin
  FCapabilities.Free;
  inherited;
end;

{ TModels }

destructor TModels.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TListModelsParams }

function TListModelsParams.AfterId(const Value: string): TListModelsParams;
begin
  Result := TListModelsParams(Add('after_id', Value));
end;

function TListModelsParams.BeforeId(const Value: string): TListModelsParams;
begin
  Result := TListModelsParams(Add('before_id', Value));
end;

function TListModelsParams.Limit(const Value: Integer): TListModelsParams;
begin
  if (Value < 1) or (Value > 1000) then
    raise Exception.Create('The models list limit must be between 1 and 1000.');

  Result := TListModelsParams(Add('limit', Value));
end;

{ TModelsRoute }

function TModelsRoute.List: TModels;
begin
  Result := API.Get<TModels>('models');
end;

function TModelsRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseModels>): TPromise<TModels>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TModels>(
    procedure(const CallbackParams: TFunc<TAsynModels>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TModelsRoute.AsyncAwaitList(const ParamProc: TListModelsParamProc;
  const Callbacks: TFunc<TPromiseModels>): TPromise<TModels>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TModels>(
    procedure(const CallbackParams: TFunc<TAsynModels>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TModelsRoute.AsyncAwaitRetrieve(const ModelId: string;
  const Callbacks: TFunc<TPromiseModel>): TPromise<TModel>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TModel>(
    procedure(const CallbackParams: TFunc<TAsynModel>)
    begin
      Self.AsynRetrieve(ModelId, CallbackParams);
    end,
    Callbacks);
end;

function TModelsRoute.List(const ParamProc: TListModelsParamProc): TModels;
begin
  Result := API.Get<TModels, TListModelsParams>('models', ParamProc);
end;

function TModelsRoute.Retrieve(const ModelId: string): TModel;
begin
  Result := API.Get<TModel>('models/' + ModelId);
end;

{ TAsynchronousSupport }

procedure TAsynchronousSupport.AsynList(CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(ParamProc: TListModelsParamProc;
  CallBacks: TFunc<TAsynModels>);
begin
  with TAsynCallBackExec<TAsynModels, TModels>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModels
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(const ModelId: string;
  CallBacks: TFunc<TAsynModel>);
begin
  with TAsynCallBackExec<TAsynModel, TModel>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TModel
      begin
        Result := Self.Retrieve(ModelId);
      end);
  finally
    Free;
  end;
end;

end.
