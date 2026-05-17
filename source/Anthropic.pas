unit Anthropic;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.Net.URLClient,
  Anthropic.API, Anthropic.API.Params, Anthropic.API.MultiFormData,
  Anthropic.HttpClientInterface, Anthropic.Chat,
  Anthropic.Batches, Anthropic.Models, Anthropic.Batches.Support, Anthropic.Functions.Core,
  Anthropic.Schema, Anthropic.Chat.Responses, Anthropic.Chat.Request, Anthropic.Chat.StreamEvents,
  Anthropic.Monitoring, Anthropic.Net.MediaCodec, Anthropic.Chat.StreamEngine,
  Anthropic.Chat.StreamCallbacks, Anthropic.Chat.Beta, Anthropic.API.JsonSafeReader,
  Anthropic.Files, Anthropic.Skills, Anthropic.JSONL, Anthropic.Context.Helper,
  Anthropic.Agents, Anthropic.Environment, Anthropic.Sessions, Anthropic.Vaults,
  Anthropic.MemoryStore, Anthropic.Files.Helper;

const
  VERSION = '1.3.0';

type
  /// <summary>
  /// Defines the primary interface for interacting with the Anthropic API.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>IAnthropic</c> represents the main entry point of the client library and provides structured
  /// access to all supported Anthropic API domains.
  /// </para>
  /// <para>
  /// • It exposes route-specific properties (such as <c>Chat</c>, <c>Models</c>, <c>Files</c>,
  /// <c>Batch</c>, <c>Skills</c>, etc.) that encapsulate related operations behind cohesive route
  /// abstractions.
  /// </para>
  /// <para>
  /// • Each route property is typically lazily instantiated and shares the same underlying
  /// <c>TAnthropicAPI</c> configuration, including authentication credentials and base URL.
  /// </para>
  /// <para>
  /// • This interface is designed to be consumed as a long-lived service object, promoting clear
  /// separation of concerns, testability, and consistent API usage patterns.
  /// </para>
  /// </remarks>
  IAnthropic = interface
    ['{ECAF057E-EFB9-4615-BCEB-174391FB4F49}']
    function GetAPI: TAnthropicAPI;
    function GetHttpClient: IHttpClientAPI;
    procedure SetToken(const Value: string);
    function GetToken: string;
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetChatRoute: TChatRoute;
    function GetBatchRoute: TBatchRoute;
    function GetFilesRoute: TFilesRoute;
    function GetModelsRoute : TModelsRoute;
    function GetSkillsRoute : TSkillsRoute;
    function GetAgentsRoute: TAgentsRoute;
    function GetEnvironmentsRoute: TEnvironmentsRoute;
    function GetSessionsRoute: TSessionsRoute;
    function GetVaultsRoute: TVaultsRoute;
    function GetMemoryStoresRoute: TMemoryStoresRoute;

    /// <summary>
    /// Provides access to the chat API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TChatRoute</c> entry point for chat operations such as message
    /// creation, streaming message creation, and token counting.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access chat-related operations through a single, centralized client
    /// instance.
    /// </para>
    /// </remarks>
    property Chat: TChatRoute read GetChatRoute;

    /// <summary>
    /// Provides access to the managed agents API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TAgentsRoute</c> entry point for managed agent operations such as
    /// create, retrieve, list, update, archive, and version listing.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access managed-agent-related operations through a single, centralized
    /// client instance.
    /// </para>
    /// </remarks>
    property Agents: TAgentsRoute read GetAgentsRoute;

    /// <summary>
    /// Provides access to the managed environments API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TEnvironmentsRoute</c> entry point for managed environment operations such as
    /// create, retrieve, list, update, delete, and archive.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access managed-environment-related operations through a single, centralized
    /// client instance.
    /// </para>
    /// </remarks>
    property Environments: TEnvironmentsRoute read GetEnvironmentsRoute;

    /// <summary>
    /// Provides access to the managed sessions API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TSessionsRoute</c> entry point for managed session operations such as
    /// create, retrieve, list, update, delete, and archive.
    /// </para>
    /// <para>
    /// • The route also exposes nested routes for session events, resources, threads, and thread events.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access session-related operations through a single, centralized client instance.
    /// </para>
    /// </remarks>
    property Sessions: TSessionsRoute read GetSessionsRoute;

    /// <summary>
    /// Provides access to the managed vaults API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TVaultsRoute</c> entry point for managed vault operations such as
    /// create, retrieve, list, update, delete, and archive.
    /// </para>
    /// <para>
    /// • The route also exposes the nested <c>Credentials</c> route for credential operations scoped to a vault.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access managed-vault-related operations through a single, centralized
    /// client instance.
    /// </para>
    /// </remarks>
    property Vaults: TVaultsRoute read GetVaultsRoute;

    /// <summary>
    /// Provides access to the managed memory stores API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TMemoryStoresRoute</c> entry point for memory store operations such as
    /// create, retrieve, list, update, delete, and archive.
    /// </para>
    /// <para>
    /// • The route also exposes the nested <c>Memories</c> and <c>MemoryVersions</c> routes for operations
    /// scoped to a memory store.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access managed-memory-store-related operations through a single, centralized
    /// client instance.
    /// </para>
    /// </remarks>
    property MemoryStores: TMemoryStoresRoute read GetMemoryStoresRoute;

    /// <summary>
    /// Provides access to the batches API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TBatchRoute</c> entry point for message batch operations such as
    /// create, retrieve, list, cancel, and delete.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access batch-related operations through a single, centralized client
    /// instance.
    /// </para>
    /// </remarks>
    property Batch: TBatchRoute read GetBatchRoute;

    /// <summary>
    /// Provides access to the files API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TFilesRoute</c> entry point for file operations such as upload,
    /// retrieve, list, and delete.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access file-related operations through a single, centralized client
    /// instance.
    /// </para>
    /// </remarks>
    property Files: TFilesRoute read GetFilesRoute;

    /// <summary>
    /// Provides access to the models API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TModelsRoute</c> entry point for model operations such as listing
    /// available models and retrieving model metadata by identifier or alias.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access model-related operations through a single, centralized client
    /// instance.
    /// </para>
    /// </remarks>
    property Models: TModelsRoute read GetModelsRoute;

    /// <summary>
    /// Provides access to the skills API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>TSkillsRoute</c> entry point for skill management operations such as
    /// create, retrieve, list, and delete.
    /// </para>
    /// <para>
    /// • The route instance is created lazily on first access and reused for subsequent calls.
    /// </para>
    /// <para>
    /// • The returned route shares the same underlying <c>TAnthropicAPI</c> instance and therefore uses
    /// the current authentication token and base URL configuration.
    /// </para>
    /// <para>
    /// • Use this property to access skill-related operations through a single, centralized client
    /// instance.
    /// </para>
    /// </remarks>
    property Skills: TSkillsRoute read GetSkillsRoute;

    /// <summary>
    /// Provides access to the underlying API client used to issue requests.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the shared <c>TAnthropicAPI</c> instance used internally by all route
    /// objects.
    /// </para>
    /// <para>
    /// • It provides direct access to low-level request methods (GET/POST/DELETE, multipart uploads,
    /// deserialization, and header construction) when route-level abstractions are not sufficient.
    /// </para>
    /// <para>
    /// • The returned instance reflects the current configuration (token, base URL, headers, and HTTP
    /// transport template) and is intended to be long-lived.
    /// </para>
    /// <para>
    /// • Use this property for advanced scenarios such as custom routing, diagnostics, or integration
    /// with auxiliary infrastructure that requires the raw API client.
    /// </para>
    /// </remarks>
    property API: TAnthropicAPI read GetAPI;

    /// <summary>
    /// Provides access to the underlying HTTP client implementation used by the API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>IHttpClientAPI</c> instance used internally to execute all HTTP
    /// requests.
    /// </para>
    /// <para>
    /// • It reflects the active HTTP transport configured on the shared <c>TAnthropicAPI</c> instance.
    /// </para>
    /// <para>
    /// • Use this property when you need to customize transport behavior or integrate monitoring,
    /// middleware, or client-specific options.
    /// </para>
    /// <para>
    /// • The returned reference is shared across all routes and remains valid for the lifetime of the
    /// owning Anthropic client instance.
    /// </para>
    /// </remarks>
    property HttpClient: IHttpClientAPI read GetHttpClient;

    /// <summary>
    /// Sets or retrieves the API token used for authentication.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property holds the API key sent with each request to authenticate against the Anthropic API.
    /// </para>
    /// <para>
    /// • Updating this value affects all subsequent requests issued by the client and its route objects.
    /// </para>
    /// <para>
    /// • The token must be a non-empty string; otherwise request execution will fail during validation.
    /// </para>
    /// <para>
    /// • Use this property to rotate credentials or defer token assignment until after client creation.
    /// </para>
    /// </remarks>
    property Token: string read GetToken write SetToken;

    /// <summary>
    /// Sets or retrieves the base URL used for all API requests.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property defines the root endpoint used to construct request URLs for all API calls.
    /// </para>
    /// <para>
    /// • The default value is <c>https://api.anthropic.com/v1</c>.
    /// </para>
    /// <para>
    /// • Updating this value affects all subsequent requests issued by the client and its route objects.
    /// </para>
    /// <para>
    /// • Use this property to target alternative endpoints, such as proxies, gateways, or test
    /// environments.
    /// </para>
    /// </remarks>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;
  end;

  /// <summary>
  /// Factory class responsible for creating and configuring Anthropic client instances.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAnthropicFactory</c> centralizes the instantiation logic for Anthropic clients, ensuring
  /// consistent configuration and initialization across the application.
  /// </para>
  /// <para>
  /// • It typically encapsulates concerns such as API key injection, base URL selection, HTTP
  /// client configuration, and default headers.
  /// </para>
  /// <para>
  /// • The factory may expose one or more creation methods that return an <c>IAnthropic</c>
  /// interface, allowing consumers to remain decoupled from concrete implementation classes.
  /// </para>
  /// <para>
  /// • Use this class to obtain fully initialized Anthropic client instances instead of creating
  /// them directly, promoting consistency, testability, and easier future evolution of the
  /// client construction process.
  /// </para>
  TAnthropicFactory = class
    /// <summary>
    /// Creates an instance of the <see cref="IAnthropic"/> interface with the specified API token
    /// and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with Anthropic API services.
    /// </param>
    /// <param name="Option">
    /// An optional header configuration of type <see cref="THeaderOption"/> to customize the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <returns>
    /// An instance of <see cref="IAnthropic"/> initialized with the provided API token and header option.
    /// </returns>
    class function CreateInstance(const AToken: string): IAnthropic;
  end;

  TLazyRouteFactory = class(TInterfacedObject)
  protected
    FChatLock: TObject;
    FBatchLock: TObject;
    FFilesLock: TObject;
    FModelsLock: TObject;
    FSkillsLock: TObject;
    FAgentsLock: TObject;
    FEnvironmentsLock: TObject;
    FSessionsLock: TObject;
    FVaultsLock: TObject;
    FMemoryStoresLock: TObject;

    function Lazy<T: class>(var AField: T; const ALock: TObject;
      const AFactory: TFunc<T>): T; inline;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Concrete implementation of the Anthropic client, providing access to all API routes.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAnthropic</c> is the main entry point for interacting with the Anthropic API.
  /// </para>
  /// <para>
  /// • It implements the <c>IAnthropic</c> interface, ensuring a stable and decoupled public
  /// contract for consumers.
  /// </para>
  /// <para>
  /// • By inheriting from <c>TLazyRouteFactory</c>, this class lazily instantiates route objects
  /// (such as <c>Chat</c>, <c>Models</c>, <c>Batches</c>, <c>Skills</c>, etc.) only when they are first
  /// accessed, minimizing overhead and improving startup performance.
  /// </para>
  /// <para>
  /// • Each route instance returned by this class shares the same underlying configuration
  /// (API key, base URL, HTTP client, middleware), guaranteeing consistent behavior across all
  /// API calls.
  /// </para>
  /// <para>
  /// • Typical usage involves creating an instance via <c>TAnthropicFactory</c> and then accessing
  /// the desired route through the corresponding property exposed by <c>IAnthropic</c>.
  /// </para>
  /// </remarks>
  TAnthropic = class(TLazyRouteFactory, IAnthropic)
  private
    FAPI: TAnthropicAPI;

    FChatRoute: TChatRoute;
    FBatchRoute: TBatchRoute;
    FFilesRoute: TFilesRoute;
    FModelsRoute: TModelsRoute;
    FSkillsRoute: TSkillsRoute;
    FAgentsRoute: TAgentsRoute;
    FEnvironmentsRoute: TEnvironmentsRoute;
    FSessionsRoute: TSessionsRoute;
    FVaultsRoute: TVaultsRoute;
    FMemoryStoresRoute: TMemoryStoresRoute;

    function GetAPI: TAnthropicAPI;
    function GetToken: string;
    procedure SetToken(const Value: string);
    function GetBaseUrl: string;
    procedure SetBaseUrl(const Value: string);
    function GetHttpClient: IHttpClientAPI;

    function GetChatRoute: TChatRoute;
    function GetBatchRoute: TBatchRoute;
    function GetFilesRoute: TFilesRoute;
    function GetModelsRoute : TModelsRoute;
    function GetSkillsRoute : TSkillsRoute;
    function GetAgentsRoute: TAgentsRoute;
    function GetEnvironmentsRoute: TEnvironmentsRoute;
    function GetSessionsRoute: TSessionsRoute;
    function GetVaultsRoute: TVaultsRoute;
    function GetMemoryStoresRoute: TMemoryStoresRoute;

  public
    /// <summary>
    /// Provides access to the underlying API client used to issue requests.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the shared <c>TAnthropicAPI</c> instance used internally by all route
    /// objects.
    /// </para>
    /// <para>
    /// • It provides direct access to low-level request methods (GET/POST/DELETE, multipart uploads,
    /// deserialization, and header construction) when route-level abstractions are not sufficient.
    /// </para>
    /// <para>
    /// • The returned instance reflects the current configuration (token, base URL, headers, and HTTP
    /// transport template) and is intended to be long-lived.
    /// </para>
    /// <para>
    /// • Use this property for advanced scenarios such as custom routing, diagnostics, or integration
    /// with auxiliary infrastructure that requires the raw API client.
    /// </para>
    /// </remarks>
    property API: TAnthropicAPI read GetAPI;

    /// <summary>
    /// Provides access to the underlying HTTP client implementation used by the API.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property exposes the <c>IHttpClientAPI</c> instance used internally to execute all HTTP
    /// requests.
    /// </para>
    /// <para>
    /// • It reflects the active HTTP transport configured on the shared <c>TAnthropicAPI</c> instance.
    /// </para>
    /// <para>
    /// • Use this property when you need to customize transport behavior or integrate monitoring,
    /// middleware, or client-specific options.
    /// </para>
    /// <para>
    /// • The returned reference is shared across all routes and remains valid for the lifetime of the
    /// owning Anthropic client instance.
    /// </para>
    /// </remarks>
    property HttpClient: IHttpClientAPI read GetHttpClient;

    /// <summary>
    /// Sets or retrieves the API token used for authentication.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property holds the API key sent with each request to authenticate against the Anthropic API.
    /// </para>
    /// <para>
    /// • Updating this value affects all subsequent requests issued by the client and its route objects.
    /// </para>
    /// <para>
    /// • The token must be a non-empty string; otherwise request execution will fail during validation.
    /// </para>
    /// <para>
    /// • Use this property to rotate credentials or defer token assignment until after client creation.
    /// </para>
    /// </remarks>
    property Token: string read GetToken write SetToken;

    /// <summary>
    /// Sets or retrieves the base URL used for all API requests.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This property defines the root endpoint used to construct request URLs for all API calls.
    /// </para>
    /// <para>
    /// • The default value is <c>https://api.anthropic.com/v1</c>.
    /// </para>
    /// <para>
    /// • Updating this value affects all subsequent requests issued by the client and its route objects.
    /// </para>
    /// <para>
    /// • Use this property to target alternative endpoints, such as proxies, gateways, or test
    /// environments.
    /// </para>
    /// </remarks>
    property BaseURL: string read GetBaseUrl write SetBaseUrl;

    /// <summary>
    /// Initializes a new instance of the <see cref="TAnthropic"/> class with optional header configuration.
    /// </summary>
    /// <remarks>
    /// This constructor is typically used when no API token is provided initially.
    /// The token can be set later via the <see cref="Token"/> property.
    /// </remarks>
    constructor Create; overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="TAnthropic"/> class with the provided API token and optional header configuration.
    /// </summary>
    /// <param name="AToken">
    /// The API token as a string, required for authenticating with the Anthropic AI API.
    /// </param>
    /// <param name="Option">
    /// An optional parameter of type <see cref="THeaderOption"/> to configure the request headers.
    /// The default value is <c>THeaderOption.none</c>.
    /// </param>
    /// <remarks>
    /// This constructor allows the user to specify an API token at the time of initialization.
    /// </remarks>
    constructor Create(const AToken: string); overload;

    /// <summary>
    /// Releases all resources used by the current instance of the <see cref="TAnthropic"/> class.
    /// </summary>
    /// <remarks>
    /// This method is called to clean up any resources before the object is destroyed.
    /// It overrides the base <see cref="TInterfacedObject.Destroy"/> method.
    /// </remarks>
    destructor Destroy; override;
  end;

{$REGION 'Anthropic.API'}

  TAnthropicSettings = Anthropic.API.TAnthropicSettings;
  TApiHttpHandler = Anthropic.API.TApiHttpHandler;
  TApiDeserializer = Anthropic.API.TApiDeserializer;
  TAnthropicAPI = Anthropic.API.TAnthropicAPI;
  TAnthropicAPIRoute = Anthropic.API.TAnthropicAPIRoute;

{$ENDREGION}

{$REGION 'Anthropic.API.Params'}

  TUrlParam = Anthropic.API.Params.TUrlParam;
  TJSONFingerprint = Anthropic.API.Params.TJSONFingerprint;
  TOptionalContent = Anthropic.API.Params.TOptionalContent;
  TJSONParam = Anthropic.API.Params.TJSONParam;

{$ENDREGION}

{$REGION 'Anthropic.Files.Helper'}

  TFileHelper = Anthropic.Files.Helper.TFileHelper;

{$ENDREGION}

{$REGION 'Anthropic.API.MultiFormData'}

  TMultiFormDataParams = Anthropic.API.MultiFormData.TMultiFormDataParams;

{$ENDREGION}

{$REGION 'Anthropic.API.JsonSafeReader'}

  TJsonReader = Anthropic.API.JsonSafeReader.TJsonReader;

{$ENDREGION}

{$REGION 'Anthropic.Chat.Beta'}
  TTextCitation = Anthropic.Chat.Beta.TTextCitation;
  TCitationConfig = Anthropic.Chat.Beta.TCitationConfig;
  TToolResultErrorCode = Anthropic.Chat.Beta.TToolResultErrorCode;
  TToolResultErrorCodeMessage = Anthropic.Chat.Beta.TToolResultErrorCodeMessage;
  TWebSearchToolResultBlockContent = Anthropic.Chat.Beta.TWebSearchToolResultBlockContent;
  TWebSearchToolResultBlock = Anthropic.Chat.Beta.TWebSearchToolResultBlock;
  TDocumentSource = Anthropic.Chat.Beta.TDocumentSource;
  TDocumentBlock = Anthropic.Chat.Beta.TDocumentBlock;
  TWebFetchToolResultBlockContent = Anthropic.Chat.Beta.TWebFetchToolResultBlockContent;
  TWebFetchToolResultBlock = Anthropic.Chat.Beta.TWebFetchToolResultBlock;
  TCodeExecutionOutputBlock = Anthropic.Chat.Beta.TCodeExecutionOutputBlock;
  TCodeExecutionToolResultBlockContent = Anthropic.Chat.Beta.TCodeExecutionToolResultBlockContent;
  TCodeExecutionToolResultBlock = Anthropic.Chat.Beta.TCodeExecutionToolResultBlock;
  TBashCodeExecutionResultBlock = Anthropic.Chat.Beta.TBashCodeExecutionResultBlock;
  TBashCodeExecutionToolResultBlockContent = Anthropic.Chat.Beta.TBashCodeExecutionToolResultBlockContent;
  TBashCodeExecutionToolResultBlock = Anthropic.Chat.Beta.TBashCodeExecutionToolResultBlock;
  TTextEditorCodeExecutionToolResultBlockContent = Anthropic.Chat.Beta.TTextEditorCodeExecutionToolResultBlockContent;
  TTextEditorCodeExecutionToolResultBlock = Anthropic.Chat.Beta.TTextEditorCodeExecutionToolResultBlock;
  TToolReferenceBlock = Anthropic.Chat.Beta.TToolReferenceBlock;
  TToolSearchToolResultBlockContent = Anthropic.Chat.Beta.TToolSearchToolResultBlockContent;
  TToolSearchToolResultBlock = Anthropic.Chat.Beta.TToolSearchToolResultBlock;
  TMCPToolResultBlockContent = Anthropic.Chat.Beta.TMCPToolResultBlockContent;
  TMCPToolResultBlock = Anthropic.Chat.Beta.TMCPToolResultBlock;
  TCodeExecutionTool20260120 = Anthropic.Chat.Request.TCodeExecutionTool20260120;
  TWebSearchTool20260209 = Anthropic.Chat.Request.TWebSearchTool20260209;
  TWebFetchTool20260209 = Anthropic.Chat.Request.TWebFetchTool20260209;
  TWebFetchTool20260309 = Anthropic.Chat.Request.TWebFetchTool20260309;
  TAdvisorTool20260301 = Anthropic.Chat.Request.TAdvisorTool20260301;
  TOutputConfigFormat = Anthropic.Chat.Request.TOutputConfigFormat;
  TOutputConfigTaskBudget = Anthropic.Chat.Request.TOutputConfigTaskBudget;
  TCompactionBlockParam = Anthropic.Chat.Request.TCompactionBlockParam;
  TDirectCaller = Anthropic.Chat.Request.TDirectCaller;
  TServerToolCaller = Anthropic.Chat.Request.TServerToolCaller;
  TToolContent = Anthropic.Chat.Beta.TToolContent;

{$ENDREGION}

{$REGION 'Anthropic.Chat.Responses'}

  TContentBlockContent = Anthropic.Chat.Responses.TContentBlockContent;
  TContentBlock = Anthropic.Chat.Responses.TContentBlock;
  TCacheCreation = Anthropic.Chat.Responses.TCacheCreation;
  TServerToolUsage = Anthropic.Chat.Responses.TServerToolUsage;
  TUsage = Anthropic.Chat.Responses.TUsage;
  TMessage = Anthropic.Chat.Responses.TMessage;

  TChat = Anthropic.Chat.Responses.TChat;
  TAsynChat = Anthropic.Chat.Responses.TAsynChat;
  TPromiseChat = Anthropic.Chat.Responses.TPromiseChat;

  TTokensCountContextManagement = Anthropic.Chat.Responses.TTokensCountContextManagement;
  TTokenCount = Anthropic.Chat.Responses.TTokenCount;
  TAsynTokenCount = Anthropic.Chat.Responses.TAsynTokenCount;
  TPromiseTokenCount = Anthropic.Chat.Responses.TPromiseTokenCount;

{$ENDREGION}

{$REGION 'Anthropic.Chat.StreamEvents'}

  TErrorMessage = Anthropic.Chat.StreamEvents.TErrorMessage;
  TRawErrorEvent = Anthropic.Chat.StreamEvents.TRawErrorEvent;
  TRawMessageStartEvent = Anthropic.Chat.StreamEvents.TRawMessageStartEvent;
  TRawContentBlockStartEvent = Anthropic.Chat.StreamEvents.TRawContentBlockStartEvent;
  TRawContentBlockDelta = Anthropic.Chat.StreamEvents.TRawContentBlockDelta;
  TRawContentBlockDeltaEvent = Anthropic.Chat.StreamEvents.TRawContentBlockDeltaEvent;
  TRawContentBlockStopEvent = Anthropic.Chat.StreamEvents.TRawContentBlockStopEvent;
  TDelta = Anthropic.Chat.StreamEvents.TDelta;
  TRawMessageDeltaEvent = Anthropic.Chat.StreamEvents.TRawMessageDeltaEvent;
  TRawMessageStopEvent = Anthropic.Chat.StreamEvents.TRawMessageStopEvent;
  TStreamEvent = Anthropic.Chat.StreamEvents.TStreamEvent;

  TChatStream = Anthropic.Chat.StreamEvents.TChatStream;
  TAsynChatStream = Anthropic.Chat.StreamEvents.TAsynChatStream;
  TPromiseChatStream = Anthropic.Chat.StreamEvents.TPromiseChatStream;
  TChatEvent = Anthropic.Chat.StreamEvents.TChatEvent;

  TSessionCallbacks = Anthropic.Chat.StreamEvents.TSessionCallbacks;
  TSessionCallbacksStream = Anthropic.Chat.StreamEvents.TSessionCallbacksStream;

{$ENDREGION}

{$REGION 'Anthropic.Chat.StreamEngine'}

  IStreamEventHandler = Anthropic.Chat.StreamEngine.IStreamEventHandler;
  IEventEngineManager = Anthropic.Chat.StreamEngine.IEventEngineManager;
  TEventEngineManagerFactory = Anthropic.Chat.StreamEngine.TEventEngineManagerFactory;
  TEventExecutionEngine = Anthropic.Chat.StreamEngine.TEventExecutionEngine;
  TEventEngineManager = Anthropic.Chat.StreamEngine.TEventEngineManager;
  TMessageStart = Anthropic.Chat.StreamEngine.TMessageStart;
  TMessageDelta = Anthropic.Chat.StreamEngine.TMessageDelta;
  TMessageStop = Anthropic.Chat.StreamEngine.TMessageStop;
  TContentStart = Anthropic.Chat.StreamEngine.TContentStart;
  TContentDelta = Anthropic.Chat.StreamEngine.TContentDelta;
  TContentStop = Anthropic.Chat.StreamEngine.TContentStop;
  TErrorEvent = Anthropic.Chat.StreamEngine.TErrorEvent;

{$ENDREGION}

{$REGION 'Anthropic.Chat.StreamCallbacks'}

  TEventData = Anthropic.Chat.StreamCallbacks.TEventData;
  TStreamEventCallBack = Anthropic.Chat.StreamCallbacks.TStreamEventCallBack;
  IStreamEventDispatcher = Anthropic.Chat.StreamCallbacks.IStreamEventDispatcher;
  TStreamEventDispatcher = Anthropic.Chat.StreamCallbacks.TStreamEventDispatcher;

{$ENDREGION}

{$REGION 'Anthropic.Chat.Request'}

  TContentBlockParam = Anthropic.Chat.Request.TContentBlockParam;
  TCacheControlEphemeral = Anthropic.Chat.Request.TCacheControlEphemeral;
  TTextCitationParam = Anthropic.Chat.Request.TTextCitationParam;
  TCitationCharLocationParam = Anthropic.Chat.Request.TCitationCharLocationParam;
  TCitationPageLocationParam = Anthropic.Chat.Request.TCitationPageLocationParam;
  TCitationContentBlockLocationParam = Anthropic.Chat.Request.TCitationContentBlockLocationParam;
  TCitationWebSearchResultLocationParam = Anthropic.Chat.Request.TCitationWebSearchResultLocationParam;
  TCitationSearchResultLocationParam = Anthropic.Chat.Request.TCitationSearchResultLocationParam;
  TTextBlockParam = Anthropic.Chat.Request.TTextBlockParam;

  TImageSource = Anthropic.Chat.Request.TImageSource;
  TBase64ImageSource = Anthropic.Chat.Request.TBase64ImageSource;
  TURLImageSource = Anthropic.Chat.Request.TURLImageSource;
  TFileImageSource = Anthropic.Chat.Request.TFileImageSource;
  TImage = Anthropic.Chat.Request.TImage;
  TImageBlockParam = Anthropic.Chat.Request.TImageBlockParam;

  TDocumentSourceParam = Anthropic.Chat.Request.TDocumentSourceParam;
  TBase64PDFSource = Anthropic.Chat.Request.TBase64PDFSource;
  TPlainTextSource = Anthropic.Chat.Request.TPlainTextSource;
  TContentBlockSourceContent = Anthropic.Chat.Request.TContentBlockSourceContent;
  TContentBlockSource = Anthropic.Chat.Request.TContentBlockSource;
  TURLPDFSource = Anthropic.Chat.Request.TURLPDFSource;
  TFileDocumentSource = Anthropic.Chat.Request.TFileDocumentSource;
  TCitationsConfigParam = Anthropic.Chat.Request.TCitationsConfigParam;
  TDocument = Anthropic.Chat.Request.TDocument;
  TDocumentBlockParam = Anthropic.Chat.Request.TDocumentBlockParam;

  TSearchResultBlockParam = Anthropic.Chat.Request.TSearchResultBlockParam;
  TThinkingBlockParam = Anthropic.Chat.Request.TThinkingBlockParam;
  TRedactedThinkingBlockParam = Anthropic.Chat.Request.TRedactedThinkingBlockParam;
  TToolUseBlockParam = Anthropic.Chat.Request.TToolUseBlockParam;
  TToolReferenceBlockParam = Anthropic.Chat.Request.TToolReferenceBlockParam;
  TToolResultBlockParam = Anthropic.Chat.Request.TToolResultBlockParam;
  TServerToolUseBlockParam = Anthropic.Chat.Request.TServerToolUseBlockParam;
  TWebSearchToolResultBlockParamContent = Anthropic.Chat.Request.TWebSearchToolResultBlockParamContent;
  TWebSearchToolResultBlockItem = Anthropic.Chat.Request.TWebSearchToolResultBlockItem;
  TWebSearchToolRequestError = Anthropic.Chat.Request.TWebSearchToolRequestError;
  TWebSearchToolResultBlockParam = Anthropic.Chat.Request.TWebSearchToolResultBlockParam;
  TWebFetchToolResultErrorBlockParam = Anthropic.Chat.Request.TWebFetchToolResultErrorBlockParam;
  TWebFetchBlockParam = Anthropic.Chat.Request.TWebFetchBlockParam;
  TWebFetchToolResultBlockParam = Anthropic.Chat.Request.TWebFetchToolResultBlockParam;
  TCodeExecutionToolResultErrorParam = Anthropic.Chat.Request.TCodeExecutionToolResultErrorParam;
  TCodeExecutionOutputBlockParam = Anthropic.Chat.Request.TCodeExecutionOutputBlockParam;
  TCodeExecutionResultBlockParam = Anthropic.Chat.Request.TCodeExecutionResultBlockParam;
  TCodeExecutionToolResultBlockParam = Anthropic.Chat.Request.TCodeExecutionToolResultBlockParam;
  TBashCodeExecutionToolResultErrorParam = Anthropic.Chat.Request.TBashCodeExecutionToolResultErrorParam;
  TBashCodeExecutionOutputBlockParam = Anthropic.Chat.Request.TBashCodeExecutionOutputBlockParam;
  TBashCodeExecutionResultBlockParam = Anthropic.Chat.Request.TBashCodeExecutionResultBlockParam;
  TBashCodeExecutionToolResultBlockParam = Anthropic.Chat.Request.TBashCodeExecutionToolResultBlockParam;
  TTextEditorCodeExecutionToolResultErrorParam = Anthropic.Chat.Request.TTextEditorCodeExecutionToolResultErrorParam;
  TTextEditorCodeExecutionViewResultBlockParam = Anthropic.Chat.Request.TTextEditorCodeExecutionViewResultBlockParam;
  TTextEditorCodeExecutionCreateResultBlockParam = Anthropic.Chat.Request.TTextEditorCodeExecutionCreateResultBlockParam;
  TTextEditorCodeExecutionStrReplaceResultBlockParam = Anthropic.Chat.Request.TTextEditorCodeExecutionStrReplaceResultBlockParam;
  TTextEditorCodeExecutionToolResultBlockParam = Anthropic.Chat.Request.TTextEditorCodeExecutionToolResultBlockParam;
  TToolSearchToolResultErrorParam = Anthropic.Chat.Request.TToolSearchToolResultErrorParam;
  TToolSearchToolSearchResultBlockParam = Anthropic.Chat.Request.TToolSearchToolSearchResultBlockParam;
  TToolSearchToolResultBlockParam = Anthropic.Chat.Request.TToolSearchToolResultBlockParam;
  TMCPToolUseBlockParam = Anthropic.Chat.Request.TMCPToolUseBlockParam;
  TMCPToolResultBlockParam = Anthropic.Chat.Request.TMCPToolResultBlockParam;
  TContainerUploadBlockParam = Anthropic.Chat.Request.TContainerUploadBlockParam;

  TMessageParam = Anthropic.Chat.Request.TMessageParam;
  TThinkingConfigParam = Anthropic.Chat.Request.TThinkingConfigParam;
  TToolChoice = Anthropic.Chat.Request.TToolChoice;

  TSkillParams = Anthropic.Chat.Request.TSkillParams;
  TContainerParams = Anthropic.Chat.Request.TContainerParams;
  TEditsParams = Anthropic.Chat.Request.TEditsParams;
  TInputTokensClearAtLeast = Anthropic.Chat.Request.TInputTokensClearAtLeast;
  TToolUsesKeep = Anthropic.Chat.Request.TToolUsesKeep;
  TInputTokensTrigger = Anthropic.Chat.Request.TInputTokensTrigger;
  TToolUsesTrigger = Anthropic.Chat.Request.TToolUsesTrigger;
  TClearToolUses20250919Edit = Anthropic.Chat.Request.TClearToolUses20250919Edit;
  TThinkingTurns = Anthropic.Chat.Request.TThinkingTurns;
  TAllThinkingTurns = Anthropic.Chat.Request.TAllThinkingTurns;
  TClearThinking20251015Edit = Anthropic.Chat.Request.TClearThinking20251015Edit;
  TCompact20260112Edit = Anthropic.Chat.Request.TCompact20260112Edit;
  TContextManagementConfig = Anthropic.Chat.Request.TContextManagementConfig;
  TRequestMCPServerToolConfiguration = Anthropic.Chat.Request.TRequestMCPServerToolConfiguration;
  TRequestMCPServerURLDefinition = Anthropic.Chat.Request.TRequestMCPServerURLDefinition;
  TOutputConfig = Anthropic.Chat.Request.TOutputConfig;
  TJSONOutputFormat = Anthropic.Chat.Request.TJSONOutputFormat;

  TInputSchema = Anthropic.Chat.Request.TInputSchema;
  TUserLocation = Anthropic.Chat.Request.TUserLocation;
  TMCPconfigs = Anthropic.Chat.Request.TMCPconfigs;

  TToolUnion = Anthropic.Chat.Request.TToolUnion;

  // Tools
  TTool = Anthropic.Chat.Request.TTool;
  TToolBash20250124 = Anthropic.Chat.Request.TToolBash20250124;
  TToolTextEditor20250124 = Anthropic.Chat.Request.TToolTextEditor20250124;
  TToolTextEditor20250429 = Anthropic.Chat.Request.TToolTextEditor20250429;
  TToolTextEditor20250728 = Anthropic.Chat.Request.TToolTextEditor20250728;
  TWebSearchTool20250305 = Anthropic.Chat.Request.TWebSearchTool20250305;

  // [beta] Tools
  TToolBash20241022 = Anthropic.Chat.Request.TToolBash20241022;
  TCodeExecutionTool20250522 = Anthropic.Chat.Request.TCodeExecutionTool20250522;
  TCodeExecutionTool20250825 = Anthropic.Chat.Request.TCodeExecutionTool20250825;
  TToolComputerUse20241022 = Anthropic.Chat.Request.TToolComputerUse20241022;
  TMemoryTool20250818 = Anthropic.Chat.Request.TMemoryTool20250818;
  TToolComputerUse20250124 = Anthropic.Chat.Request.TToolComputerUse20250124;
  TToolTextEditor20241022 = Anthropic.Chat.Request.TToolTextEditor20241022;
  TToolComputerUse20251124 = Anthropic.Chat.Request.TToolComputerUse20251124;
  TWebFetchTool20250910 = Anthropic.Chat.Request.TWebFetchTool20250910;
  TToolSearchToolBm25_20251119 = Anthropic.Chat.Request.TToolSearchToolBm25_20251119;
  TToolSearchToolRegex20251119 = Anthropic.Chat.Request.TToolSearchToolRegex20251119;
  TMCPToolset = Anthropic.Chat.Request.TMCPToolset;

  TChatParams = Anthropic.Chat.Request.TChatParams;

  TChatParamProc = Anthropic.Chat.Request.TChatParamProc;

{$ENDREGION}

{$REGION 'Anthropic.Batches'}

  TBatchParams = Anthropic.Batches.TBatchParams;
  TRequestParams = Anthropic.Batches.TRequestParams;
  TRequestParamProc = Anthropic.Batches.TRequestParamProc;
  TRequestCounts = Anthropic.Batches.TRequestCounts;
  TBatch = Anthropic.Batches.TBatch;
  TBatchList = Anthropic.Batches.TBatchList;
  TBatchDelete = Anthropic.Batches.TBatchDelete;
  TListParams = Anthropic.Batches.TListParams;
  TListParamProc = Anthropic.Batches.TListParamProc;

  TAsynBatch = Anthropic.Batches.TAsynBatch;
  TPromiseBatch = Anthropic.Batches.TPromiseBatch;
  TAsynBatchList = Anthropic.Batches.TAsynBatchList;
  TPromiseBatchList = Anthropic.Batches.TPromiseBatchList;
  TAsynStringList = Anthropic.Batches.TAsynStringList;
  TPromiseStringList = Anthropic.Batches.TPromiseStringList;
  TAsynBatchDelete = Anthropic.Batches.TAsynBatchDelete;
  TPromiseBatchDelete = Anthropic.Batches.TPromiseBatchDelete;

{$ENDREGION}

{$REGION 'Anthropic.Batches.Support'}

  IBatcheResults = Anthropic.Batches.Support.IBatcheResults;
  TBatcheResultsFactory = Anthropic.Batches.Support.TBatcheResultsFactory;
  TBatcheResultItem = Anthropic.Batches.Support.TBatcheResultItem;
  TBatcheResult = Anthropic.Batches.Support.TBatcheResult;

{$ENDREGION}

{$REGION 'Anthropic.Files'}

  TUploadParams = Anthropic.Files.TUploadParams;
  TUploadParamProc = Anthropic.Files.TUploadParamProc;
  TFilesListParams = Anthropic.Files.TFilesListParams;
  TFilesListParamProc = Anthropic.Files.TFilesListParamProc;
  TFile = Anthropic.Files.TFile;
  TFileList = Anthropic.Files.TFileList;
  TFileDeleted = Anthropic.Files.TFileDeleted;
  TFileDownloaded = Anthropic.Files.TFileDownloaded;

  TAsynFile = Anthropic.Files.TAsynFile;
  TPromiseFile = Anthropic.Files.TPromiseFile;
  TAsynFileList = Anthropic.Files.TAsynFileList;
  TPromiseFileList = Anthropic.Files.TPromiseFileList;
  TAsynFileDeleted = Anthropic.Files.TAsynFileDeleted;
  TPromiseFileDeleted = Anthropic.Files.TPromiseFileDeleted;

{$ENDREGION}

{$REGION 'Anthropic.Models'}

  TListModelsParams = Anthropic.Models.TListModelsParams;
  TListModelsParamProc = Anthropic.Models.TListModelsParamProc;
  TModel = Anthropic.Models.TModel;
  TAsynModel = Anthropic.Models.TAsynModel;
  TPromiseModel = Anthropic.Models.TPromiseModel;

  TModels = Anthropic.Models.TModels;
  TAsynModels = Anthropic.Models.TAsynModels;
  TPromiseModels = Anthropic.Models.TPromiseModels;

{$ENDREGION}

{$REGION 'Anthropic.Skills'}

  TSkillFormDataParams = Anthropic.Skills.TSkillFormDataParams;
  TSkillFormDataParamProc = Anthropic.Skills.TSkillFormDataParamProc;

  TSkillListParams = Anthropic.Skills.TSkillListParams;
  TSkillListParamProc = Anthropic.Skills.TSkillListParamProc;

  TSkill = Anthropic.Skills.TSkill;
  TSkillList = Anthropic.Skills.TSkillList;
  TSkillDeleted = Anthropic.Skills.TSkillDeleted;

  TAsynSkill = Anthropic.Skills.TAsynSkill;
  TPromiseSkill = Anthropic.Skills.TPromiseSkill;
  TAsynSkillList = Anthropic.Skills.TAsynSkillList;
  TPromiseSkillList = Anthropic.Skills.TPromiseSkillList;
  TAsynSkillDeleted = Anthropic.Skills.TAsynSkillDeleted;
  TPromiseSkillDeleted = Anthropic.Skills.TPromiseSkillDeleted;

  TSkillVersion = Anthropic.Skills.TSkillVersion;
  TAsynSkillVersion = Anthropic.Skills.TAsynSkillVersion;
  TPromiseSkillVersion = Anthropic.Skills.TPromiseSkillVersion;

  TSkillVersionList = Anthropic.Skills.TSkillVersionList;
  TAsynSkillVersionList = Anthropic.Skills.TAsynSkillVersionList;
  TPromiseSkillVersionList = Anthropic.Skills.TPromiseSkillVersionList;

{$ENDREGION}

{$REGION 'Anthropic.Functions.Core'}

  IFunctionCore = Anthropic.Functions.Core.IFunctionCore;
  TFunctionCore = Anthropic.Functions.Core.TFunctionCore;

{$ENDREGION}

{$REGION 'Anthropic.Schema'}

  TPropertyItem = Anthropic.Schema.TPropertyItem;
  TSchemaParams = Anthropic.Schema.TSchemaParams;

{$ENDREGION}

{$REGION 'Anthropic.Net.MediaCodec'}

  TMediaCodec = Anthropic.Net.MediaCodec.TMediaCodec;

{$ENDREGION}

{$REGION 'Anthropic.JSONL'}

  TJSONLHelper = Anthropic.JSONL.TJSONLHelper;

{$ENDREGION}

{$REGION 'Anthropic.Agents'}

  TAgentModelConfigParams = Anthropic.Agents.TAgentModelConfigParams;
  TAgentMCPServerParams = Anthropic.Agents.TAgentMCPServerParams;
  TAgentRosterEntryParams = Anthropic.Agents.TAgentRosterEntryParams;
  TAgentReferenceParams = Anthropic.Agents.TAgentReferenceParams;
  TAgentSelfReferenceParams = Anthropic.Agents.TAgentSelfReferenceParams;
  TAgentMultiagentParams = Anthropic.Agents.TAgentMultiagentParams;

  TAgentSkillParams = Anthropic.Agents.TAgentSkillParams;
  TAgentAnthropicSkillParams = Anthropic.Agents.TAgentAnthropicSkillParams;
  TAgentCustomSkillParams = Anthropic.Agents.TAgentCustomSkillParams;

  TAgentPermissionPolicyParams = Anthropic.Agents.TAgentPermissionPolicyParams;
  TAgentAlwaysAllowPolicyParams = Anthropic.Agents.TAgentAlwaysAllowPolicyParams;
  TAgentAlwaysAskPolicyParams = Anthropic.Agents.TAgentAlwaysAskPolicyParams;

  TAgentToolConfigParams = Anthropic.Agents.TAgentToolConfigParams;
  TAgentToolsetDefaultConfigParams = Anthropic.Agents.TAgentToolsetDefaultConfigParams;
  TAgentToolParams = Anthropic.Agents.TAgentToolParams;
  TAgentBuiltInToolsetParams = Anthropic.Agents.TAgentBuiltInToolsetParams;
  TAgentMCPToolsetParams = Anthropic.Agents.TAgentMCPToolsetParams;
  TAgentCustomToolParams = Anthropic.Agents.TAgentCustomToolParams;

  TAgentCreateParams = Anthropic.Agents.TAgentCreateParams;
  TAgentUpdateParams = Anthropic.Agents.TAgentUpdateParams;
  TAgentListParams = Anthropic.Agents.TAgentListParams;
  TAgentRetrieveParams = Anthropic.Agents.TAgentRetrieveParams;

  TAgentListParamProc = Anthropic.Agents.TAgentListParamProc;
  TAgentRetrieveParamProc = Anthropic.Agents.TAgentRetrieveParamProc;
  TAgentCreateParamProc = Anthropic.Agents.TAgentCreateParamProc;
  TAgentUpdateParamProc = Anthropic.Agents.TAgentUpdateParamProc;

  TAgentModelConfig = Anthropic.Agents.TAgentModelConfig;
  TAgentMCPServer = Anthropic.Agents.TAgentMCPServer;
  TAgentReference = Anthropic.Agents.TAgentReference;
  TAgentMultiagent = Anthropic.Agents.TAgentMultiagent;
  TAgentSkill = Anthropic.Agents.TAgentSkill;
  TAgentAnthropicSkill = Anthropic.Agents.TAgentAnthropicSkill;
  TAgentCustomSkill = Anthropic.Agents.TAgentCustomSkill;
  TAgentPermissionPolicy = Anthropic.Agents.TAgentPermissionPolicy;
  TAgentAlwaysAllowPolicy = Anthropic.Agents.TAgentAlwaysAllowPolicy;
  TAgentAlwaysAskPolicy = Anthropic.Agents.TAgentAlwaysAskPolicy;
  TAgentToolConfig = Anthropic.Agents.TAgentToolConfig;
  TAgentToolsetDefaultConfig = Anthropic.Agents.TAgentToolsetDefaultConfig;
  TAgentTool = Anthropic.Agents.TAgentTool;
  TAgentBuiltInToolset = Anthropic.Agents.TAgentBuiltInToolset;
  TAgentMCPToolset = Anthropic.Agents.TAgentMCPToolset;
  TAgentCustomTool = Anthropic.Agents.TAgentCustomTool;

  TAgent = Anthropic.Agents.TAgent;
  TAgentList = Anthropic.Agents.TAgentList;

  TAsynAgent = Anthropic.Agents.TAsynAgent;
  TPromiseAgent = Anthropic.Agents.TPromiseAgent;
  TAsynAgentList = Anthropic.Agents.TAsynAgentList;
  TPromiseAgentList = Anthropic.Agents.TPromiseAgentList;

{$ENDREGION}

{$REGION 'Anthropic.Environment'}

  TEnvironmentNetworkParams = Anthropic.Environment.TEnvironmentNetworkParams;
  TEnvironmentUnrestrictedNetworkParams = Anthropic.Environment.TEnvironmentUnrestrictedNetworkParams;
  TEnvironmentLimitedNetworkParams = Anthropic.Environment.TEnvironmentLimitedNetworkParams;
  TEnvironmentPackagesParams = Anthropic.Environment.TEnvironmentPackagesParams;
  TEnvironmentCloudConfigParams = Anthropic.Environment.TEnvironmentCloudConfigParams;
  TEnvironmentCreateParams = Anthropic.Environment.TEnvironmentCreateParams;
  TEnvironmentUpdateParams = Anthropic.Environment.TEnvironmentUpdateParams;
  TEnvironmentListParams = Anthropic.Environment.TEnvironmentListParams;

  TEnvironmentListParamProc = Anthropic.Environment.TEnvironmentListParamProc;
  TEnvironmentCreateParamProc = Anthropic.Environment.TEnvironmentCreateParamProc;
  TEnvironmentUpdateParamProc = Anthropic.Environment.TEnvironmentUpdateParamProc;

  TEnvironmentNetwork = Anthropic.Environment.TEnvironmentNetwork;
  TEnvironmentUnrestrictedNetwork = Anthropic.Environment.TEnvironmentUnrestrictedNetwork;
  TEnvironmentLimitedNetwork = Anthropic.Environment.TEnvironmentLimitedNetwork;
  TEnvironmentPackages = Anthropic.Environment.TEnvironmentPackages;
  TEnvironmentCloudConfig = Anthropic.Environment.TEnvironmentCloudConfig;
  TEnvironment = Anthropic.Environment.TEnvironment;
  TEnvironmentList = Anthropic.Environment.TEnvironmentList;
  TEnvironmentDeleteResponse = Anthropic.Environment.TEnvironmentDeleteResponse;

  TAsynEnvironment = Anthropic.Environment.TAsynEnvironment;
  TPromiseEnvironment = Anthropic.Environment.TPromiseEnvironment;
  TAsynEnvironmentList = Anthropic.Environment.TAsynEnvironmentList;
  TPromiseEnvironmentList = Anthropic.Environment.TPromiseEnvironmentList;
  TAsynEnvironmentDeleteResponse = Anthropic.Environment.TAsynEnvironmentDeleteResponse;
  TPromiseEnvironmentDeleteResponse = Anthropic.Environment.TPromiseEnvironmentDeleteResponse;

  TEnvironmentsAbstractSupport = Anthropic.Environment.TEnvironmentsAbstractSupport;
  TEnvironmentsAsynchronousSupport = Anthropic.Environment.TEnvironmentsAsynchronousSupport;
  TEnvironmentsRoute = Anthropic.Environment.TEnvironmentsRoute;

{$ENDREGION}

{$REGION 'Anthropic.Sessions'}

  TSessionAgentParams = Anthropic.Sessions.TSessionAgentParams;
  TSessionCheckoutParams = Anthropic.Sessions.TSessionCheckoutParams;
  TSessionBranchCheckoutParams = Anthropic.Sessions.TSessionBranchCheckoutParams;
  TSessionCommitCheckoutParams = Anthropic.Sessions.TSessionCommitCheckoutParams;

  TSessionResourceParams = Anthropic.Sessions.TSessionResourceParams;
  TSessionGitHubRepositoryResourceParams = Anthropic.Sessions.TSessionGitHubRepositoryResourceParams;
  TSessionFileResourceParams = Anthropic.Sessions.TSessionFileResourceParams;
  TSessionMemoryStoreResourceParams = Anthropic.Sessions.TSessionMemoryStoreResourceParams;

  TSessionCreateParams = Anthropic.Sessions.TSessionCreateParams;
  TSessionUpdateParams = Anthropic.Sessions.TSessionUpdateParams;
  TSessionListParams = Anthropic.Sessions.TSessionListParams;
  TSessionEventListParams = Anthropic.Sessions.TSessionEventListParams;
  TSessionSimpleListParams = Anthropic.Sessions.TSessionSimpleListParams;

  TSessionSourceParams = Anthropic.Sessions.TSessionSourceParams;
  TSessionBase64ImageSourceParams = Anthropic.Sessions.TSessionBase64ImageSourceParams;
  TSessionUrlSourceParams = Anthropic.Sessions.TSessionUrlSourceParams;
  TSessionFileSourceParams = Anthropic.Sessions.TSessionFileSourceParams;
  TSessionBase64DocumentSourceParams = Anthropic.Sessions.TSessionBase64DocumentSourceParams;
  TSessionPlainTextDocumentSourceParams = Anthropic.Sessions.TSessionPlainTextDocumentSourceParams;

  TSessionContentBlockParams = Anthropic.Sessions.TSessionContentBlockParams;
  TSessionTextBlockParams = Anthropic.Sessions.TSessionTextBlockParams;
  TSessionImageBlockParams = Anthropic.Sessions.TSessionImageBlockParams;
  TSessionDocumentBlockParams = Anthropic.Sessions.TSessionDocumentBlockParams;

  TSessionEventParams = Anthropic.Sessions.TSessionEventParams;
  TSessionUserMessageEventParams = Anthropic.Sessions.TSessionUserMessageEventParams;
  TSessionUserInterruptEventParams = Anthropic.Sessions.TSessionUserInterruptEventParams;
  TSessionUserToolConfirmationEventParams = Anthropic.Sessions.TSessionUserToolConfirmationEventParams;
  TSessionUserCustomToolResultEventParams = Anthropic.Sessions.TSessionUserCustomToolResultEventParams;
  TSessionRubricParams = Anthropic.Sessions.TSessionRubricParams;
  TSessionTextRubricParams = Anthropic.Sessions.TSessionTextRubricParams;
  TSessionFileRubricParams = Anthropic.Sessions.TSessionFileRubricParams;
  TSessionUserDefineOutcomeEventParams = Anthropic.Sessions.TSessionUserDefineOutcomeEventParams;
  TSessionSendEventsParams = Anthropic.Sessions.TSessionSendEventsParams;

  TSessionResourceUpdateParams = Anthropic.Sessions.TSessionResourceUpdateParams;

  TSessionListParamProc = Anthropic.Sessions.TSessionListParamProc;
  TSessionCreateParamProc = Anthropic.Sessions.TSessionCreateParamProc;
  TSessionUpdateParamProc = Anthropic.Sessions.TSessionUpdateParamProc;
  TSessionEventListParamProc = Anthropic.Sessions.TSessionEventListParamProc;
  TSessionSimpleListParamProc = Anthropic.Sessions.TSessionSimpleListParamProc;
  TSessionSendEventsParamProc = Anthropic.Sessions.TSessionSendEventsParamProc;
  TSessionResourceAddParamProc = Anthropic.Sessions.TSessionResourceAddParamProc;
  TSessionResourceUpdateParamProc = Anthropic.Sessions.TSessionResourceUpdateParamProc;

  TSessionModelConfig = Anthropic.Sessions.TSessionModelConfig;
  TSessionAgent = Anthropic.Sessions.TSessionAgent;
  TSessionCacheCreationUsage = Anthropic.Sessions.TSessionCacheCreationUsage;
  TSessionUsage = Anthropic.Sessions.TSessionUsage;
  TSessionStats = Anthropic.Sessions.TSessionStats;
  TSessionResource = Anthropic.Sessions.TSessionResource;
  TSessionOutcomeEvaluation = Anthropic.Sessions.TSessionOutcomeEvaluation;
  TSession = Anthropic.Sessions.TSession;
  TSessionList = Anthropic.Sessions.TSessionList;
  TSessionDeleted = Anthropic.Sessions.TSessionDeleted;
  TSessionEvent = Anthropic.Sessions.TSessionEvent;
  TSessionEventList = Anthropic.Sessions.TSessionEventList;
  TSessionSendEventsResponse = Anthropic.Sessions.TSessionSendEventsResponse;
  TSessionResourceList = Anthropic.Sessions.TSessionResourceList;
  TSessionResourceDeleted = Anthropic.Sessions.TSessionResourceDeleted;
  TSessionThread = Anthropic.Sessions.TSessionThread;
  TSessionThreadList = Anthropic.Sessions.TSessionThreadList;
  TSessionStream = Anthropic.Sessions.TSessionStream;

  TAsynSession = Anthropic.Sessions.TAsynSession;
  TPromiseSession = Anthropic.Sessions.TPromiseSession;
  TAsynSessionList = Anthropic.Sessions.TAsynSessionList;
  TPromiseSessionList = Anthropic.Sessions.TPromiseSessionList;
  TAsynSessionDeleted = Anthropic.Sessions.TAsynSessionDeleted;
  TPromiseSessionDeleted = Anthropic.Sessions.TPromiseSessionDeleted;
  TAsynSessionEventList = Anthropic.Sessions.TAsynSessionEventList;
  TPromiseSessionEventList = Anthropic.Sessions.TPromiseSessionEventList;
  TAsynSessionSendEventsResponse = Anthropic.Sessions.TAsynSessionSendEventsResponse;
  TPromiseSessionSendEventsResponse = Anthropic.Sessions.TPromiseSessionSendEventsResponse;
  TAsynSessionResource = Anthropic.Sessions.TAsynSessionResource;
  TPromiseSessionResource = Anthropic.Sessions.TPromiseSessionResource;
  TAsynSessionResourceList = Anthropic.Sessions.TAsynSessionResourceList;
  TPromiseSessionResourceList = Anthropic.Sessions.TPromiseSessionResourceList;
  TAsynSessionResourceDeleted = Anthropic.Sessions.TAsynSessionResourceDeleted;
  TPromiseSessionResourceDeleted = Anthropic.Sessions.TPromiseSessionResourceDeleted;
  TAsynSessionThread = Anthropic.Sessions.TAsynSessionThread;
  TPromiseSessionThread = Anthropic.Sessions.TPromiseSessionThread;
  TAsynSessionThreadList = Anthropic.Sessions.TSessionThreadList;
  TPromiseSessionThreadList = Anthropic.Sessions.TPromiseSessionThreadList;
  TAsynSessionStream = Anthropic.Sessions.TAsynSessionStream;
  TPromiseSessionStream = Anthropic.Sessions.TPromiseSessionStream;

  TSessionsAbstractSupport = Anthropic.Sessions.TSessionsAbstractSupport;
  TSessionsAsynchronousSupport = Anthropic.Sessions.TSessionsAsynchronousSupport;
  TSessionsRoute = Anthropic.Sessions.TSessionsRoute;

  TSessionEventsAbstractSupport = Anthropic.Sessions.TSessionEventsAbstractSupport;
  TSessionEventsAsynchronousSupport = Anthropic.Sessions.TSessionEventsAsynchronousSupport;
  TSessionEventsRoute = Anthropic.Sessions.TSessionEventsRoute;

  TSessionResourcesAbstractSupport = Anthropic.Sessions.TSessionResourcesAbstractSupport;
  TSessionResourcesAsynchronousSupport = Anthropic.Sessions.TSessionResourcesAsynchronousSupport;
  TSessionResourcesRoute = Anthropic.Sessions.TSessionResourcesRoute;

  TSessionThreadsAbstractSupport = Anthropic.Sessions.TSessionThreadsAbstractSupport;
  TSessionThreadsAsynchronousSupport = Anthropic.Sessions.TSessionThreadsAsynchronousSupport;
  TSessionThreadsRoute = Anthropic.Sessions.TSessionThreadsRoute;

  TSessionThreadEventsAbstractSupport = Anthropic.Sessions.TSessionThreadEventsAbstractSupport;
  TSessionThreadEventsAsynchronousSupport = Anthropic.Sessions.TSessionThreadEventsAsynchronousSupport;
  TSessionThreadEventsRoute = Anthropic.Sessions.TSessionThreadEventsRoute;

{$ENDREGION}

{$REGION 'Anthropic.Vaults'}

  TVaultCreateParams = Anthropic.Vaults.TVaultCreateParams;
  TVaultUpdateParams = Anthropic.Vaults.TVaultUpdateParams;
  TVaultListParams = Anthropic.Vaults.TVaultListParams;

  TVaultCredentialTokenEndpointAuthParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthParams;
  TVaultCredentialTokenEndpointAuthNoneParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthNoneParams;
  TVaultCredentialTokenEndpointAuthBasicParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthBasicParams;
  TVaultCredentialTokenEndpointAuthPostParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthPostParams;

  TVaultCredentialMCPOAuthRefreshParams = Anthropic.Vaults.TVaultCredentialMCPOAuthRefreshParams;
  TVaultCredentialAuthCreateParams = Anthropic.Vaults.TVaultCredentialAuthCreateParams;
  TVaultCredentialMCPOAuthCreateParams = Anthropic.Vaults.TVaultCredentialMCPOAuthCreateParams;
  TVaultCredentialStaticBearerCreateParams = Anthropic.Vaults.TVaultCredentialStaticBearerCreateParams;

  TVaultCredentialTokenEndpointAuthUpdateParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthUpdateParams;
  TVaultCredentialTokenEndpointAuthBasicUpdateParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthBasicUpdateParams;
  TVaultCredentialTokenEndpointAuthPostUpdateParams = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthPostUpdateParams;

  TVaultCredentialMCPOAuthRefreshUpdateParams = Anthropic.Vaults.TVaultCredentialMCPOAuthRefreshUpdateParams;
  TVaultCredentialAuthUpdateParams = Anthropic.Vaults.TVaultCredentialAuthUpdateParams;
  TVaultCredentialMCPOAuthUpdateParams = Anthropic.Vaults.TVaultCredentialMCPOAuthUpdateParams;
  TVaultCredentialStaticBearerUpdateParams = Anthropic.Vaults.TVaultCredentialStaticBearerUpdateParams;

  TVaultCredentialCreateParams = Anthropic.Vaults.TVaultCredentialCreateParams;
  TVaultCredentialUpdateParams = Anthropic.Vaults.TVaultCredentialUpdateParams;
  TVaultCredentialListParams = Anthropic.Vaults.TVaultCredentialListParams;

  TVaultCreateParamProc = Anthropic.Vaults.TVaultCreateParamProc;
  TVaultUpdateParamProc = Anthropic.Vaults.TVaultUpdateParamProc;
  TVaultListParamProc = Anthropic.Vaults.TVaultListParamProc;
  TVaultCredentialCreateParamProc = Anthropic.Vaults.TVaultCredentialCreateParamProc;
  TVaultCredentialUpdateParamProc = Anthropic.Vaults.TVaultCredentialUpdateParamProc;
  TVaultCredentialListParamProc = Anthropic.Vaults.TVaultCredentialListParamProc;

  TVault = Anthropic.Vaults.TVault;
  TVaultDeleted = Anthropic.Vaults.TVaultDeleted;
  TVaultList = Anthropic.Vaults.TVaultList;

  TVaultCredentialTokenEndpointAuth = Anthropic.Vaults.TVaultCredentialTokenEndpointAuth;
  TVaultCredentialTokenEndpointAuthNone = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthNone;
  TVaultCredentialTokenEndpointAuthBasic = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthBasic;
  TVaultCredentialTokenEndpointAuthPost = Anthropic.Vaults.TVaultCredentialTokenEndpointAuthPost;

  TVaultCredentialMCPOAuthRefresh = Anthropic.Vaults.TVaultCredentialMCPOAuthRefresh;
  TVaultCredentialAuth = Anthropic.Vaults.TVaultCredentialAuth;
  TVaultCredentialMCPOAuthAuth = Anthropic.Vaults.TVaultCredentialMCPOAuthAuth;
  TVaultCredentialStaticBearerAuth = Anthropic.Vaults.TVaultCredentialStaticBearerAuth;

  TVaultCredential = Anthropic.Vaults.TVaultCredential;
  TVaultCredentialDeleted = Anthropic.Vaults.TVaultCredentialDeleted;
  TVaultCredentialList = Anthropic.Vaults.TVaultCredentialList;

  TVaultCredentialRefreshHTTPResponse = Anthropic.Vaults.TVaultCredentialRefreshHTTPResponse;
  TVaultCredentialMCPProbe = Anthropic.Vaults.TVaultCredentialMCPProbe;
  TVaultCredentialRefreshObject = Anthropic.Vaults.TVaultCredentialRefreshObject;
  TVaultCredentialValidation = Anthropic.Vaults.TVaultCredentialValidation;

  TAsynVault = Anthropic.Vaults.TAsynVault;
  TPromiseVault = Anthropic.Vaults.TPromiseVault;
  TAsynVaultList = Anthropic.Vaults.TAsynVaultList;
  TPromiseVaultList = Anthropic.Vaults.TPromiseVaultList;
  TAsynVaultDeleted = Anthropic.Vaults.TAsynVaultDeleted;
  TPromiseVaultDeleted = Anthropic.Vaults.TPromiseVaultDeleted;

  TAsynVaultCredential = Anthropic.Vaults.TAsynVaultCredential;
  TPromiseVaultCredential = Anthropic.Vaults.TPromiseVaultCredential;
  TAsynVaultCredentialList = Anthropic.Vaults.TAsynVaultCredentialList;
  TPromiseVaultCredentialList = Anthropic.Vaults.TPromiseVaultCredentialList;
  TAsynVaultCredentialDeleted = Anthropic.Vaults.TAsynVaultCredentialDeleted;
  TPromiseVaultCredentialDeleted = Anthropic.Vaults.TPromiseVaultCredentialDeleted;
  TAsynVaultCredentialValidation = Anthropic.Vaults.TAsynVaultCredentialValidation;
  TPromiseVaultCredentialValidation = Anthropic.Vaults.TPromiseVaultCredentialValidation;

  TVaultCredentialsAbstractSupport = Anthropic.Vaults.TVaultCredentialsAbstractSupport;
  TVaultCredentialsAsynchronousSupport = Anthropic.Vaults.TVaultCredentialsAsynchronousSupport;
  TVaultCredentialsRoute = Anthropic.Vaults.TVaultCredentialsRoute;

  TVaultsAbstractSupport = Anthropic.Vaults.TVaultsAbstractSupport;
  TVaultsAsynchronousSupport = Anthropic.Vaults.TVaultsAsynchronousSupport;
  TVaultsRoute = Anthropic.Vaults.TVaultsRoute;

{$ENDREGION}

{$REGION 'Anthropic.MemoryStore'}

  TMemoryStoreCreateParams = Anthropic.MemoryStore.TMemoryStoreCreateParams;
  TMemoryStoreUpdateParams = Anthropic.MemoryStore.TMemoryStoreUpdateParams;
  TMemoryStoreListParams = Anthropic.MemoryStore.TMemoryStoreListParams;

  TMemoryViewParams = Anthropic.MemoryStore.TMemoryViewParams;
  TMemoryCreateParams = Anthropic.MemoryStore.TMemoryCreateParams;
  TMemoryPreconditionParams = Anthropic.MemoryStore.TMemoryPreconditionParams;
  TMemoryUpdateParams = Anthropic.MemoryStore.TMemoryUpdateParams;
  TMemoryListParams = Anthropic.MemoryStore.TMemoryListParams;
  TMemoryDeleteParams = Anthropic.MemoryStore.TMemoryDeleteParams;

  TMemoryVersionListParams = Anthropic.MemoryStore.TMemoryVersionListParams;

  TMemoryStoreCreateParamProc = Anthropic.MemoryStore.TMemoryStoreCreateParamProc;
  TMemoryStoreUpdateParamProc = Anthropic.MemoryStore.TMemoryStoreUpdateParamProc;
  TMemoryStoreListParamProc = Anthropic.MemoryStore.TMemoryStoreListParamProc;

  TMemoryCreateParamProc = Anthropic.MemoryStore.TMemoryCreateParamProc;
  TMemoryUpdateParamProc = Anthropic.MemoryStore.TMemoryUpdateParamProc;
  TMemoryListParamProc = Anthropic.MemoryStore.TMemoryListParamProc;
  TMemoryViewParamProc = Anthropic.MemoryStore.TMemoryViewParamProc;
  TMemoryDeleteParamProc = Anthropic.MemoryStore.TMemoryDeleteParamProc;

  TMemoryVersionListParamProc = Anthropic.MemoryStore.TMemoryVersionListParamProc;
  TMemoryVersionRetrieveParamProc = Anthropic.MemoryStore.TMemoryVersionRetrieveParamProc;

  TMemoryStore = Anthropic.MemoryStore.TMemoryStore;
  TMemoryStoreDeleted = Anthropic.MemoryStore.TMemoryStoreDeleted;
  TMemoryStoreList = Anthropic.MemoryStore.TMemoryStoreList;

  TMemoryListItem = Anthropic.MemoryStore.TMemoryListItem;
  TMemoryPrefix = Anthropic.MemoryStore.TMemoryPrefix;
  TMemory = Anthropic.MemoryStore.TMemory;
  TMemoryDeleted = Anthropic.MemoryStore.TMemoryDeleted;
  TMemoryList = Anthropic.MemoryStore.TMemoryList;

  TMemoryActor = Anthropic.MemoryStore.TMemoryActor;
  TMemoryVersion = Anthropic.MemoryStore.TMemoryVersion;
  TMemoryVersionList = Anthropic.MemoryStore.TMemoryVersionList;

  TAsynMemoryStore = Anthropic.MemoryStore.TAsynMemoryStore;
  TPromiseMemoryStore = Anthropic.MemoryStore.TPromiseMemoryStore;
  TAsynMemoryStoreList = Anthropic.MemoryStore.TAsynMemoryStoreList;
  TPromiseMemoryStoreList = Anthropic.MemoryStore.TPromiseMemoryStoreList;
  TAsynMemoryStoreDeleted = Anthropic.MemoryStore.TAsynMemoryStoreDeleted;
  TPromiseMemoryStoreDeleted = Anthropic.MemoryStore.TPromiseMemoryStoreDeleted;

  TAsynMemory = Anthropic.MemoryStore.TAsynMemory;
  TPromiseMemory = Anthropic.MemoryStore.TPromiseMemory;
  TAsynMemoryList = Anthropic.MemoryStore.TAsynMemoryList;
  TPromiseMemoryList = Anthropic.MemoryStore.TPromiseMemoryList;
  TAsynMemoryDeleted = Anthropic.MemoryStore.TAsynMemoryDeleted;
  TPromiseMemoryDeleted = Anthropic.MemoryStore.TPromiseMemoryDeleted;

  TAsynMemoryVersion = Anthropic.MemoryStore.TAsynMemoryVersion;
  TPromiseMemoryVersion = Anthropic.MemoryStore.TPromiseMemoryVersion;
  TAsynMemoryVersionList = Anthropic.MemoryStore.TAsynMemoryVersionList;
  TPromiseMemoryVersionList = Anthropic.MemoryStore.TPromiseMemoryVersionList;

  TMemoriesAbstractSupport = Anthropic.MemoryStore.TMemoriesAbstractSupport;
  TMemoriesAsynchronousSupport = Anthropic.MemoryStore.TMemoriesAsynchronousSupport;
  TMemoriesRoute = Anthropic.MemoryStore.TMemoriesRoute;

  TMemoryVersionsAbstractSupport = Anthropic.MemoryStore.TMemoryVersionsAbstractSupport;
  TMemoryVersionsAsynchronousSupport = Anthropic.MemoryStore.TMemoryVersionsAsynchronousSupport;
  TMemoryVersionsRoute = Anthropic.MemoryStore.TMemoryVersionsRoute;

  TMemoryStoresAbstractSupport = Anthropic.MemoryStore.TMemoryStoresAbstractSupport;
  TMemoryStoresAsynchronousSupport = Anthropic.MemoryStore.TMemoryStoresAsynchronousSupport;
  TMemoryStoresRoute = Anthropic.MemoryStore.TMemoryStoresRoute;

{$ENDREGION}

{$REGION 'Anthropic.Context.Helper'}

  TToolResponse = Anthropic.Context.Helper.TToolResponse;
  TTurnItem = Anthropic.Context.Helper.TTurnItem;

  TPayloadReader = Anthropic.Context.Helper.TPayloadReader;
  ITurns = Anthropic.Context.Helper.ITurns;
  TTurns = Anthropic.Context.Helper.TTurns;

{$ENDREGION}

function CurrentVersion: string;
function HttpMonitoring: IRequestMonitor;

implementation

function CurrentVersion: string;
begin
  Result := VERSION;
end;

function HttpMonitoring: IRequestMonitor;
begin
  Result := Monitoring;
end;

{ TAnthropic }

constructor TAnthropic.Create;
begin
  inherited Create;
  FAPI := TAnthropicAPI.Create;
end;

constructor TAnthropic.Create(const AToken: string);
begin
  Create;
  Token := AToken;
end;

destructor TAnthropic.Destroy;
begin
  FChatRoute.Free;
  FBatchRoute.Free;
  FFilesRoute.Free;
  FModelsRoute.Free;
  FSkillsRoute.Free;
  FAgentsRoute.Free;
  FEnvironmentsRoute.Free;
  FSessionsRoute.Free;
  FVaultsRoute.Free;
  FMemoryStoresRoute.Free;
  FAPI.Free;
  inherited;
end;

function TAnthropic.GetAgentsRoute: TAgentsRoute;
begin
  Result := Lazy<TAgentsRoute>(FAgentsRoute, FAgentsLock,
    function: TAgentsRoute
    begin
      Result := TAgentsRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetAPI: TAnthropicAPI;
begin
  Result := FAPI;
end;

function TAnthropic.GetBaseUrl: string;
begin
  Result := FAPI.BaseURL;
end;

function TAnthropic.GetBatchRoute: TBatchRoute;
begin
  Result := Lazy<TBatchRoute>(FBatchRoute, FBatchLock,
    function: TBatchRoute
    begin
      Result := TBatchRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetChatRoute: TChatRoute;
begin
  Result := Lazy<TChatRoute>(FChatRoute, FChatLock,
    function: TChatRoute
    begin
      Result := TChatRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetEnvironmentsRoute: TEnvironmentsRoute;
begin
  Result := Lazy<TEnvironmentsRoute>(FEnvironmentsRoute, FEnvironmentsLock,
    function: TEnvironmentsRoute
    begin
      Result := TEnvironmentsRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetFilesRoute: TFilesRoute;
begin
  Result := Lazy<TFilesRoute>(FFilesRoute, FFilesLock,
    function: TFilesRoute
    begin
      Result := TFilesRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetHttpClient: IHttpClientAPI;
begin
  Result := API.HttpClient;
end;

function TAnthropic.GetMemoryStoresRoute: TMemoryStoresRoute;
begin
  Result := Lazy<TMemoryStoresRoute>(FMemoryStoresRoute, FMemoryStoresLock,
    function: TMemoryStoresRoute
    begin
      Result := TMemoryStoresRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetModelsRoute: TModelsRoute;
begin
  Result := Lazy<TModelsRoute>(FModelsRoute, FModelsLock,
    function: TModelsRoute
    begin
      Result := TModelsRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetSessionsRoute: TSessionsRoute;
begin
  Result := Lazy<TSessionsRoute>(FSessionsRoute, FSessionsLock,
    function: TSessionsRoute
    begin
      Result := TSessionsRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetSkillsRoute: TSkillsRoute;
begin
  Result := Lazy<TSkillsRoute>(FSkillsRoute, FSkillsLock,
    function: TSkillsRoute
    begin
      Result := TSkillsRoute.CreateRoute(API);
    end);
end;

function TAnthropic.GetToken: string;
begin
  Result := FAPI.Token;
end;

function TAnthropic.GetVaultsRoute: TVaultsRoute;
begin
  Result := Lazy<TVaultsRoute>(FVaultsRoute, FVaultsLock,
    function: TVaultsRoute
    begin
      Result := TVaultsRoute.CreateRoute(API);
    end);
end;

procedure TAnthropic.SetBaseUrl(const Value: string);
begin
  FAPI.BaseURL := Value;
end;

procedure TAnthropic.SetToken(const Value: string);
begin
  FAPI.Token := Value;
end;

{ TAnthropicFactory }

class function TAnthropicFactory.CreateInstance(const AToken: string): IAnthropic;
begin
  Result := TAnthropic.Create(AToken);
end;

{ TLazyRouteFactory }

constructor TLazyRouteFactory.Create;
begin
  inherited Create;
  FChatLock := TObject.Create;
  FBatchLock := TObject.Create;
  FFilesLock := TObject.Create;
  FModelsLock := TObject.Create;
  FSkillsLock := TObject.Create;
  FAgentsLock := TObject.Create;
  FEnvironmentsLock := TObject.Create;
  FSessionsLock := TObject.Create;
  FVaultsLock := TObject.Create;
  FMemoryStoresLock := TObject.Create;
end;

destructor TLazyRouteFactory.Destroy;
begin
  FChatLock.Free;
  FBatchLock.Free;
  FFilesLock.Free;
  FModelsLock.Free;
  FSkillsLock.Free;
  FAgentsLock.Free;
  FEnvironmentsLock.Free;
  FSessionsLock.Free;
  FVaultsLock.Free;
  FMemoryStoresLock.Free;
  inherited;
end;

function TLazyRouteFactory.Lazy<T>(var AField: T; const ALock: TObject;
  const AFactory: TFunc<T>): T;
begin
  Result := AField;
  if Result <> nil then
    Exit;

  TMonitor.Enter(ALock);
  try
    if AField = nil then
      AField := AFactory();
    Result := AField;
  finally
    TMonitor.Exit(ALock);
  end;
end;

end.
