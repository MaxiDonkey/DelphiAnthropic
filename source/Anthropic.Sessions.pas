unit Anthropic.Sessions;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient, System.Threading,
  REST.Json.Types,
  Anthropic.API.Params, Anthropic.API, Anthropic.Types,
  Anthropic.Async.Support, Anthropic.Async.Params, Anthropic.Async.Promise;

type
  TSessionAgentParams = class(TJSONParam)
  public
    /// <summary>Sets the managed Agent reference type.</summary>
    function &Type(const Value: string = 'agent'): TSessionAgentParams;

    /// <summary>Sets the Agent identifier.</summary>
    function Id(const Value: string): TSessionAgentParams;

    /// <summary>Pins a specific Agent version. Omit to use the latest version.</summary>
    function Version(const Value: Integer): TSessionAgentParams;

    class function New: TSessionAgentParams;
  end;

  TSessionCheckoutParams = class(TJSONParam)
  public
    /// <summary>Sets the checkout discriminator.</summary>
    function &Type(const Value: string): TSessionCheckoutParams;

    class function New(const Value: string): TSessionCheckoutParams;
  end;

  TSessionBranchCheckoutParams = class(TSessionCheckoutParams)
  public
    /// <summary>Sets branch checkout.</summary>
    function &Type(const Value: string = 'branch'): TSessionBranchCheckoutParams;

    /// <summary>Sets the branch name to check out.</summary>
    function Name(const Value: string): TSessionBranchCheckoutParams;

    class function New: TSessionBranchCheckoutParams;
  end;

  TSessionCommitCheckoutParams = class(TSessionCheckoutParams)
  public
    /// <summary>Sets commit checkout.</summary>
    function &Type(const Value: string = 'commit'): TSessionCommitCheckoutParams;

    /// <summary>Sets the full commit SHA to check out.</summary>
    function Sha(const Value: string): TSessionCommitCheckoutParams;

    class function New: TSessionCommitCheckoutParams;
  end;

  TSessionResourceParams = class(TJSONParam)
  public
    /// <summary>Sets the resource discriminator.</summary>
    function &Type(const Value: string): TSessionResourceParams;

    class function New(const Value: string): TSessionResourceParams;
  end;

  TSessionGitHubRepositoryResourceParams = class(TSessionResourceParams)
  public
    /// <summary>Sets this resource as a GitHub repository mount.</summary>
    function &Type(const Value: string = 'github_repository'): TSessionGitHubRepositoryResourceParams;

    /// <summary>Sets the GitHub repository URL.</summary>
    function Url(const Value: string): TSessionGitHubRepositoryResourceParams;

    /// <summary>Sets the GitHub authorization token used to clone the repository.</summary>
    function AuthorizationToken(const Value: string): TSessionGitHubRepositoryResourceParams;

    /// <summary>Sets the optional branch or commit checkout.</summary>
    function Checkout(const Value: TSessionCheckoutParams): TSessionGitHubRepositoryResourceParams;

    /// <summary>Sets the optional mount path inside the session container.</summary>
    function MountPath(const Value: string): TSessionGitHubRepositoryResourceParams;

    class function New: TSessionGitHubRepositoryResourceParams;
  end;

  TSessionFileResourceParams = class(TSessionResourceParams)
  public
    /// <summary>Sets this resource as a Files API file mount.</summary>
    function &Type(const Value: string = 'file'): TSessionFileResourceParams;

    /// <summary>Sets the Files API file identifier.</summary>
    function FileId(const Value: string): TSessionFileResourceParams;

    /// <summary>Sets the optional mount path inside the session container.</summary>
    function MountPath(const Value: string): TSessionFileResourceParams;

    class function New: TSessionFileResourceParams;
  end;

  TSessionMemoryStoreResourceParams = class(TSessionResourceParams)
  public
    /// <summary>Sets this resource as a memory-store attachment.</summary>
    function &Type(const Value: string = 'memory_store'): TSessionMemoryStoreResourceParams;

    /// <summary>Sets the memory-store identifier.</summary>
    function MemoryStoreId(const Value: string): TSessionMemoryStoreResourceParams;

    /// <summary>Sets the access mode, for example read_write or read_only.</summary>
    function Access(const Value: string): TSessionMemoryStoreResourceParams;

    /// <summary>Sets per-attachment instructions for this memory store.</summary>
    function Instructions(const Value: string): TSessionMemoryStoreResourceParams;

    class function New: TSessionMemoryStoreResourceParams;
  end;

  TSessionCreateParams = class(TJSONParam)
  public
    /// <summary>Adds one or more beta header values to the request.</summary>
    function Beta(const Value: TArray<string>): TSessionCreateParams;

    /// <summary>Sets the Agent using the short-form Agent id.</summary>
    function Agent(const Value: string): TSessionCreateParams; overload;

    /// <summary>Sets the Agent using an explicit id/version object.</summary>
    function Agent(const Value: TSessionAgentParams): TSessionCreateParams; overload;

    /// <summary>Sets the environment defining the session container.</summary>
    function EnvironmentId(const Value: string): TSessionCreateParams;

    /// <summary>Sets arbitrary session metadata from a JSON object.</summary>
    function Metadata(const Value: TJSONObject): TSessionCreateParams; overload;

    /// <summary>Adds or replaces a single metadata key-value pair.</summary>
    function Metadata(const Key, Value: string): TSessionCreateParams; overload;

    /// <summary>Sets resources mounted at session creation time.</summary>
    function Resources(const Value: TArray<TSessionResourceParams>): TSessionCreateParams;

    /// <summary>Sets the human-readable session title.</summary>
    function Title(const Value: string): TSessionCreateParams;

    /// <summary>Sets vault ids attached at session creation time.</summary>
    function VaultIds(const Value: TArray<string>): TSessionCreateParams;

    class function New: TSessionCreateParams;
  end;

  TSessionUpdateParams = class(TJSONParam)
  public
    /// <summary>Adds one or more beta header values to the request.</summary>
    function Beta(const Value: TArray<string>): TSessionUpdateParams;

    /// <summary>Replaces the session title.</summary>
    function Title(const Value: string): TSessionUpdateParams;

    /// <summary>Patches session metadata from a JSON object.</summary>
    function Metadata(const Value: TJSONObject): TSessionUpdateParams; overload;

    /// <summary>Adds or replaces a single metadata key-value pair.</summary>
    function Metadata(const Key, Value: string): TSessionUpdateParams; overload;

    /// <summary>Deletes a metadata key by sending JSON null.</summary>
    function DeleteMetadata(const Key: string): TSessionUpdateParams;

    /// <summary>Sets vault ids. The API currently rejects this field; it is kept for forward compatibility.</summary>
    function VaultIds(const Value: TArray<string>): TSessionUpdateParams;

    class function New: TSessionUpdateParams;
  end;

  TSessionListParams = class(TUrlParam)
  public
    function AgentId(const Value: string): TSessionListParams;
    function AgentVersion(const Value: Integer): TSessionListParams;
    function CreatedAtGt(const Value: string): TSessionListParams;
    function CreatedAtGte(const Value: string): TSessionListParams;
    function CreatedAtLt(const Value: string): TSessionListParams;
    function CreatedAtLte(const Value: string): TSessionListParams;
    function IncludeArchived(const Value: Boolean): TSessionListParams;
    function Limit(const Value: Integer): TSessionListParams;
    function MemoryStoreId(const Value: string): TSessionListParams;
    function Order(const Value: string): TSessionListParams;
    function Page(const Value: string): TSessionListParams;
    function Statuses(const Value: TArray<string>): TSessionListParams;

    class function New: TSessionListParams;
  end;

  TSessionEventListParams = class(TUrlParam)
  public
    function CreatedAtGt(const Value: string): TSessionEventListParams;
    function CreatedAtGte(const Value: string): TSessionEventListParams;
    function CreatedAtLt(const Value: string): TSessionEventListParams;
    function CreatedAtLte(const Value: string): TSessionEventListParams;
    function Limit(const Value: Integer): TSessionEventListParams;
    function Order(const Value: string): TSessionEventListParams;
    function Page(const Value: string): TSessionEventListParams;
    function Types(const Value: TArray<string>): TSessionEventListParams;

    class function New: TSessionEventListParams;
  end;

  TSessionSimpleListParams = class(TUrlParam)
  public
    function Limit(const Value: Integer): TSessionSimpleListParams;
    function Page(const Value: string): TSessionSimpleListParams;

    class function New: TSessionSimpleListParams;
  end;

  TSessionSourceParams = class(TJSONParam)
  public
    function &Type(const Value: string): TSessionSourceParams;

    class function New(const Value: string): TSessionSourceParams;
  end;

  TSessionBase64ImageSourceParams = class(TSessionSourceParams)
  public
    function &Type(const Value: string = 'base64'): TSessionBase64ImageSourceParams;
    function Data(const Value: string): TSessionBase64ImageSourceParams;
    function MediaType(const Value: string): TSessionBase64ImageSourceParams;

    class function New: TSessionBase64ImageSourceParams;
  end;

  TSessionUrlSourceParams = class(TSessionSourceParams)
  public
    function &Type(const Value: string = 'url'): TSessionUrlSourceParams;
    function Url(const Value: string): TSessionUrlSourceParams;

    class function New: TSessionUrlSourceParams;
  end;

  TSessionFileSourceParams = class(TSessionSourceParams)
  public
    function &Type(const Value: string = 'file'): TSessionFileSourceParams;
    function FileId(const Value: string): TSessionFileSourceParams;

    class function New: TSessionFileSourceParams;
  end;

  TSessionBase64DocumentSourceParams = class(TSessionSourceParams)
  public
    function &Type(const Value: string = 'base64'): TSessionBase64DocumentSourceParams;
    function Data(const Value: string): TSessionBase64DocumentSourceParams;
    function MediaType(const Value: string): TSessionBase64DocumentSourceParams;

    class function New: TSessionBase64DocumentSourceParams;
  end;

  TSessionPlainTextDocumentSourceParams = class(TSessionSourceParams)
  public
    function &Type(const Value: string = 'text'): TSessionPlainTextDocumentSourceParams;
    function Data(const Value: string): TSessionPlainTextDocumentSourceParams;
    function MediaType(const Value: string = 'text/plain'): TSessionPlainTextDocumentSourceParams;

    class function New: TSessionPlainTextDocumentSourceParams;
  end;

  TSessionContentBlockParams = class(TJSONParam)
  public
    function &Type(const Value: string): TSessionContentBlockParams;

    class function New(const Value: string): TSessionContentBlockParams;
  end;

  TSessionTextBlockParams = class(TSessionContentBlockParams)
  public
    function &Type(const Value: string = 'text'): TSessionTextBlockParams;
    function Text(const Value: string): TSessionTextBlockParams;

    class function New: TSessionTextBlockParams;
  end;

  TSessionImageBlockParams = class(TSessionContentBlockParams)
  public
    function &Type(const Value: string = 'image'): TSessionImageBlockParams;
    function Source(const Value: TSessionSourceParams): TSessionImageBlockParams;

    class function New: TSessionImageBlockParams;
  end;

  TSessionDocumentBlockParams = class(TSessionContentBlockParams)
  public
    function &Type(const Value: string = 'document'): TSessionDocumentBlockParams;
    function Source(const Value: TSessionSourceParams): TSessionDocumentBlockParams;
    function Context(const Value: string): TSessionDocumentBlockParams;
    function Title(const Value: string): TSessionDocumentBlockParams;

    class function New: TSessionDocumentBlockParams;
  end;

  TSessionEventParams = class(TJSONParam)
  public
    function &Type(const Value: string): TSessionEventParams;

    class function New(const Value: string): TSessionEventParams;
  end;

  TSessionUserMessageEventParams = class(TSessionEventParams)
  public
    function &Type(const Value: string = 'user.message'): TSessionUserMessageEventParams;
    function Content(const Value: TArray<TSessionContentBlockParams>): TSessionUserMessageEventParams;
    function Text(const Value: string): TSessionUserMessageEventParams;

    class function New: TSessionUserMessageEventParams;
  end;

  TSessionUserInterruptEventParams = class(TSessionEventParams)
  public
    function &Type(const Value: string = 'user.interrupt'): TSessionUserInterruptEventParams;
    function SessionThreadId(const Value: string): TSessionUserInterruptEventParams;

    class function New: TSessionUserInterruptEventParams;
  end;

  TSessionUserToolConfirmationEventParams = class(TSessionEventParams)
  public
    function &Type(const Value: string = 'user.tool_confirmation'): TSessionUserToolConfirmationEventParams;
    function Result(const Value: string): TSessionUserToolConfirmationEventParams;
    function ToolUseId(const Value: string): TSessionUserToolConfirmationEventParams;
    function DenyMessage(const Value: string): TSessionUserToolConfirmationEventParams;
    function SessionThreadId(const Value: string): TSessionUserToolConfirmationEventParams;

    class function New: TSessionUserToolConfirmationEventParams;
  end;

  TSessionUserCustomToolResultEventParams = class(TSessionEventParams)
  public
    function &Type(const Value: string = 'user.custom_tool_result'): TSessionUserCustomToolResultEventParams;
    function CustomToolUseId(const Value: string): TSessionUserCustomToolResultEventParams;
    function Content(const Value: TArray<TSessionContentBlockParams>): TSessionUserCustomToolResultEventParams;
    function IsError(const Value: Boolean): TSessionUserCustomToolResultEventParams;
    function SessionThreadId(const Value: string): TSessionUserCustomToolResultEventParams;

    class function New: TSessionUserCustomToolResultEventParams;
  end;

  TSessionRubricParams = class(TJSONParam)
  public
    function &Type(const Value: string): TSessionRubricParams;

    class function New(const Value: string): TSessionRubricParams;
  end;

  TSessionTextRubricParams = class(TSessionRubricParams)
  public
    function &Type(const Value: string = 'text'): TSessionTextRubricParams;
    function Content(const Value: string): TSessionTextRubricParams;

    class function New: TSessionTextRubricParams;
  end;

  TSessionFileRubricParams = class(TSessionRubricParams)
  public
    function &Type(const Value: string = 'file'): TSessionFileRubricParams;
    function FileId(const Value: string): TSessionFileRubricParams;

    class function New: TSessionFileRubricParams;
  end;

  TSessionUserDefineOutcomeEventParams = class(TSessionEventParams)
  public
    function &Type(const Value: string = 'user.define_outcome'): TSessionUserDefineOutcomeEventParams;
    function Description(const Value: string): TSessionUserDefineOutcomeEventParams;
    function Rubric(const Value: TSessionRubricParams): TSessionUserDefineOutcomeEventParams;
    function MaxIterations(const Value: Integer): TSessionUserDefineOutcomeEventParams;

    class function New: TSessionUserDefineOutcomeEventParams;
  end;

  TSessionSendEventsParams = class(TJSONParam)
  public
    /// <summary>Adds one or more beta header values to the request.</summary>
    function Beta(const Value: TArray<string>): TSessionSendEventsParams;

    /// <summary>Sets the user-side events to send to the session.</summary>
    function Events(const Value: TArray<TSessionEventParams>): TSessionSendEventsParams;

    class function New: TSessionSendEventsParams;
  end;

  TSessionResourceUpdateParams = class(TJSONParam)
  public
    /// <summary>Adds one or more beta header values to the request.</summary>
    function Beta(const Value: TArray<string>): TSessionResourceUpdateParams;

    /// <summary>Rotates the GitHub authorization token for a github_repository resource.</summary>
    function AuthorizationToken(const Value: string): TSessionResourceUpdateParams;

    class function New: TSessionResourceUpdateParams;
  end;

  TSessionListParamProc = TProc<TSessionListParams>;
  TSessionCreateParamProc = TProc<TSessionCreateParams>;
  TSessionUpdateParamProc = TProc<TSessionUpdateParams>;
  TSessionEventListParamProc = TProc<TSessionEventListParams>;
  TSessionSimpleListParamProc = TProc<TSessionSimpleListParams>;
  TSessionSendEventsParamProc = TProc<TSessionSendEventsParams>;
  TSessionResourceAddParamProc = TProc<TSessionFileResourceParams>;
  TSessionResourceUpdateParamProc = TProc<TSessionResourceUpdateParams>;
  TSessionStreamEvent = reference to procedure(const Data: string; var Abort: Boolean);

  TSessionModelConfig = class(TJSONFingerprint)
  private
    FId: string;
    FSpeed: string;
  public
    property Id: string read FId write FId;
    property Speed: string read FSpeed write FSpeed;
  end;

  TSessionAgent = class(TJSONFingerprint)
  private
    FId: string;
    FDescription: string;
    [JSONMarshalled(False)]
    [JsonNameAttribute('mcp_servers')]
    FMCPServers: string;
    FModel: TSessionModelConfig;
    [JSONMarshalled(False)]
    FMultiagent: string;
    FName: string;
    [JSONMarshalled(False)]
    FSkills: string;
    FSystem: string;
    [JSONMarshalled(False)]
    FTools: string;
    FType: string;
    FVersion: Integer;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Id: string read FId write FId;
    property Description: string read FDescription write FDescription;
    property MCPServers: string read FMCPServers write FMCPServers;
    property Model: TSessionModelConfig read FModel write FModel;
    property Multiagent: string read FMultiagent write FMultiagent;
    property Name: string read FName write FName;
    property Skills: string read FSkills write FSkills;
    property System: string read FSystem write FSystem;
    property Tools: string read FTools write FTools;
    property &Type: string read FType write FType;
    property Version: Integer read FVersion write FVersion;
    destructor Destroy; override;
  end;

  TSessionCacheCreationUsage = class(TJSONFingerprint)
  private
    [JsonNameAttribute('ephemeral_1h_input_tokens')]
    FEphemeral1hInputTokens: Integer;
    [JsonNameAttribute('ephemeral_5m_input_tokens')]
    FEphemeral5mInputTokens: Integer;
  public
    property Ephemeral1hInputTokens: Integer read FEphemeral1hInputTokens write FEphemeral1hInputTokens;
    property Ephemeral5mInputTokens: Integer read FEphemeral5mInputTokens write FEphemeral5mInputTokens;
  end;

  TSessionUsage = class(TJSONFingerprint)
  private
    [JsonNameAttribute('cache_creation')]
    FCacheCreation: TSessionCacheCreationUsage;
    [JsonNameAttribute('cache_read_input_tokens')]
    FCacheReadInputTokens: Integer;
    [JsonNameAttribute('input_tokens')]
    FInputTokens: Integer;
    [JsonNameAttribute('output_tokens')]
    FOutputTokens: Integer;
  public
    property CacheCreation: TSessionCacheCreationUsage read FCacheCreation write FCacheCreation;
    property CacheReadInputTokens: Integer read FCacheReadInputTokens write FCacheReadInputTokens;
    property InputTokens: Integer read FInputTokens write FInputTokens;
    property OutputTokens: Integer read FOutputTokens write FOutputTokens;
    destructor Destroy; override;
  end;

  TSessionStats = class(TJSONFingerprint)
  private
    [JsonNameAttribute('active_seconds')]
    FActiveSeconds: Double;
    [JsonNameAttribute('duration_seconds')]
    FDurationSeconds: Double;
    [JsonNameAttribute('startup_seconds')]
    FStartupSeconds: Double;
  public
    property ActiveSeconds: Double read FActiveSeconds write FActiveSeconds;
    property DurationSeconds: Double read FDurationSeconds write FDurationSeconds;
    property StartupSeconds: Double read FStartupSeconds write FStartupSeconds;
  end;

  TSessionResource = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    [JsonNameAttribute('mount_path')]
    FMountPath: string;
    FType: string;
    FUrl: string;
    [JsonNameAttribute('file_id')]
    FFileId: string;
    [JsonNameAttribute('memory_store_id')]
    FMemoryStoreId: string;
    FAccess: string;
    FDescription: string;
    FInstructions: string;
    FName: string;
    [JSONMarshalled(False)]
    FCheckout: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Id: string read FId write FId;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
    property MountPath: string read FMountPath write FMountPath;
    property &Type: string read FType write FType;
    property Url: string read FUrl write FUrl;
    property FileId: string read FFileId write FFileId;
    property MemoryStoreId: string read FMemoryStoreId write FMemoryStoreId;
    property Access: string read FAccess write FAccess;
    property Description: string read FDescription write FDescription;
    property Instructions: string read FInstructions write FInstructions;
    property Name: string read FName write FName;
    property Checkout: string read FCheckout write FCheckout;
    function IsGitHubRepository: Boolean;
    function IsFile: Boolean;
    function IsMemoryStore: Boolean;
  end;

  TSessionOutcomeEvaluation = class(TJSONFingerprint)
  private
    [JsonNameAttribute('completed_at')]
    FCompletedAt: string;
    FDescription: string;
    FExplanation: string;
    FIteration: Integer;
    [JsonNameAttribute('outcome_id')]
    FOutcomeId: string;
    FResult: string;
    FType: string;
  public
    property CompletedAt: string read FCompletedAt write FCompletedAt;
    property Description: string read FDescription write FDescription;
    property Explanation: string read FExplanation write FExplanation;
    property Iteration: Integer read FIteration write FIteration;
    property OutcomeId: string read FOutcomeId write FOutcomeId;
    property Result: string read FResult write FResult;
    property &Type: string read FType write FType;
  end;

  TSession = class(TJSONFingerprint)
  private
    FId: string;
    FAgent: TSessionAgent;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('environment_id')]
    FEnvironmentId: string;
    [JSONMarshalled(False)]
    FMetadata: string;
    [JsonNameAttribute('outcome_evaluations')]
    FOutcomeEvaluations: TArray<TSessionOutcomeEvaluation>;
    FResources: TArray<TSessionResource>;
    FStats: TSessionStats;
    FStatus: string;
    FTitle: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    FUsage: TSessionUsage;
    [JsonNameAttribute('vault_ids')]
    FVaultIds: TArray<string>;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Id: string read FId write FId;
    property Agent: TSessionAgent read FAgent write FAgent;
    property ArchivedAt: string read FArchivedAt write FArchivedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property EnvironmentId: string read FEnvironmentId write FEnvironmentId;
    property Metadata: string read FMetadata write FMetadata;
    property OutcomeEvaluations: TArray<TSessionOutcomeEvaluation> read FOutcomeEvaluations write FOutcomeEvaluations;
    property Resources: TArray<TSessionResource> read FResources write FResources;
    property Stats: TSessionStats read FStats write FStats;
    property Status: string read FStatus write FStatus;
    property Title: string read FTitle write FTitle;
    property &Type: string read FType write FType;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
    property Usage: TSessionUsage read FUsage write FUsage;
    property VaultIds: TArray<string> read FVaultIds write FVaultIds;
    destructor Destroy; override;
  end;

  TSessionList = class(TJSONFingerprint)
  private
    FData: TArray<TSession>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Data: TArray<TSession> read FData write FData;
    property NextPage: string read FNextPage write FNextPage;
    destructor Destroy; override;
  end;

  TSessionDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    property Id: string read FId write FId;
    property &Type: string read FType write FType;
  end;

  TSessionEvent = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
    [JsonNameAttribute('processed_at')]
    FProcessedAt: string;
    [JSONMarshalled(False)]
    FContent: string;
    FResult: string;
    [JsonNameAttribute('tool_use_id')]
    FToolUseId: string;
    [JsonNameAttribute('deny_message')]
    FDenyMessage: string;
    [JsonNameAttribute('custom_tool_use_id')]
    FCustomToolUseId: string;
    [JSONMarshalled(False)]
    FInput: string;
    FName: string;
    [JsonNameAttribute('mcp_server_name')]
    FMCPServerName: string;
    [JsonNameAttribute('evaluated_permission')]
    FEvaluatedPermission: string;
    [JsonNameAttribute('session_thread_id')]
    FSessionThreadId: string;
    [JsonNameAttribute('mcp_tool_use_id')]
    FMCPToolUseId: string;
    [JsonNameAttribute('is_error')]
    FIsError: Boolean;
    [JsonNameAttribute('from_session_thread_id')]
    FFromSessionThreadId: string;
    [JsonNameAttribute('to_session_thread_id')]
    FToSessionThreadId: string;
    [JsonNameAttribute('from_agent_name')]
    FFromAgentName: string;
    [JsonNameAttribute('to_agent_name')]
    FToAgentName: string;
    [JSONMarshalled(False)]
    FError: string;
    [JSONMarshalled(False)]
    [JsonNameAttribute('stop_reason')]
    FStopReason: string;
    [JsonNameAttribute('agent_name')]
    FAgentName: string;
    FIteration: Integer;
    [JsonNameAttribute('outcome_id')]
    FOutcomeId: string;
    FExplanation: string;
    [JsonNameAttribute('outcome_evaluation_start_id')]
    FOutcomeEvaluationStartId: string;
    [JSONMarshalled(False)]
    FUsage: string;
    [JsonNameAttribute('model_request_start_id')]
    FModelRequestStartId: string;
    [JSONMarshalled(False)]
    [JsonNameAttribute('model_usage')]
    FModelUsage: string;
    FDescription: string;
    [JSONMarshalled(False)]
    FRubric: string;
    [JsonNameAttribute('max_iterations')]
    FMaxIterations: Integer;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Id: string read FId write FId;
    property &Type: string read FType write FType;
    property ProcessedAt: string read FProcessedAt write FProcessedAt;
    property Content: string read FContent write FContent;
    property Result: string read FResult write FResult;
    property ToolUseId: string read FToolUseId write FToolUseId;
    property DenyMessage: string read FDenyMessage write FDenyMessage;
    property CustomToolUseId: string read FCustomToolUseId write FCustomToolUseId;
    property Input: string read FInput write FInput;
    property Name: string read FName write FName;
    property MCPServerName: string read FMCPServerName write FMCPServerName;
    property EvaluatedPermission: string read FEvaluatedPermission write FEvaluatedPermission;
    property SessionThreadId: string read FSessionThreadId write FSessionThreadId;
    property MCPToolUseId: string read FMCPToolUseId write FMCPToolUseId;
    property IsError: Boolean read FIsError write FIsError;
    property FromSessionThreadId: string read FFromSessionThreadId write FFromSessionThreadId;
    property ToSessionThreadId: string read FToSessionThreadId write FToSessionThreadId;
    property FromAgentName: string read FFromAgentName write FFromAgentName;
    property ToAgentName: string read FToAgentName write FToAgentName;
    property Error: string read FError write FError;
    property StopReason: string read FStopReason write FStopReason;
    property AgentName: string read FAgentName write FAgentName;
    property Iteration: Integer read FIteration write FIteration;
    property OutcomeId: string read FOutcomeId write FOutcomeId;
    property Explanation: string read FExplanation write FExplanation;
    property OutcomeEvaluationStartId: string read FOutcomeEvaluationStartId write FOutcomeEvaluationStartId;
    property Usage: string read FUsage write FUsage;
    property ModelRequestStartId: string read FModelRequestStartId write FModelRequestStartId;
    property ModelUsage: string read FModelUsage write FModelUsage;
    property Description: string read FDescription write FDescription;
    property Rubric: string read FRubric write FRubric;
    property MaxIterations: Integer read FMaxIterations write FMaxIterations;
  end;

  TSessionEventList = class(TJSONFingerprint)
  private
    FData: TArray<TSessionEvent>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Data: TArray<TSessionEvent> read FData write FData;
    property NextPage: string read FNextPage write FNextPage;
    destructor Destroy; override;
  end;

  TSessionSendEventsResponse = class(TJSONFingerprint)
  private
    FData: TArray<TSessionEvent>;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Data: TArray<TSessionEvent> read FData write FData;
    destructor Destroy; override;
  end;

  TSessionResourceList = class(TJSONFingerprint)
  private
    FData: TArray<TSessionResource>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Data: TArray<TSessionResource> read FData write FData;
    property NextPage: string read FNextPage write FNextPage;
    destructor Destroy; override;
  end;

  TSessionResourceDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    property Id: string read FId write FId;
    property &Type: string read FType write FType;
  end;

  TSessionThread = class(TJSONFingerprint)
  private
    FId: string;
    FAgent: TSessionAgent;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('parent_thread_id')]
    FParentThreadId: string;
    [JsonNameAttribute('session_id')]
    FSessionId: string;
    FStats: TSessionStats;
    FStatus: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    FUsage: TSessionUsage;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Id: string read FId write FId;
    property Agent: TSessionAgent read FAgent write FAgent;
    property ArchivedAt: string read FArchivedAt write FArchivedAt;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property ParentThreadId: string read FParentThreadId write FParentThreadId;
    property SessionId: string read FSessionId write FSessionId;
    property Stats: TSessionStats read FStats write FStats;
    property Status: string read FStatus write FStatus;
    property &Type: string read FType write FType;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
    property Usage: TSessionUsage read FUsage write FUsage;
    destructor Destroy; override;
  end;

  TSessionThreadList = class(TJSONFingerprint)
  private
    FData: TArray<TSessionThread>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    property Data: TArray<TSessionThread> read FData write FData;
    property NextPage: string read FNextPage write FNextPage;
    destructor Destroy; override;
  end;

  TSessionStreamStatus = class(TJSONFingerprint)
  private
    FStatusCode: Integer;
    FContentLength: Int64;
    FReadCount: Int64;
    FDone: Boolean;
  public
    class function New(const AStatusCode: Integer; const AContentLength,
      AReadCount: Int64; const ADone: Boolean): TSessionStreamStatus; static;

    property StatusCode: Integer read FStatusCode write FStatusCode;
    property ContentLength: Int64 read FContentLength write FContentLength;
    property ReadCount: Int64 read FReadCount write FReadCount;
    property Done: Boolean read FDone write FDone;
  end;

  TSessionStream = class(TJSONFingerprint)
  private
    FData: string;
  public
    constructor Create; overload;
    constructor Create(const Value: string); overload;

    /// <summary>Raw Server-Sent Events payload returned by the stream endpoint.</summary>
    property Data: string read FData write FData;

    /// <summary>Alias for Data, useful when the raw stream is consumed as a scalar async result.</summary>
    property Value: string read FData write FData;
  end;

  TAsynSession = TAsynCallBack<TSession>;
  TPromiseSession = TPromiseCallback<TSession>;
  TAsynSessionList = TAsynCallBack<TSessionList>;
  TPromiseSessionList = TPromiseCallback<TSessionList>;
  TAsynSessionDeleted = TAsynCallBack<TSessionDeleted>;
  TPromiseSessionDeleted = TPromiseCallback<TSessionDeleted>;
  TAsynSessionEventList = TAsynCallBack<TSessionEventList>;
  TPromiseSessionEventList = TPromiseCallback<TSessionEventList>;
  TAsynSessionSendEventsResponse = TAsynCallBack<TSessionSendEventsResponse>;
  TPromiseSessionSendEventsResponse = TPromiseCallback<TSessionSendEventsResponse>;
  TAsynSessionResource = TAsynCallBack<TSessionResource>;
  TPromiseSessionResource = TPromiseCallback<TSessionResource>;
  TAsynSessionResourceList = TAsynCallBack<TSessionResourceList>;
  TPromiseSessionResourceList = TPromiseCallback<TSessionResourceList>;
  TAsynSessionResourceDeleted = TAsynCallBack<TSessionResourceDeleted>;
  TPromiseSessionResourceDeleted = TPromiseCallback<TSessionResourceDeleted>;
  TAsynSessionThread = TAsynCallBack<TSessionThread>;
  TPromiseSessionThread = TPromiseCallback<TSessionThread>;
  TAsynSessionThreadList = TAsynCallBack<TSessionThreadList>;
  TPromiseSessionThreadList = TPromiseCallback<TSessionThreadList>;
  TAsynSessionStream = TAsynCallBack<TSessionStream>;
  TPromiseSessionStream = TPromiseCallback<TSessionStream>;
  TAsynSessionStreamStatus = TAsynStreamCallBack<TSessionStreamStatus>;
  TPromiseSessionStreamStatus = TPromiseStreamCallBack<TSessionStreamStatus>;

  TSessionEventsRoute = class;
  TSessionResourcesRoute = class;
  TSessionThreadsRoute = class;
  TSessionThreadEventsRoute = class;

  TSessionsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const ParamProc: TSessionCreateParamProc): TSession; overload; virtual; abstract;
    function List: TSessionList; overload; virtual; abstract;
    function List(const ParamProc: TSessionListParamProc): TSessionList; overload; virtual; abstract;
    function Retrieve(const SessionId: string): TSession; overload; virtual; abstract;
    function Update(const SessionId: string; const ParamProc: TSessionUpdateParamProc): TSession; overload; virtual; abstract;
    function Delete(const SessionId: string): TSessionDeleted; overload; virtual; abstract;
    function Archive(const SessionId: string): TSession; overload; virtual; abstract;
  end;

  TSessionsAsynchronousSupport = class(TSessionsAbstractSupport)
  protected
    procedure AsynCreate(const ParamProc: TSessionCreateParamProc; const CallBacks: TFunc<TAsynSession>); overload;
    procedure AsynList(const CallBacks: TFunc<TAsynSessionList>); overload;
    procedure AsynList(const ParamProc: TSessionListParamProc; const CallBacks: TFunc<TAsynSessionList>); overload;
    procedure AsynRetrieve(const SessionId: string; const CallBacks: TFunc<TAsynSession>); overload;
    procedure AsynUpdate(const SessionId: string; const ParamProc: TSessionUpdateParamProc; const CallBacks: TFunc<TAsynSession>); overload;
    procedure AsynDelete(const SessionId: string; const CallBacks: TFunc<TAsynSessionDeleted>); overload;
    procedure AsynArchive(const SessionId: string; const CallBacks: TFunc<TAsynSession>); overload;
  end;

  TSessionsRoute = class(TSessionsAsynchronousSupport)
  private
    FEvents: TSessionEventsRoute;
    FResources: TSessionResourcesRoute;
    FThreads: TSessionThreadsRoute;
    function GetEvents: TSessionEventsRoute;
    function GetResources: TSessionResourcesRoute;
    function GetThreads: TSessionThreadsRoute;
  public
    /// <summary>Sub-route for /sessions/{session_id}/events.</summary>
    property Events: TSessionEventsRoute read GetEvents;

    /// <summary>Sub-route for /sessions/{session_id}/resources.</summary>
    property Resources: TSessionResourcesRoute read GetResources;

    /// <summary>Sub-route for /sessions/{session_id}/threads.</summary>
    property Threads: TSessionThreadsRoute read GetThreads;

    function Create(const ParamProc: TSessionCreateParamProc): TSession; overload; override;
    function List: TSessionList; overload; override;
    function List(const ParamProc: TSessionListParamProc): TSessionList; overload; override;
    function Retrieve(const SessionId: string): TSession; overload; override;
    function Update(const SessionId: string; const ParamProc: TSessionUpdateParamProc): TSession; overload; override;
    function Delete(const SessionId: string): TSessionDeleted; overload; override;
    function Archive(const SessionId: string): TSession; overload; override;

    function AsyncAwaitCreate(const ParamProc: TSessionCreateParamProc; const Callbacks: TFunc<TPromiseSession> = nil): TPromise<TSession>; overload;
    function AsyncAwaitList(const Callbacks: TFunc<TPromiseSessionList> = nil): TPromise<TSessionList>; overload;
    function AsyncAwaitList(const ParamProc: TSessionListParamProc; const Callbacks: TFunc<TPromiseSessionList> = nil): TPromise<TSessionList>; overload;
    function AsyncAwaitRetrieve(const SessionId: string; const Callbacks: TFunc<TPromiseSession> = nil): TPromise<TSession>; overload;
    function AsyncAwaitUpdate(const SessionId: string; const ParamProc: TSessionUpdateParamProc; const Callbacks: TFunc<TPromiseSession> = nil): TPromise<TSession>; overload;
    function AsyncAwaitDelete(const SessionId: string; const Callbacks: TFunc<TPromiseSessionDeleted> = nil): TPromise<TSessionDeleted>; overload;
    function AsyncAwaitArchive(const SessionId: string; const Callbacks: TFunc<TPromiseSession> = nil): TPromise<TSession>; overload;

    constructor CreateRoute(AAPI: TAnthropicAPI); reintroduce;
    destructor Destroy; override;
  end;

  TSessionEventsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function List(const SessionId: string): TSessionEventList; overload; virtual; abstract;
    function List(const SessionId: string; const ParamProc: TSessionEventListParamProc): TSessionEventList; overload; virtual; abstract;
    function Send(const SessionId: string; const ParamProc: TSessionSendEventsParamProc): TSessionSendEventsResponse; overload; virtual; abstract;
    function StreamRaw(const SessionId: string): TSessionStream; overload; virtual; abstract;
    function StreamRaw(const SessionId: string; const Response: TStream;
      Event: TReceiveDataCallback): Integer; overload; virtual; abstract;
    function StreamEvents(const SessionId: string; const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback): Integer; overload; virtual; abstract;
  end;

  TSessionEventsAsynchronousSupport = class(TSessionEventsAbstractSupport)
  protected
    procedure AsynList(const SessionId: string; const CallBacks: TFunc<TAsynSessionEventList>); overload;
    procedure AsynList(const SessionId: string; const ParamProc: TSessionEventListParamProc; const CallBacks: TFunc<TAsynSessionEventList>); overload;
    procedure AsynSend(const SessionId: string; const ParamProc: TSessionSendEventsParamProc; const CallBacks: TFunc<TAsynSessionSendEventsResponse>); overload;
    procedure AsynStreamRaw(const SessionId: string; const CallBacks: TFunc<TAsynSessionStream>); overload;
    procedure AsynStreamRaw(const SessionId: string; const Response: TStream;
      Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
    procedure AsynStreamEvents(const SessionId: string; const Event: TSessionStreamEvent;
      const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
  end;

  TSessionEventsRoute = class(TSessionEventsAsynchronousSupport)
  public
    function List(const SessionId: string): TSessionEventList; overload; override;
    function List(const SessionId: string; const ParamProc: TSessionEventListParamProc): TSessionEventList; overload; override;
    function Send(const SessionId: string; const ParamProc: TSessionSendEventsParamProc): TSessionSendEventsResponse; overload; override;
    function StreamRaw(const SessionId: string): TSessionStream; overload; override;
    function StreamRaw(const SessionId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil): Integer; overload; override;
    function StreamEvents(const SessionId: string; const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback = nil): Integer; overload; override;

    function AsyncAwaitList(const SessionId: string; const Callbacks: TFunc<TPromiseSessionEventList> = nil): TPromise<TSessionEventList>; overload;
    function AsyncAwaitList(const SessionId: string; const ParamProc: TSessionEventListParamProc; const Callbacks: TFunc<TPromiseSessionEventList> = nil): TPromise<TSessionEventList>; overload;
    function AsyncAwaitSend(const SessionId: string; const ParamProc: TSessionSendEventsParamProc; const Callbacks: TFunc<TPromiseSessionSendEventsResponse> = nil): TPromise<TSessionSendEventsResponse>; overload;
    function AsyncAwaitStreamRaw(const SessionId: string; const Callbacks: TFunc<TPromiseSessionStream> = nil): TPromise<TSessionStream>; overload;
    function AsyncAwaitStreamRaw(const SessionId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;
    function AsyncAwaitStreamEvents(const SessionId: string; const Event: TSessionStreamEvent;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;
  end;

  TSessionResourcesAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Add(const SessionId: string; const ParamProc: TSessionResourceAddParamProc): TSessionResource; overload; virtual; abstract;
    function List(const SessionId: string): TSessionResourceList; overload; virtual; abstract;
    function List(const SessionId: string; const ParamProc: TSessionSimpleListParamProc): TSessionResourceList; overload; virtual; abstract;
    function Retrieve(const SessionId, ResourceId: string): TSessionResource; overload; virtual; abstract;
    function Update(const SessionId, ResourceId: string; const ParamProc: TSessionResourceUpdateParamProc): TSessionResource; overload; virtual; abstract;
    function Delete(const SessionId, ResourceId: string): TSessionResourceDeleted; overload; virtual; abstract;
  end;

  TSessionResourcesAsynchronousSupport = class(TSessionResourcesAbstractSupport)
  protected
    procedure AsynAdd(const SessionId: string; const ParamProc: TSessionResourceAddParamProc; const CallBacks: TFunc<TAsynSessionResource>); overload;
    procedure AsynList(const SessionId: string; const CallBacks: TFunc<TAsynSessionResourceList>); overload;
    procedure AsynList(const SessionId: string; const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionResourceList>); overload;
    procedure AsynRetrieve(const SessionId, ResourceId: string; const CallBacks: TFunc<TAsynSessionResource>); overload;
    procedure AsynUpdate(const SessionId, ResourceId: string; const ParamProc: TSessionResourceUpdateParamProc; const CallBacks: TFunc<TAsynSessionResource>); overload;
    procedure AsynDelete(const SessionId, ResourceId: string; const CallBacks: TFunc<TAsynSessionResourceDeleted>); overload;
  end;

  TSessionResourcesRoute = class(TSessionResourcesAsynchronousSupport)
  public
    function Add(const SessionId: string; const ParamProc: TSessionResourceAddParamProc): TSessionResource; overload; override;
    function List(const SessionId: string): TSessionResourceList; overload; override;
    function List(const SessionId: string; const ParamProc: TSessionSimpleListParamProc): TSessionResourceList; overload; override;
    function Retrieve(const SessionId, ResourceId: string): TSessionResource; overload; override;
    function Update(const SessionId, ResourceId: string; const ParamProc: TSessionResourceUpdateParamProc): TSessionResource; overload; override;
    function Delete(const SessionId, ResourceId: string): TSessionResourceDeleted; overload; override;

    function AsyncAwaitAdd(const SessionId: string; const ParamProc: TSessionResourceAddParamProc; const Callbacks: TFunc<TPromiseSessionResource> = nil): TPromise<TSessionResource>; overload;
    function AsyncAwaitList(const SessionId: string; const Callbacks: TFunc<TPromiseSessionResourceList> = nil): TPromise<TSessionResourceList>; overload;
    function AsyncAwaitList(const SessionId: string; const ParamProc: TSessionSimpleListParamProc; const Callbacks: TFunc<TPromiseSessionResourceList> = nil): TPromise<TSessionResourceList>; overload;
    function AsyncAwaitRetrieve(const SessionId, ResourceId: string; const Callbacks: TFunc<TPromiseSessionResource> = nil): TPromise<TSessionResource>; overload;
    function AsyncAwaitUpdate(const SessionId, ResourceId: string; const ParamProc: TSessionResourceUpdateParamProc; const Callbacks: TFunc<TPromiseSessionResource> = nil): TPromise<TSessionResource>; overload;
    function AsyncAwaitDelete(const SessionId, ResourceId: string; const Callbacks: TFunc<TPromiseSessionResourceDeleted> = nil): TPromise<TSessionResourceDeleted>; overload;
  end;

  TSessionThreadsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function List(const SessionId: string): TSessionThreadList; overload; virtual; abstract;
    function List(const SessionId: string; const ParamProc: TSessionSimpleListParamProc): TSessionThreadList; overload; virtual; abstract;
    function Retrieve(const SessionId, ThreadId: string): TSessionThread; overload; virtual; abstract;
    function StreamRaw(const SessionId, ThreadId: string): TSessionStream; overload; virtual; abstract;
    function StreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback): Integer; overload; virtual; abstract;
    function StreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback): Integer; overload; virtual; abstract;
  end;

  TSessionThreadsAsynchronousSupport = class(TSessionThreadsAbstractSupport)
  protected
    procedure AsynList(const SessionId: string; const CallBacks: TFunc<TAsynSessionThreadList>); overload;
    procedure AsynList(const SessionId: string; const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionThreadList>); overload;
    procedure AsynRetrieve(const SessionId, ThreadId: string; const CallBacks: TFunc<TAsynSessionThread>); overload;
    procedure AsynStreamRaw(const SessionId, ThreadId: string; const CallBacks: TFunc<TAsynSessionStream>); overload;
    procedure AsynStreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
    procedure AsynStreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
  end;

  TSessionThreadsRoute = class(TSessionThreadsAsynchronousSupport)
  private
    FEvents: TSessionThreadEventsRoute;
    function GetEvents: TSessionThreadEventsRoute;
  public
    /// <summary>Sub-route for /sessions/{session_id}/threads/{thread_id}/events.</summary>
    property Events: TSessionThreadEventsRoute read GetEvents;

    function List(const SessionId: string): TSessionThreadList; overload; override;
    function List(const SessionId: string; const ParamProc: TSessionSimpleListParamProc): TSessionThreadList; overload; override;
    function Retrieve(const SessionId, ThreadId: string): TSessionThread; overload; override;
    function StreamRaw(const SessionId, ThreadId: string): TSessionStream; overload; override;
    function StreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil): Integer; overload; override;
    function StreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback = nil): Integer; overload; override;

    function AsyncAwaitList(const SessionId: string; const Callbacks: TFunc<TPromiseSessionThreadList> = nil): TPromise<TSessionThreadList>; overload;
    function AsyncAwaitList(const SessionId: string; const ParamProc: TSessionSimpleListParamProc; const Callbacks: TFunc<TPromiseSessionThreadList> = nil): TPromise<TSessionThreadList>; overload;
    function AsyncAwaitRetrieve(const SessionId, ThreadId: string; const Callbacks: TFunc<TPromiseSessionThread> = nil): TPromise<TSessionThread>; overload;
    function AsyncAwaitStreamRaw(const SessionId, ThreadId: string; const Callbacks: TFunc<TPromiseSessionStream> = nil): TPromise<TSessionStream>; overload;
    function AsyncAwaitStreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;
    function AsyncAwaitStreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;

    constructor CreateRoute(AAPI: TAnthropicAPI); reintroduce;
    destructor Destroy; override;
  end;

  TSessionThreadEventsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function List(const SessionId, ThreadId: string): TSessionEventList; overload; virtual; abstract;
    function List(const SessionId, ThreadId: string; const ParamProc: TSessionSimpleListParamProc): TSessionEventList; overload; virtual; abstract;
    function StreamRaw(const SessionId, ThreadId: string): TSessionStream; overload; virtual; abstract;
    function StreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback): Integer; overload; virtual; abstract;
    function StreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback): Integer; overload; virtual; abstract;
  end;

  TSessionThreadEventsAsynchronousSupport = class(TSessionThreadEventsAbstractSupport)
  protected
    procedure AsynList(const SessionId, ThreadId: string; const CallBacks: TFunc<TAsynSessionEventList>); overload;
    procedure AsynList(const SessionId, ThreadId: string; const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionEventList>); overload;
    procedure AsynStreamRaw(const SessionId, ThreadId: string; const CallBacks: TFunc<TAsynSessionStream>); overload;
    procedure AsynStreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
    procedure AsynStreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      const CallBacks: TFunc<TAsynSessionStreamStatus>); overload;
  end;

  TSessionThreadEventsRoute = class(TSessionThreadEventsAsynchronousSupport)
  public
    function List(const SessionId, ThreadId: string): TSessionEventList; overload; override;
    function List(const SessionId, ThreadId: string; const ParamProc: TSessionSimpleListParamProc): TSessionEventList; overload; override;
    function StreamRaw(const SessionId, ThreadId: string): TSessionStream; overload; override;
    function StreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil): Integer; overload; override;
    function StreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback = nil): Integer; overload; override;

    function AsyncAwaitList(const SessionId, ThreadId: string; const Callbacks: TFunc<TPromiseSessionEventList> = nil): TPromise<TSessionEventList>; overload;
    function AsyncAwaitList(const SessionId, ThreadId: string; const ParamProc: TSessionSimpleListParamProc; const Callbacks: TFunc<TPromiseSessionEventList> = nil): TPromise<TSessionEventList>; overload;
    function AsyncAwaitStreamRaw(const SessionId, ThreadId: string; const Callbacks: TFunc<TPromiseSessionStream> = nil): TPromise<TSessionStream>; overload;
    function AsyncAwaitStreamRaw(const SessionId, ThreadId: string; const Response: TStream;
      Event: TReceiveDataCallback = nil;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;
    function AsyncAwaitStreamEvents(const SessionId, ThreadId: string;
      const Event: TSessionStreamEvent;
      const Callbacks: TFunc<TPromiseSessionStreamStatus> = nil): TPromise<TSessionStreamStatus>; overload;
  end;

implementation

uses
  Anthropic.API.JsonSafeReader, Anthropic.API.SSEDecoder;

type
  ESessionStreamAborted = class(Exception);

  TSessionSSEFeedStream = class(TStream)
  private
    FPosition: Int64;
    FOnBytes: TFunc<TBytes, Boolean>;
  public
    constructor Create(const AOnBytes: TFunc<TBytes, Boolean>);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TSessionStreamEventAdapter = class
  private
    type
      TExecuteProc = reference to function(
        const Response: TStream;
        Event: TReceiveDataCallback): Integer;
  public
    class function Run(const Event: TSessionStreamEvent;
      ReceiveData: TReceiveDataCallback;
      const Execute: TExecuteProc): Integer; static;
  end;

  TSessionStreamRawAsyncSupport = class
  private
    type
      TExecuteProc = reference to function(
        const Event: TReceiveDataCallback): Integer;
      TPromiseInvokeProc = reference to procedure(
        const CallBacks: TFunc<TAsynSessionStreamStatus>);

    class function CopyStatus(
      const Value: TSessionStreamStatus): TSessionStreamStatus; static;
    class procedure DispatchProgress(const Sender: TObject;
      const OnProgress: TProc<TObject, TSessionStreamStatus>;
      const Status: TSessionStreamStatus); static;
  public
    class procedure Run(const Owner: TObject;
      const CallBacks: TFunc<TAsynSessionStreamStatus>;
      const Event: TReceiveDataCallback;
      const Execute: TExecuteProc); static;
    class function CreatePromise(
      const Callbacks: TFunc<TPromiseSessionStreamStatus>;
      const Invoke: TPromiseInvokeProc): TPromise<TSessionStreamStatus>; static;
  end;

  TSessionResponseHydrator = class
  public
    class procedure HydrateAgent(const Agent: TSessionAgent); static;
    class procedure HydrateResource(const Resource: TSessionResource); static;
    class procedure HydrateSession(const Session: TSession); static;
    class procedure HydrateSessionList(const List: TSessionList); static;
    class procedure HydrateEvent(const Event: TSessionEvent); static;
    class procedure HydrateEventList(const List: TSessionEventList); static;
    class procedure HydrateSendEventsResponse(const Response: TSessionSendEventsResponse); static;
    class procedure HydrateResourceList(const List: TSessionResourceList); static;
    class procedure HydrateThread(const Thread: TSessionThread); static;
    class procedure HydrateThreadList(const List: TSessionThreadList); static;
  end;

{ TSessionSSEFeedStream }

constructor TSessionSSEFeedStream.Create(const AOnBytes: TFunc<TBytes, Boolean>);
begin
  inherited Create;
  FOnBytes := AOnBytes;
  FPosition := 0;
end;

function TSessionSSEFeedStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
end;

function TSessionSSEFeedStream.Write(const Buffer; Count: Longint): Longint;
var
  Chunk: TBytes;
begin
  Result := Count;
  if Count <= 0 then
    Exit;

  SetLength(Chunk, Count);
  Move(Buffer, Chunk[0], Count);
  Inc(FPosition, Count);

  if Assigned(FOnBytes) and FOnBytes(Chunk) then
    raise ESessionStreamAborted.Create('Session stream aborted.');
end;

function TSessionSSEFeedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FPosition := Offset;
    soCurrent: FPosition := FPosition + Offset;
    soEnd: ;
  end;
  Result := FPosition;
end;

{ TSessionStreamEventAdapter }

class function TSessionStreamEventAdapter.Run(
  const Event: TSessionStreamEvent; ReceiveData: TReceiveDataCallback;
  const Execute: TExecuteProc): Integer;
var
  Aborted: Boolean;
  Decoder: TSSEDecoder;
  Feed: TSessionSSEFeedStream;
begin
  Aborted := False;
  Decoder := TSSEDecoder.Create(
    procedure(const Data: string; var Abort: Boolean)
    begin
      if Aborted then
        begin
          Abort := True;
          Exit;
        end;

      if Assigned(Event) then
        Event(Data, Abort);

      if Abort then
        Aborted := True;
    end);
  try
    Feed := TSessionSSEFeedStream.Create(
      function(Bytes: TBytes): Boolean
      begin
        Result := Aborted;
        if Result then
          Exit;

        Decoder.Feed(Bytes, Aborted);
        Result := Aborted;
      end);
    try
      try
        Result := Execute(Feed,
          procedure(const Sender: TObject; AContentLength,
            AReadCount: Int64; var AAbort: Boolean)
          begin
            if Assigned(ReceiveData) then
              ReceiveData(Sender, AContentLength, AReadCount, AAbort);

            if Aborted then
              AAbort := True;

            if AAbort then
              Aborted := True;
          end);

        if not Aborted then
          Decoder.Flush(Aborted);
      except
        on E: ESessionStreamAborted do
          Result := 0;
      end;
    finally
      Feed.Free;
    end;
  finally
    Decoder.Free;
  end;
end;

class function TSessionStreamRawAsyncSupport.CopyStatus(
  const Value: TSessionStreamStatus): TSessionStreamStatus;
begin
  if not Assigned(Value) then
    Exit(nil);

  Result := TSessionStreamStatus.New(Value.StatusCode, Value.ContentLength,
    Value.ReadCount, Value.Done);
end;

class procedure TSessionStreamRawAsyncSupport.DispatchProgress(const Sender: TObject;
  const OnProgress: TProc<TObject, TSessionStreamStatus>;
  const Status: TSessionStreamStatus);
begin
  if Assigned(OnProgress) then
    TThread.Synchronize(nil,
      procedure
      begin
        try
          OnProgress(Sender, Status);
        finally
          Status.Free;
        end;
      end)
  else
    Status.Free;
end;

class procedure TSessionStreamRawAsyncSupport.Run(const Owner: TObject;
  const CallBacks: TFunc<TAsynSessionStreamStatus>;
  const Event: TReceiveDataCallback;
  const Execute: TExecuteProc);
var
  Sender: TObject;
  OnStart: TProc<TObject>;
  OnSuccess: TProc<TObject>;
  OnProgress: TProc<TObject, TSessionStreamStatus>;
  OnError: TProc<TObject, string>;
  OnCancellation: TProc<TObject>;
  OnDoCancel: TFunc<Boolean>;
begin
  var CallBackParams := TUseParamsFactory<TAsynSessionStreamStatus>.CreateInstance(CallBacks);

  Sender := CallBackParams.Param.Sender;
  OnStart := CallBackParams.Param.OnStart;
  OnSuccess := CallBackParams.Param.OnSuccess;
  OnProgress := CallBackParams.Param.OnProgress;
  OnError := CallBackParams.Param.OnError;
  OnCancellation := CallBackParams.Param.OnCancellation;
  OnDoCancel := CallBackParams.Param.OnDoCancel;

  var Task: ITask := TTask.Create(
    procedure()
    var
      Stop: Boolean;
      CancelTag: Integer;
      LastContentLength: Int64;
      LastReadCount: Int64;
      StatusCode: Integer;
      StreamEvent: TReceiveDataCallback;
    begin
      if not Assigned(Sender) then
        Sender := Owner;

      if Assigned(OnStart) then
        TThread.Queue(nil,
          procedure
          begin
            OnStart(Sender);
          end);

      Stop := False;
      CancelTag := 0;
      LastContentLength := 0;
      LastReadCount := 0;

      StreamEvent :=
        procedure(const HttpSender: TObject; AContentLength, AReadCount: Int64;
          var AAbort: Boolean)
        begin
          LastContentLength := AContentLength;
          LastReadCount := AReadCount;

          if Assigned(Event) then
            Event(HttpSender, AContentLength, AReadCount, AAbort);

          if AAbort then
            Stop := True;

          if Assigned(OnDoCancel) then
            TThread.Synchronize(nil,
              procedure
              begin
                Stop := Stop or OnDoCancel();
              end);

          if Stop then
            begin
              AAbort := True;
              if (CancelTag = 0) and Assigned(OnCancellation) then
                TThread.Queue(nil,
                  procedure
                  begin
                    OnCancellation(Sender);
                  end);
              Inc(CancelTag);
              Exit;
            end;

          DispatchProgress(Sender, OnProgress,
            TSessionStreamStatus.New(0, AContentLength, AReadCount, False));
        end;

      try
        StatusCode := Execute(StreamEvent);

        if not Stop then
          begin
            DispatchProgress(Sender, OnProgress,
              TSessionStreamStatus.New(StatusCode, LastContentLength,
                LastReadCount, True));

            if Assigned(OnSuccess) then
              TThread.Queue(nil,
                procedure
                begin
                  OnSuccess(Sender);
                end);
          end;
      except
        on E: Exception do
          begin
            var Error := AcquireExceptionObject;
            try
              var ErrorMsg := (Error as Exception).Message;
              if Assigned(OnError) then
                TThread.Queue(nil,
                  procedure
                  begin
                    OnError(Sender, ErrorMsg);
                  end);
            finally
              Error.Free;
            end;
          end;
      end;
    end);
  Task.Start;
end;

class function TSessionStreamRawAsyncSupport.CreatePromise(
  const Callbacks: TFunc<TPromiseSessionStreamStatus>;
  const Invoke: TPromiseInvokeProc): TPromise<TSessionStreamStatus>;
begin
  Result := TPromise<TSessionStreamStatus>.Create(
    procedure(Resolve: TProc<TSessionStreamStatus>; Reject: TProc<Exception>)
    begin
      var PromiseCallbacks := Default(TPromiseSessionStreamStatus);
      var HasCallbacks := Assigned(Callbacks);
      var FinalStatus: TSessionStreamStatus := nil;

      if HasCallbacks then
        PromiseCallbacks := Callbacks();

      Invoke(
        function: TAsynSessionStreamStatus
        begin
          Result := Default(TAsynSessionStreamStatus);

          if HasCallbacks then
            begin
              Result.Sender := PromiseCallbacks.Sender;
              Result.OnStart := PromiseCallbacks.OnStart;
            end;

          Result.OnProgress :=
            procedure(Sender: TObject; Status: TSessionStreamStatus)
            begin
              if Assigned(Status) and Status.Done then
                begin
                  FinalStatus.Free;
                  FinalStatus := CopyStatus(Status);
                end;

              if HasCallbacks and Assigned(PromiseCallbacks.OnProgress) then
                PromiseCallbacks.OnProgress(Sender, Status);
            end;

          Result.OnSuccess :=
            procedure(Sender: TObject)
            begin
              if not Assigned(FinalStatus) then
                FinalStatus := TSessionStreamStatus.New(0, 0, 0, True);

              Resolve(FinalStatus);
              FinalStatus := nil;
            end;

          Result.OnError :=
            procedure(Sender: TObject; Error: string)
            begin
              FinalStatus.Free;
              FinalStatus := nil;

              if HasCallbacks and Assigned(PromiseCallbacks.OnError) then
                Error := PromiseCallbacks.OnError(Sender, Error);
              Reject(Exception.Create(Error));
            end;

          Result.OnDoCancel :=
            function: Boolean
            begin
              if HasCallbacks and Assigned(PromiseCallbacks.OnDoCancel) then
                Result := PromiseCallbacks.OnDoCancel()
              else
                Result := False;
            end;

          Result.OnCancellation :=
            procedure(Sender: TObject)
            begin
              var Error := 'aborted';

              FinalStatus.Free;
              FinalStatus := nil;

              if HasCallbacks and Assigned(PromiseCallbacks.OnCancellation) then
                begin
                  var CallbackError := PromiseCallbacks.OnCancellation(Sender);
                  if not CallbackError.IsEmpty then
                    Error := CallbackError;
                end;

              Reject(Exception.Create(Error));
            end;
        end);
    end);
end;

{ TSessionResponseHydrator }

class procedure TSessionResponseHydrator.HydrateAgent(const Agent: TSessionAgent);
begin
  if (Agent = nil) or Agent.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Agent.JSONResponse);
  if not Root.IsValid then
    Exit;

  Agent.FMCPServers := Root.ExtractSubJson('mcp_servers', Agent.FMCPServers);
  Agent.FMultiagent := Root.ExtractSubJson('multiagent', Agent.FMultiagent);
  Agent.FSkills := Root.ExtractSubJson('skills', Agent.FSkills);
  Agent.FTools := Root.ExtractSubJson('tools', Agent.FTools);
end;

class procedure TSessionResponseHydrator.HydrateResource(const Resource: TSessionResource);
begin
  if (Resource = nil) or Resource.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Resource.JSONResponse);
  if not Root.IsValid then
    Exit;

  Resource.FCheckout := Root.ExtractSubJson('checkout', Resource.FCheckout);
end;

class procedure TSessionResponseHydrator.HydrateSession(const Session: TSession);
begin
  if (Session = nil) or Session.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Session.JSONResponse);
  if not Root.IsValid then
    Exit;

  Session.FMetadata := Root.ExtractSubJson('metadata', Session.FMetadata);

  if Assigned(Session.FAgent) then
    begin
      var AgentJson := Root.ExtractSubJson('agent');
      if not AgentJson.IsEmpty then
        begin
          Session.FAgent.JSONResponse := AgentJson;
          Session.FAgent.InternalFinalizeDeserialize;
        end;
    end;

  for var I := 0 to High(Session.FResources) do
    if Assigned(Session.FResources[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('resources[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            Session.FResources[I].JSONResponse := JsonText;
            Session.FResources[I].InternalFinalizeDeserialize;
          end;
      end;
end;

class procedure TSessionResponseHydrator.HydrateSessionList(const List: TSessionList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    if Assigned(List.FData[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            List.FData[I].JSONResponse := JsonText;
            List.FData[I].InternalFinalizeDeserialize;
          end;
      end;
end;

class procedure TSessionResponseHydrator.HydrateEvent(const Event: TSessionEvent);
begin
  if (Event = nil) or Event.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Event.JSONResponse);
  if not Root.IsValid then
    Exit;

  Event.FContent := Root.ExtractSubJson('content', Event.FContent);
  Event.FInput := Root.ExtractSubJson('input', Event.FInput);
  Event.FError := Root.ExtractSubJson('error', Event.FError);
  Event.FStopReason := Root.ExtractSubJson('stop_reason', Event.FStopReason);
  Event.FUsage := Root.ExtractSubJson('usage', Event.FUsage);
  Event.FModelUsage := Root.ExtractSubJson('model_usage', Event.FModelUsage);
  Event.FRubric := Root.ExtractSubJson('rubric', Event.FRubric);
end;

class procedure TSessionResponseHydrator.HydrateEventList(const List: TSessionEventList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    if Assigned(List.FData[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            List.FData[I].JSONResponse := JsonText;
            List.FData[I].InternalFinalizeDeserialize;
          end;
      end;
end;

class procedure TSessionResponseHydrator.HydrateSendEventsResponse(
  const Response: TSessionSendEventsResponse);
begin
  if (Response = nil) or Response.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Response.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(Response.FData) do
    if Assigned(Response.FData[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            Response.FData[I].JSONResponse := JsonText;
            Response.FData[I].InternalFinalizeDeserialize;
          end;
      end;
end;

class procedure TSessionResponseHydrator.HydrateResourceList(const List: TSessionResourceList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    if Assigned(List.FData[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            List.FData[I].JSONResponse := JsonText;
            List.FData[I].InternalFinalizeDeserialize;
          end;
      end;
end;

class procedure TSessionResponseHydrator.HydrateThread(const Thread: TSessionThread);
begin
  if (Thread = nil) or Thread.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Thread.JSONResponse);
  if not Root.IsValid then
    Exit;

  if Assigned(Thread.FAgent) then
    begin
      var AgentJson := Root.ExtractSubJson('agent');
      if not AgentJson.IsEmpty then
        begin
          Thread.FAgent.JSONResponse := AgentJson;
          Thread.FAgent.InternalFinalizeDeserialize;
        end;
    end;
end;

class procedure TSessionResponseHydrator.HydrateThreadList(const List: TSessionThreadList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    if Assigned(List.FData[I]) then
      begin
        var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
        if not JsonText.IsEmpty then
          begin
            List.FData[I].JSONResponse := JsonText;
            List.FData[I].InternalFinalizeDeserialize;
          end;
      end;
end;

{ TSessionAgentParams }

class function TSessionAgentParams.New: TSessionAgentParams;
begin
  Result := TSessionAgentParams.Create.&Type();
end;

function TSessionAgentParams.&Type(const Value: string): TSessionAgentParams;
begin
  Result := TSessionAgentParams(Add('type', Value));
end;

function TSessionAgentParams.Id(const Value: string): TSessionAgentParams;
begin
  Result := TSessionAgentParams(Add('id', Value));
end;

function TSessionAgentParams.Version(const Value: Integer): TSessionAgentParams;
begin
  Result := TSessionAgentParams(Add('version', Value));
end;

{ TSessionCheckoutParams }

class function TSessionCheckoutParams.New(const Value: string): TSessionCheckoutParams;
begin
  Result := TSessionCheckoutParams.Create.&Type(Value);
end;

function TSessionCheckoutParams.&Type(const Value: string): TSessionCheckoutParams;
begin
  Result := TSessionCheckoutParams(Add('type', Value));
end;

{ TSessionBranchCheckoutParams }

class function TSessionBranchCheckoutParams.New: TSessionBranchCheckoutParams;
begin
  Result := TSessionBranchCheckoutParams.Create.&Type();
end;

function TSessionBranchCheckoutParams.&Type(const Value: string): TSessionBranchCheckoutParams;
begin
  Result := TSessionBranchCheckoutParams(Add('type', Value));
end;

function TSessionBranchCheckoutParams.Name(const Value: string): TSessionBranchCheckoutParams;
begin
  Result := TSessionBranchCheckoutParams(Add('name', Value));
end;

{ TSessionCommitCheckoutParams }

class function TSessionCommitCheckoutParams.New: TSessionCommitCheckoutParams;
begin
  Result := TSessionCommitCheckoutParams.Create.&Type();
end;

function TSessionCommitCheckoutParams.&Type(const Value: string): TSessionCommitCheckoutParams;
begin
  Result := TSessionCommitCheckoutParams(Add('type', Value));
end;

function TSessionCommitCheckoutParams.Sha(const Value: string): TSessionCommitCheckoutParams;
begin
  Result := TSessionCommitCheckoutParams(Add('sha', Value));
end;

{ TSessionResourceParams }

class function TSessionResourceParams.New(const Value: string): TSessionResourceParams;
begin
  Result := TSessionResourceParams.Create.&Type(Value);
end;

function TSessionResourceParams.&Type(const Value: string): TSessionResourceParams;
begin
  Result := TSessionResourceParams(Add('type', Value));
end;

{ TSessionGitHubRepositoryResourceParams }

class function TSessionGitHubRepositoryResourceParams.New: TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams.Create.&Type();
end;

function TSessionGitHubRepositoryResourceParams.&Type(const Value: string): TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams(Add('type', Value));
end;

function TSessionGitHubRepositoryResourceParams.Url(const Value: string): TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams(Add('url', Value));
end;

function TSessionGitHubRepositoryResourceParams.AuthorizationToken(const Value: string): TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams(Add('authorization_token', Value));
end;

function TSessionGitHubRepositoryResourceParams.Checkout(const Value: TSessionCheckoutParams): TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams(Add('checkout', Value.Detach));
end;

function TSessionGitHubRepositoryResourceParams.MountPath(const Value: string): TSessionGitHubRepositoryResourceParams;
begin
  Result := TSessionGitHubRepositoryResourceParams(Add('mount_path', Value));
end;

{ TSessionFileResourceParams }

class function TSessionFileResourceParams.New: TSessionFileResourceParams;
begin
  Result := TSessionFileResourceParams.Create.&Type();
end;

function TSessionFileResourceParams.&Type(const Value: string): TSessionFileResourceParams;
begin
  Result := TSessionFileResourceParams(Add('type', Value));
end;

function TSessionFileResourceParams.FileId(const Value: string): TSessionFileResourceParams;
begin
  Result := TSessionFileResourceParams(Add('file_id', Value));
end;

function TSessionFileResourceParams.MountPath(const Value: string): TSessionFileResourceParams;
begin
  Result := TSessionFileResourceParams(Add('mount_path', Value));
end;

{ TSessionMemoryStoreResourceParams }

class function TSessionMemoryStoreResourceParams.New: TSessionMemoryStoreResourceParams;
begin
  Result := TSessionMemoryStoreResourceParams.Create.&Type();
end;

function TSessionMemoryStoreResourceParams.&Type(const Value: string): TSessionMemoryStoreResourceParams;
begin
  Result := TSessionMemoryStoreResourceParams(Add('type', Value));
end;

function TSessionMemoryStoreResourceParams.MemoryStoreId(const Value: string): TSessionMemoryStoreResourceParams;
begin
  Result := TSessionMemoryStoreResourceParams(Add('memory_store_id', Value));
end;

function TSessionMemoryStoreResourceParams.Access(const Value: string): TSessionMemoryStoreResourceParams;
begin
  Result := TSessionMemoryStoreResourceParams(Add('access', Value));
end;

function TSessionMemoryStoreResourceParams.Instructions(const Value: string): TSessionMemoryStoreResourceParams;
begin
  Result := TSessionMemoryStoreResourceParams(Add('instructions', Value));
end;

{ TSessionCreateParams }

class function TSessionCreateParams.New: TSessionCreateParams;
begin
  Result := TSessionCreateParams.Create;
end;

function TSessionCreateParams.Beta(const Value: TArray<string>): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('beta', Value));
end;

function TSessionCreateParams.Agent(const Value: string): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('agent', Value));
end;

function TSessionCreateParams.Agent(const Value: TSessionAgentParams): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('agent', Value.Detach));
end;

function TSessionCreateParams.EnvironmentId(const Value: string): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('environment_id', Value));
end;

function TSessionCreateParams.Metadata(const Value: TJSONObject): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TSessionCreateParams.Metadata(const Key, Value: string): TSessionCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TSessionCreateParams.Resources(const Value: TArray<TSessionResourceParams>): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('resources', TJSONHelper.ToJsonArray<TSessionResourceParams>(Value)));
end;

function TSessionCreateParams.Title(const Value: string): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('title', Value));
end;

function TSessionCreateParams.VaultIds(const Value: TArray<string>): TSessionCreateParams;
begin
  Result := TSessionCreateParams(Add('vault_ids', Value));
end;

{ TSessionUpdateParams }

class function TSessionUpdateParams.New: TSessionUpdateParams;
begin
  Result := TSessionUpdateParams.Create;
end;

function TSessionUpdateParams.Beta(const Value: TArray<string>): TSessionUpdateParams;
begin
  Result := TSessionUpdateParams(Add('beta', Value));
end;

function TSessionUpdateParams.Title(const Value: string): TSessionUpdateParams;
begin
  Result := TSessionUpdateParams(Add('title', Value));
end;

function TSessionUpdateParams.Metadata(const Value: TJSONObject): TSessionUpdateParams;
begin
  Result := TSessionUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TSessionUpdateParams.Metadata(const Key, Value: string): TSessionUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TSessionUpdateParams.DeleteMetadata(const Key: string): TSessionUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

function TSessionUpdateParams.VaultIds(const Value: TArray<string>): TSessionUpdateParams;
begin
  Result := TSessionUpdateParams(Add('vault_ids', Value));
end;

{ TSessionListParams }

class function TSessionListParams.New: TSessionListParams;
begin
  Result := TSessionListParams.Create;
end;

function TSessionListParams.AgentId(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('agent_id', Value));
end;

function TSessionListParams.AgentVersion(const Value: Integer): TSessionListParams;
begin
  Result := TSessionListParams(Add('agent_version', Value));
end;

function TSessionListParams.CreatedAtGt(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('created_at[gt]', Value));
end;

function TSessionListParams.CreatedAtGte(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('created_at[gte]', Value));
end;

function TSessionListParams.CreatedAtLt(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('created_at[lt]', Value));
end;

function TSessionListParams.CreatedAtLte(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('created_at[lte]', Value));
end;

function TSessionListParams.IncludeArchived(const Value: Boolean): TSessionListParams;
begin
  Result := TSessionListParams(Add('include_archived', Value));
end;

function TSessionListParams.Limit(const Value: Integer): TSessionListParams;
begin
  Result := TSessionListParams(Add('limit', Value));
end;

function TSessionListParams.MemoryStoreId(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('memory_store_id', Value));
end;

function TSessionListParams.Order(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('order', Value));
end;

function TSessionListParams.Page(const Value: string): TSessionListParams;
begin
  Result := TSessionListParams(Add('page', Value));
end;

function TSessionListParams.Statuses(const Value: TArray<string>): TSessionListParams;
begin
  Result := TSessionListParams(Add('statuses', Value));
end;

{ TSessionEventListParams }

class function TSessionEventListParams.New: TSessionEventListParams;
begin
  Result := TSessionEventListParams.Create;
end;

function TSessionEventListParams.CreatedAtGt(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('created_at[gt]', Value));
end;

function TSessionEventListParams.CreatedAtGte(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('created_at[gte]', Value));
end;

function TSessionEventListParams.CreatedAtLt(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('created_at[lt]', Value));
end;

function TSessionEventListParams.CreatedAtLte(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('created_at[lte]', Value));
end;

function TSessionEventListParams.Limit(const Value: Integer): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('limit', Value));
end;

function TSessionEventListParams.Order(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('order', Value));
end;

function TSessionEventListParams.Page(const Value: string): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('page', Value));
end;

function TSessionEventListParams.Types(const Value: TArray<string>): TSessionEventListParams;
begin
  Result := TSessionEventListParams(Add('types', Value));
end;

{ TSessionSimpleListParams }

class function TSessionSimpleListParams.New: TSessionSimpleListParams;
begin
  Result := TSessionSimpleListParams.Create;
end;

function TSessionSimpleListParams.Limit(const Value: Integer): TSessionSimpleListParams;
begin
  Result := TSessionSimpleListParams(Add('limit', Value));
end;

function TSessionSimpleListParams.Page(const Value: string): TSessionSimpleListParams;
begin
  Result := TSessionSimpleListParams(Add('page', Value));
end;

{ TSessionSourceParams }

class function TSessionSourceParams.New(const Value: string): TSessionSourceParams;
begin
  Result := TSessionSourceParams.Create.&Type(Value);
end;

function TSessionSourceParams.&Type(const Value: string): TSessionSourceParams;
begin
  Result := TSessionSourceParams(Add('type', Value));
end;

{ TSessionBase64ImageSourceParams }

class function TSessionBase64ImageSourceParams.New: TSessionBase64ImageSourceParams;
begin
  Result := TSessionBase64ImageSourceParams.Create.&Type();
end;

function TSessionBase64ImageSourceParams.&Type(const Value: string): TSessionBase64ImageSourceParams;
begin
  Result := TSessionBase64ImageSourceParams(Add('type', Value));
end;

function TSessionBase64ImageSourceParams.Data(const Value: string): TSessionBase64ImageSourceParams;
begin
  Result := TSessionBase64ImageSourceParams(Add('data', Value));
end;

function TSessionBase64ImageSourceParams.MediaType(const Value: string): TSessionBase64ImageSourceParams;
begin
  Result := TSessionBase64ImageSourceParams(Add('media_type', Value));
end;

{ TSessionUrlSourceParams }

class function TSessionUrlSourceParams.New: TSessionUrlSourceParams;
begin
  Result := TSessionUrlSourceParams.Create.&Type();
end;

function TSessionUrlSourceParams.&Type(const Value: string): TSessionUrlSourceParams;
begin
  Result := TSessionUrlSourceParams(Add('type', Value));
end;

function TSessionUrlSourceParams.Url(const Value: string): TSessionUrlSourceParams;
begin
  Result := TSessionUrlSourceParams(Add('url', Value));
end;

{ TSessionFileSourceParams }

class function TSessionFileSourceParams.New: TSessionFileSourceParams;
begin
  Result := TSessionFileSourceParams.Create.&Type();
end;

function TSessionFileSourceParams.&Type(const Value: string): TSessionFileSourceParams;
begin
  Result := TSessionFileSourceParams(Add('type', Value));
end;

function TSessionFileSourceParams.FileId(const Value: string): TSessionFileSourceParams;
begin
  Result := TSessionFileSourceParams(Add('file_id', Value));
end;

{ TSessionBase64DocumentSourceParams }

class function TSessionBase64DocumentSourceParams.New: TSessionBase64DocumentSourceParams;
begin
  Result := TSessionBase64DocumentSourceParams.Create.&Type();
end;

function TSessionBase64DocumentSourceParams.&Type(const Value: string): TSessionBase64DocumentSourceParams;
begin
  Result := TSessionBase64DocumentSourceParams(Add('type', Value));
end;

function TSessionBase64DocumentSourceParams.Data(const Value: string): TSessionBase64DocumentSourceParams;
begin
  Result := TSessionBase64DocumentSourceParams(Add('data', Value));
end;

function TSessionBase64DocumentSourceParams.MediaType(const Value: string): TSessionBase64DocumentSourceParams;
begin
  Result := TSessionBase64DocumentSourceParams(Add('media_type', Value));
end;

{ TSessionPlainTextDocumentSourceParams }

class function TSessionPlainTextDocumentSourceParams.New: TSessionPlainTextDocumentSourceParams;
begin
  Result := TSessionPlainTextDocumentSourceParams.Create.&Type();
end;

function TSessionPlainTextDocumentSourceParams.&Type(const Value: string): TSessionPlainTextDocumentSourceParams;
begin
  Result := TSessionPlainTextDocumentSourceParams(Add('type', Value));
end;

function TSessionPlainTextDocumentSourceParams.Data(const Value: string): TSessionPlainTextDocumentSourceParams;
begin
  Result := TSessionPlainTextDocumentSourceParams(Add('data', Value));
end;

function TSessionPlainTextDocumentSourceParams.MediaType(const Value: string): TSessionPlainTextDocumentSourceParams;
begin
  Result := TSessionPlainTextDocumentSourceParams(Add('media_type', Value));
end;

{ TSessionContentBlockParams }

class function TSessionContentBlockParams.New(const Value: string): TSessionContentBlockParams;
begin
  Result := TSessionContentBlockParams.Create.&Type(Value);
end;

function TSessionContentBlockParams.&Type(const Value: string): TSessionContentBlockParams;
begin
  Result := TSessionContentBlockParams(Add('type', Value));
end;

{ TSessionTextBlockParams }

class function TSessionTextBlockParams.New: TSessionTextBlockParams;
begin
  Result := TSessionTextBlockParams.Create.&Type();
end;

function TSessionTextBlockParams.&Type(const Value: string): TSessionTextBlockParams;
begin
  Result := TSessionTextBlockParams(Add('type', Value));
end;

function TSessionTextBlockParams.Text(const Value: string): TSessionTextBlockParams;
begin
  Result := TSessionTextBlockParams(Add('text', Value));
end;

{ TSessionImageBlockParams }

class function TSessionImageBlockParams.New: TSessionImageBlockParams;
begin
  Result := TSessionImageBlockParams.Create.&Type();
end;

function TSessionImageBlockParams.&Type(const Value: string): TSessionImageBlockParams;
begin
  Result := TSessionImageBlockParams(Add('type', Value));
end;

function TSessionImageBlockParams.Source(const Value: TSessionSourceParams): TSessionImageBlockParams;
begin
  Result := TSessionImageBlockParams(Add('source', Value.Detach));
end;

{ TSessionDocumentBlockParams }

class function TSessionDocumentBlockParams.New: TSessionDocumentBlockParams;
begin
  Result := TSessionDocumentBlockParams.Create.&Type();
end;

function TSessionDocumentBlockParams.&Type(const Value: string): TSessionDocumentBlockParams;
begin
  Result := TSessionDocumentBlockParams(Add('type', Value));
end;

function TSessionDocumentBlockParams.Source(const Value: TSessionSourceParams): TSessionDocumentBlockParams;
begin
  Result := TSessionDocumentBlockParams(Add('source', Value.Detach));
end;

function TSessionDocumentBlockParams.Context(const Value: string): TSessionDocumentBlockParams;
begin
  Result := TSessionDocumentBlockParams(Add('context', Value));
end;

function TSessionDocumentBlockParams.Title(const Value: string): TSessionDocumentBlockParams;
begin
  Result := TSessionDocumentBlockParams(Add('title', Value));
end;

{ TSessionEventParams }

class function TSessionEventParams.New(const Value: string): TSessionEventParams;
begin
  Result := TSessionEventParams.Create.&Type(Value);
end;

function TSessionEventParams.&Type(const Value: string): TSessionEventParams;
begin
  Result := TSessionEventParams(Add('type', Value));
end;

{ TSessionUserMessageEventParams }

class function TSessionUserMessageEventParams.New: TSessionUserMessageEventParams;
begin
  Result := TSessionUserMessageEventParams.Create.&Type();
end;

function TSessionUserMessageEventParams.&Type(const Value: string): TSessionUserMessageEventParams;
begin
  Result := TSessionUserMessageEventParams(Add('type', Value));
end;

function TSessionUserMessageEventParams.Content(const Value: TArray<TSessionContentBlockParams>): TSessionUserMessageEventParams;
begin
  Result := TSessionUserMessageEventParams(Add('content', TJSONHelper.ToJsonArray<TSessionContentBlockParams>(Value)));
end;

function TSessionUserMessageEventParams.Text(const Value: string): TSessionUserMessageEventParams;
var
  Blocks: TArray<TSessionContentBlockParams>;
begin
  SetLength(Blocks, 1);
  Blocks[0] := TSessionTextBlockParams.Create.&Type().Text(Value);
  Result := Content(Blocks);
end;

{ TSessionUserInterruptEventParams }

class function TSessionUserInterruptEventParams.New: TSessionUserInterruptEventParams;
begin
  Result := TSessionUserInterruptEventParams.Create.&Type();
end;

function TSessionUserInterruptEventParams.&Type(const Value: string): TSessionUserInterruptEventParams;
begin
  Result := TSessionUserInterruptEventParams(Add('type', Value));
end;

function TSessionUserInterruptEventParams.SessionThreadId(const Value: string): TSessionUserInterruptEventParams;
begin
  Result := TSessionUserInterruptEventParams(Add('session_thread_id', Value));
end;

{ TSessionUserToolConfirmationEventParams }

class function TSessionUserToolConfirmationEventParams.New: TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams.Create.&Type();
end;

function TSessionUserToolConfirmationEventParams.&Type(const Value: string): TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams(Add('type', Value));
end;

function TSessionUserToolConfirmationEventParams.Result(const Value: string): TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams(Add('result', Value));
end;

function TSessionUserToolConfirmationEventParams.ToolUseId(const Value: string): TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams(Add('tool_use_id', Value));
end;

function TSessionUserToolConfirmationEventParams.DenyMessage(const Value: string): TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams(Add('deny_message', Value));
end;

function TSessionUserToolConfirmationEventParams.SessionThreadId(const Value: string): TSessionUserToolConfirmationEventParams;
begin
  Result := TSessionUserToolConfirmationEventParams(Add('session_thread_id', Value));
end;

{ TSessionUserCustomToolResultEventParams }

class function TSessionUserCustomToolResultEventParams.New: TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams.Create.&Type();
end;

function TSessionUserCustomToolResultEventParams.&Type(const Value: string): TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams(Add('type', Value));
end;

function TSessionUserCustomToolResultEventParams.CustomToolUseId(const Value: string): TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams(Add('custom_tool_use_id', Value));
end;

function TSessionUserCustomToolResultEventParams.Content(const Value: TArray<TSessionContentBlockParams>): TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams(Add('content', TJSONHelper.ToJsonArray<TSessionContentBlockParams>(Value)));
end;

function TSessionUserCustomToolResultEventParams.IsError(const Value: Boolean): TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams(Add('is_error', Value));
end;

function TSessionUserCustomToolResultEventParams.SessionThreadId(const Value: string): TSessionUserCustomToolResultEventParams;
begin
  Result := TSessionUserCustomToolResultEventParams(Add('session_thread_id', Value));
end;

{ TSessionRubricParams }

class function TSessionRubricParams.New(const Value: string): TSessionRubricParams;
begin
  Result := TSessionRubricParams.Create.&Type(Value);
end;

function TSessionRubricParams.&Type(const Value: string): TSessionRubricParams;
begin
  Result := TSessionRubricParams(Add('type', Value));
end;

{ TSessionTextRubricParams }

class function TSessionTextRubricParams.New: TSessionTextRubricParams;
begin
  Result := TSessionTextRubricParams.Create.&Type();
end;

function TSessionTextRubricParams.&Type(const Value: string): TSessionTextRubricParams;
begin
  Result := TSessionTextRubricParams(Add('type', Value));
end;

function TSessionTextRubricParams.Content(const Value: string): TSessionTextRubricParams;
begin
  Result := TSessionTextRubricParams(Add('content', Value));
end;

{ TSessionFileRubricParams }

class function TSessionFileRubricParams.New: TSessionFileRubricParams;
begin
  Result := TSessionFileRubricParams.Create.&Type();
end;

function TSessionFileRubricParams.&Type(const Value: string): TSessionFileRubricParams;
begin
  Result := TSessionFileRubricParams(Add('type', Value));
end;

function TSessionFileRubricParams.FileId(const Value: string): TSessionFileRubricParams;
begin
  Result := TSessionFileRubricParams(Add('file_id', Value));
end;

{ TSessionUserDefineOutcomeEventParams }

class function TSessionUserDefineOutcomeEventParams.New: TSessionUserDefineOutcomeEventParams;
begin
  Result := TSessionUserDefineOutcomeEventParams.Create.&Type();
end;

function TSessionUserDefineOutcomeEventParams.&Type(const Value: string): TSessionUserDefineOutcomeEventParams;
begin
  Result := TSessionUserDefineOutcomeEventParams(Add('type', Value));
end;

function TSessionUserDefineOutcomeEventParams.Description(const Value: string): TSessionUserDefineOutcomeEventParams;
begin
  Result := TSessionUserDefineOutcomeEventParams(Add('description', Value));
end;

function TSessionUserDefineOutcomeEventParams.Rubric(const Value: TSessionRubricParams): TSessionUserDefineOutcomeEventParams;
begin
  Result := TSessionUserDefineOutcomeEventParams(Add('rubric', Value.Detach));
end;

function TSessionUserDefineOutcomeEventParams.MaxIterations(const Value: Integer): TSessionUserDefineOutcomeEventParams;
begin
  Result := TSessionUserDefineOutcomeEventParams(Add('max_iterations', Value));
end;

{ TSessionSendEventsParams }

class function TSessionSendEventsParams.New: TSessionSendEventsParams;
begin
  Result := TSessionSendEventsParams.Create;
end;

function TSessionSendEventsParams.Beta(const Value: TArray<string>): TSessionSendEventsParams;
begin
  Result := TSessionSendEventsParams(Add('beta', Value));
end;

function TSessionSendEventsParams.Events(const Value: TArray<TSessionEventParams>): TSessionSendEventsParams;
begin
  Result := TSessionSendEventsParams(Add('events', TJSONHelper.ToJsonArray<TSessionEventParams>(Value)));
end;

{ TSessionResourceUpdateParams }

class function TSessionResourceUpdateParams.New: TSessionResourceUpdateParams;
begin
  Result := TSessionResourceUpdateParams.Create;
end;

function TSessionResourceUpdateParams.Beta(const Value: TArray<string>): TSessionResourceUpdateParams;
begin
  Result := TSessionResourceUpdateParams(Add('beta', Value));
end;

function TSessionResourceUpdateParams.AuthorizationToken(const Value: string): TSessionResourceUpdateParams;
begin
  Result := TSessionResourceUpdateParams(Add('authorization_token', Value));
end;

{ TSessionAgent }

procedure TSessionAgent.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionAgent.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateAgent(Self);
end;

destructor TSessionAgent.Destroy;
begin
  FModel.Free;
  inherited;
end;

{ TSessionUsage }

destructor TSessionUsage.Destroy;
begin
  FCacheCreation.Free;
  inherited;
end;

{ TSessionResource }

procedure TSessionResource.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionResource.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateResource(Self);
end;

function TSessionResource.IsFile: Boolean;
begin
  Result := SameText(FType, 'file');
end;

function TSessionResource.IsGitHubRepository: Boolean;
begin
  Result := SameText(FType, 'github_repository');
end;

function TSessionResource.IsMemoryStore: Boolean;
begin
  Result := SameText(FType, 'memory_store');
end;

{ TSession }

procedure TSession.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSession.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateSession(Self);
end;

destructor TSession.Destroy;
begin
  FAgent.Free;
  for var Item in FOutcomeEvaluations do
    Item.Free;
  for var Item in FResources do
    Item.Free;
  FStats.Free;
  FUsage.Free;
  inherited;
end;

{ TSessionList }

procedure TSessionList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionList.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateSessionList(Self);
end;

destructor TSessionList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TSessionEvent }

procedure TSessionEvent.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionEvent.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateEvent(Self);
end;

{ TSessionEventList }

procedure TSessionEventList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionEventList.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateEventList(Self);
end;

destructor TSessionEventList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TSessionSendEventsResponse }

procedure TSessionSendEventsResponse.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionSendEventsResponse.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateSendEventsResponse(Self);
end;

destructor TSessionSendEventsResponse.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TSessionResourceList }

procedure TSessionResourceList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionResourceList.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateResourceList(Self);
end;

destructor TSessionResourceList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TSessionThread }

procedure TSessionThread.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionThread.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateThread(Self);
end;

destructor TSessionThread.Destroy;
begin
  FAgent.Free;
  FStats.Free;
  FUsage.Free;
  inherited;
end;

{ TSessionThreadList }

procedure TSessionThreadList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TSessionThreadList.ContentUpdate;
begin
  inherited;
  TSessionResponseHydrator.HydrateThreadList(Self);
end;

destructor TSessionThreadList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TSessionStreamStatus }

class function TSessionStreamStatus.New(const AStatusCode: Integer;
  const AContentLength, AReadCount: Int64;
  const ADone: Boolean): TSessionStreamStatus;
begin
  Result := TSessionStreamStatus.Create;
  Result.StatusCode := AStatusCode;
  Result.ContentLength := AContentLength;
  Result.ReadCount := AReadCount;
  Result.Done := ADone;
end;

{ TSessionStream }

constructor TSessionStream.Create;
begin
  inherited Create;
end;

constructor TSessionStream.Create(const Value: string);
begin
  inherited Create;
  FData := Value;
  JSONResponse := Value;
end;

{ TSessionsRoute }

constructor TSessionsRoute.CreateRoute(AAPI: TAnthropicAPI);
begin
  inherited CreateRoute(AAPI);
  FEvents := TSessionEventsRoute.CreateRoute(AAPI);
  FResources := TSessionResourcesRoute.CreateRoute(AAPI);
  FThreads := TSessionThreadsRoute.CreateRoute(AAPI);
end;

destructor TSessionsRoute.Destroy;
begin
  FEvents.Free;
  FResources.Free;
  FThreads.Free;
  inherited;
end;

function TSessionsRoute.GetEvents: TSessionEventsRoute;
begin
  Result := FEvents;
end;

function TSessionsRoute.GetResources: TSessionResourcesRoute;
begin
  Result := FResources;
end;

function TSessionsRoute.GetThreads: TSessionThreadsRoute;
begin
  Result := FThreads;
end;

function TSessionsRoute.Create(const ParamProc: TSessionCreateParamProc): TSession;
begin
  Result := API.Post<TSession, TSessionCreateParams>('sessions', ParamProc, True);
end;

function TSessionsRoute.List: TSessionList;
begin
  Result := API.Get<TSessionList>('sessions');
end;

function TSessionsRoute.List(const ParamProc: TSessionListParamProc): TSessionList;
begin
  Result := API.Get<TSessionList, TSessionListParams>('sessions', ParamProc);
end;

function TSessionsRoute.Retrieve(const SessionId: string): TSession;
begin
  Result := API.Get<TSession>('sessions/' + SessionId);
end;

function TSessionsRoute.Update(const SessionId: string; const ParamProc: TSessionUpdateParamProc): TSession;
begin
  Result := API.Post<TSession, TSessionUpdateParams>('sessions/' + SessionId, ParamProc, True);
end;

function TSessionsRoute.Delete(const SessionId: string): TSessionDeleted;
begin
  Result := API.Delete<TSessionDeleted>('sessions/' + SessionId);
end;

function TSessionsRoute.Archive(const SessionId: string): TSession;
begin
  Result := API.Post<TSession>('sessions/' + SessionId + '/archive');
end;

function TSessionsRoute.AsyncAwaitCreate(const ParamProc: TSessionCreateParamProc;
  const Callbacks: TFunc<TPromiseSession>): TPromise<TSession>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSession>(
    procedure(const CallbackParams: TFunc<TAsynSession>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitList(const Callbacks: TFunc<TPromiseSessionList>): TPromise<TSessionList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionList>(
    procedure(const CallbackParams: TFunc<TAsynSessionList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitList(const ParamProc: TSessionListParamProc;
  const Callbacks: TFunc<TPromiseSessionList>): TPromise<TSessionList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionList>(
    procedure(const CallbackParams: TFunc<TAsynSessionList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitRetrieve(const SessionId: string;
  const Callbacks: TFunc<TPromiseSession>): TPromise<TSession>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSession>(
    procedure(const CallbackParams: TFunc<TAsynSession>)
    begin
      Self.AsynRetrieve(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitUpdate(const SessionId: string;
  const ParamProc: TSessionUpdateParamProc;
  const Callbacks: TFunc<TPromiseSession>): TPromise<TSession>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSession>(
    procedure(const CallbackParams: TFunc<TAsynSession>)
    begin
      Self.AsynUpdate(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitDelete(const SessionId: string;
  const Callbacks: TFunc<TPromiseSessionDeleted>): TPromise<TSessionDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionDeleted>(
    procedure(const CallbackParams: TFunc<TAsynSessionDeleted>)
    begin
      Self.AsynDelete(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionsRoute.AsyncAwaitArchive(const SessionId: string;
  const Callbacks: TFunc<TPromiseSession>): TPromise<TSession>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSession>(
    procedure(const CallbackParams: TFunc<TAsynSession>)
    begin
      Self.AsynArchive(SessionId, CallbackParams);
    end,
    Callbacks);
end;

{ TSessionsAsynchronousSupport }

procedure TSessionsAsynchronousSupport.AsynCreate(const ParamProc: TSessionCreateParamProc;
  const CallBacks: TFunc<TAsynSession>);
begin
  with TAsynCallBackExec<TAsynSession, TSession>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSession
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynList(const CallBacks: TFunc<TAsynSessionList>);
begin
  with TAsynCallBackExec<TAsynSessionList, TSessionList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynList(const ParamProc: TSessionListParamProc;
  const CallBacks: TFunc<TAsynSessionList>);
begin
  with TAsynCallBackExec<TAsynSessionList, TSessionList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynRetrieve(const SessionId: string;
  const CallBacks: TFunc<TAsynSession>);
begin
  with TAsynCallBackExec<TAsynSession, TSession>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSession
      begin
        Result := Self.Retrieve(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynUpdate(const SessionId: string;
  const ParamProc: TSessionUpdateParamProc; const CallBacks: TFunc<TAsynSession>);
begin
  with TAsynCallBackExec<TAsynSession, TSession>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSession
      begin
        Result := Self.Update(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynDelete(const SessionId: string;
  const CallBacks: TFunc<TAsynSessionDeleted>);
begin
  with TAsynCallBackExec<TAsynSessionDeleted, TSessionDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionDeleted
      begin
        Result := Self.Delete(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionsAsynchronousSupport.AsynArchive(const SessionId: string;
  const CallBacks: TFunc<TAsynSession>);
begin
  with TAsynCallBackExec<TAsynSession, TSession>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSession
      begin
        Result := Self.Archive(SessionId);
      end);
  finally
    Free;
  end;
end;

{ TSessionEventsRoute }

function TSessionEventsRoute.List(const SessionId: string): TSessionEventList;
begin
  Result := API.Get<TSessionEventList>('sessions/' + SessionId + '/events');
end;

function TSessionEventsRoute.List(const SessionId: string;
  const ParamProc: TSessionEventListParamProc): TSessionEventList;
begin
  Result := API.Get<TSessionEventList, TSessionEventListParams>('sessions/' + SessionId + '/events', ParamProc);
end;

function TSessionEventsRoute.Send(const SessionId: string;
  const ParamProc: TSessionSendEventsParamProc): TSessionSendEventsResponse;
begin
  Result := API.Post<TSessionSendEventsResponse, TSessionSendEventsParams>('sessions/' + SessionId + '/events', ParamProc, True);
end;

function TSessionEventsRoute.StreamRaw(const SessionId: string): TSessionStream;
begin
  Result := TSessionStream.Create(API.Get('sessions/' + SessionId + '/events/stream'));
end;

function TSessionEventsRoute.StreamRaw(const SessionId: string;
  const Response: TStream; Event: TReceiveDataCallback): Integer;
begin
  Result := API.GetStream('sessions/' + SessionId + '/events/stream',
    Response, [], 'text/event-stream', Event);
end;

function TSessionEventsRoute.StreamEvents(const SessionId: string;
  const Event: TSessionStreamEvent;
  ReceiveData: TReceiveDataCallback): Integer;
begin
  Result := TSessionStreamEventAdapter.Run(Event, ReceiveData,
    function(const Response: TStream;
      StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, Response, StreamEvent);
    end);
end;

function TSessionEventsRoute.AsyncAwaitList(const SessionId: string;
  const Callbacks: TFunc<TPromiseSessionEventList>): TPromise<TSessionEventList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionEventList>(
    procedure(const CallbackParams: TFunc<TAsynSessionEventList>)
    begin
      Self.AsynList(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionEventsRoute.AsyncAwaitList(const SessionId: string;
  const ParamProc: TSessionEventListParamProc;
  const Callbacks: TFunc<TPromiseSessionEventList>): TPromise<TSessionEventList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionEventList>(
    procedure(const CallbackParams: TFunc<TAsynSessionEventList>)
    begin
      Self.AsynList(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionEventsRoute.AsyncAwaitSend(const SessionId: string;
  const ParamProc: TSessionSendEventsParamProc;
  const Callbacks: TFunc<TPromiseSessionSendEventsResponse>): TPromise<TSessionSendEventsResponse>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionSendEventsResponse>(
    procedure(const CallbackParams: TFunc<TAsynSessionSendEventsResponse>)
    begin
      Self.AsynSend(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionEventsRoute.AsyncAwaitStreamRaw(const SessionId: string;
  const Callbacks: TFunc<TPromiseSessionStream>): TPromise<TSessionStream>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionStream>(
    procedure(const CallbackParams: TFunc<TAsynSessionStream>)
    begin
      Self.AsynStreamRaw(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionEventsRoute.AsyncAwaitStreamRaw(const SessionId: string;
  const Response: TStream; Event: TReceiveDataCallback;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamRaw(SessionId, Response, Event, CallbackParams);
    end);
end;

function TSessionEventsRoute.AsyncAwaitStreamEvents(const SessionId: string;
  const Event: TSessionStreamEvent;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamEvents(SessionId, Event, CallbackParams);
    end);
end;

{ TSessionEventsAsynchronousSupport }

procedure TSessionEventsAsynchronousSupport.AsynList(const SessionId: string;
  const CallBacks: TFunc<TAsynSessionEventList>);
begin
  with TAsynCallBackExec<TAsynSessionEventList, TSessionEventList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionEventList
      begin
        Result := Self.List(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionEventsAsynchronousSupport.AsynList(const SessionId: string;
  const ParamProc: TSessionEventListParamProc;
  const CallBacks: TFunc<TAsynSessionEventList>);
begin
  with TAsynCallBackExec<TAsynSessionEventList, TSessionEventList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionEventList
      begin
        Result := Self.List(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionEventsAsynchronousSupport.AsynSend(const SessionId: string;
  const ParamProc: TSessionSendEventsParamProc;
  const CallBacks: TFunc<TAsynSessionSendEventsResponse>);
begin
  with TAsynCallBackExec<TAsynSessionSendEventsResponse, TSessionSendEventsResponse>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionSendEventsResponse
      begin
        Result := Self.Send(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionEventsAsynchronousSupport.AsynStreamRaw(const SessionId: string;
  const CallBacks: TFunc<TAsynSessionStream>);
begin
  with TAsynCallBackExec<TAsynSessionStream, TSessionStream>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionStream
      begin
        Result := Self.StreamRaw(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionEventsAsynchronousSupport.AsynStreamRaw(
  const SessionId: string; const Response: TStream;
  Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, Event,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, Response, StreamEvent);
    end);
end;

procedure TSessionEventsAsynchronousSupport.AsynStreamEvents(
  const SessionId: string; const Event: TSessionStreamEvent;
  const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, nil,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamEvents(SessionId, Event, StreamEvent);
    end);
end;

{ TSessionResourcesRoute }

function TSessionResourcesRoute.Add(const SessionId: string;
  const ParamProc: TSessionResourceAddParamProc): TSessionResource;
begin
  Result := API.Post<TSessionResource, TSessionFileResourceParams>('sessions/' + SessionId + '/resources', ParamProc, True);
end;

function TSessionResourcesRoute.List(const SessionId: string): TSessionResourceList;
begin
  Result := API.Get<TSessionResourceList>('sessions/' + SessionId + '/resources');
end;

function TSessionResourcesRoute.List(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc): TSessionResourceList;
begin
  Result := API.Get<TSessionResourceList, TSessionSimpleListParams>('sessions/' + SessionId + '/resources', ParamProc);
end;

function TSessionResourcesRoute.Retrieve(const SessionId, ResourceId: string): TSessionResource;
begin
  Result := API.Get<TSessionResource>('sessions/' + SessionId + '/resources/' + ResourceId);
end;

function TSessionResourcesRoute.Update(const SessionId, ResourceId: string;
  const ParamProc: TSessionResourceUpdateParamProc): TSessionResource;
begin
  Result := API.Post<TSessionResource, TSessionResourceUpdateParams>('sessions/' + SessionId + '/resources/' + ResourceId, ParamProc, True);
end;

function TSessionResourcesRoute.Delete(const SessionId, ResourceId: string): TSessionResourceDeleted;
begin
  Result := API.Delete<TSessionResourceDeleted>('sessions/' + SessionId + '/resources/' + ResourceId);
end;

function TSessionResourcesRoute.AsyncAwaitAdd(const SessionId: string;
  const ParamProc: TSessionResourceAddParamProc;
  const Callbacks: TFunc<TPromiseSessionResource>): TPromise<TSessionResource>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResource>(
    procedure(const CallbackParams: TFunc<TAsynSessionResource>)
    begin
      Self.AsynAdd(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionResourcesRoute.AsyncAwaitList(const SessionId: string;
  const Callbacks: TFunc<TPromiseSessionResourceList>): TPromise<TSessionResourceList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResourceList>(
    procedure(const CallbackParams: TFunc<TAsynSessionResourceList>)
    begin
      Self.AsynList(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionResourcesRoute.AsyncAwaitList(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc;
  const Callbacks: TFunc<TPromiseSessionResourceList>): TPromise<TSessionResourceList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResourceList>(
    procedure(const CallbackParams: TFunc<TAsynSessionResourceList>)
    begin
      Self.AsynList(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionResourcesRoute.AsyncAwaitRetrieve(const SessionId, ResourceId: string;
  const Callbacks: TFunc<TPromiseSessionResource>): TPromise<TSessionResource>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResource>(
    procedure(const CallbackParams: TFunc<TAsynSessionResource>)
    begin
      Self.AsynRetrieve(SessionId, ResourceId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionResourcesRoute.AsyncAwaitUpdate(const SessionId, ResourceId: string;
  const ParamProc: TSessionResourceUpdateParamProc;
  const Callbacks: TFunc<TPromiseSessionResource>): TPromise<TSessionResource>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResource>(
    procedure(const CallbackParams: TFunc<TAsynSessionResource>)
    begin
      Self.AsynUpdate(SessionId, ResourceId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionResourcesRoute.AsyncAwaitDelete(const SessionId, ResourceId: string;
  const Callbacks: TFunc<TPromiseSessionResourceDeleted>): TPromise<TSessionResourceDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionResourceDeleted>(
    procedure(const CallbackParams: TFunc<TAsynSessionResourceDeleted>)
    begin
      Self.AsynDelete(SessionId, ResourceId, CallbackParams);
    end,
    Callbacks);
end;

{ TSessionResourcesAsynchronousSupport }

procedure TSessionResourcesAsynchronousSupport.AsynAdd(const SessionId: string;
  const ParamProc: TSessionResourceAddParamProc; const CallBacks: TFunc<TAsynSessionResource>);
begin
  with TAsynCallBackExec<TAsynSessionResource, TSessionResource>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResource
      begin
        Result := Self.Add(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionResourcesAsynchronousSupport.AsynList(const SessionId: string;
  const CallBacks: TFunc<TAsynSessionResourceList>);
begin
  with TAsynCallBackExec<TAsynSessionResourceList, TSessionResourceList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResourceList
      begin
        Result := Self.List(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionResourcesAsynchronousSupport.AsynList(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionResourceList>);
begin
  with TAsynCallBackExec<TAsynSessionResourceList, TSessionResourceList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResourceList
      begin
        Result := Self.List(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionResourcesAsynchronousSupport.AsynRetrieve(const SessionId, ResourceId: string;
  const CallBacks: TFunc<TAsynSessionResource>);
begin
  with TAsynCallBackExec<TAsynSessionResource, TSessionResource>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResource
      begin
        Result := Self.Retrieve(SessionId, ResourceId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionResourcesAsynchronousSupport.AsynUpdate(const SessionId, ResourceId: string;
  const ParamProc: TSessionResourceUpdateParamProc; const CallBacks: TFunc<TAsynSessionResource>);
begin
  with TAsynCallBackExec<TAsynSessionResource, TSessionResource>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResource
      begin
        Result := Self.Update(SessionId, ResourceId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionResourcesAsynchronousSupport.AsynDelete(const SessionId, ResourceId: string;
  const CallBacks: TFunc<TAsynSessionResourceDeleted>);
begin
  with TAsynCallBackExec<TAsynSessionResourceDeleted, TSessionResourceDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionResourceDeleted
      begin
        Result := Self.Delete(SessionId, ResourceId);
      end);
  finally
    Free;
  end;
end;

{ TSessionThreadsRoute }

constructor TSessionThreadsRoute.CreateRoute(AAPI: TAnthropicAPI);
begin
  inherited CreateRoute(AAPI);
  FEvents := TSessionThreadEventsRoute.CreateRoute(AAPI);
end;

destructor TSessionThreadsRoute.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TSessionThreadsRoute.GetEvents: TSessionThreadEventsRoute;
begin
  Result := FEvents;
end;

function TSessionThreadsRoute.List(const SessionId: string): TSessionThreadList;
begin
  Result := API.Get<TSessionThreadList>('sessions/' + SessionId + '/threads');
end;

function TSessionThreadsRoute.List(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc): TSessionThreadList;
begin
  Result := API.Get<TSessionThreadList, TSessionSimpleListParams>('sessions/' + SessionId + '/threads', ParamProc);
end;

function TSessionThreadsRoute.Retrieve(const SessionId, ThreadId: string): TSessionThread;
begin
  Result := API.Get<TSessionThread>('sessions/' + SessionId + '/threads/' + ThreadId);
end;

function TSessionThreadsRoute.StreamRaw(const SessionId, ThreadId: string): TSessionStream;
begin
  Result := TSessionStream.Create(API.Get('sessions/' + SessionId + '/threads/' + ThreadId + '/stream'));
end;

function TSessionThreadsRoute.StreamRaw(const SessionId, ThreadId: string;
  const Response: TStream; Event: TReceiveDataCallback): Integer;
begin
  Result := API.GetStream('sessions/' + SessionId + '/threads/' + ThreadId + '/stream',
    Response, [], 'text/event-stream', Event);
end;

function TSessionThreadsRoute.StreamEvents(const SessionId, ThreadId: string;
  const Event: TSessionStreamEvent;
  ReceiveData: TReceiveDataCallback): Integer;
begin
  Result := TSessionStreamEventAdapter.Run(Event, ReceiveData,
    function(const Response: TStream;
      StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, ThreadId, Response, StreamEvent);
    end);
end;

function TSessionThreadsRoute.AsyncAwaitList(const SessionId: string;
  const Callbacks: TFunc<TPromiseSessionThreadList>): TPromise<TSessionThreadList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionThreadList>(
    procedure(const CallbackParams: TFunc<TAsynSessionThreadList>)
    begin
      Self.AsynList(SessionId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadsRoute.AsyncAwaitList(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc;
  const Callbacks: TFunc<TPromiseSessionThreadList>): TPromise<TSessionThreadList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionThreadList>(
    procedure(const CallbackParams: TFunc<TAsynSessionThreadList>)
    begin
      Self.AsynList(SessionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadsRoute.AsyncAwaitRetrieve(const SessionId, ThreadId: string;
  const Callbacks: TFunc<TPromiseSessionThread>): TPromise<TSessionThread>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionThread>(
    procedure(const CallbackParams: TFunc<TAsynSessionThread>)
    begin
      Self.AsynRetrieve(SessionId, ThreadId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadsRoute.AsyncAwaitStreamRaw(const SessionId, ThreadId: string;
  const Callbacks: TFunc<TPromiseSessionStream>): TPromise<TSessionStream>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionStream>(
    procedure(const CallbackParams: TFunc<TAsynSessionStream>)
    begin
      Self.AsynStreamRaw(SessionId, ThreadId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadsRoute.AsyncAwaitStreamRaw(const SessionId, ThreadId: string;
  const Response: TStream; Event: TReceiveDataCallback;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamRaw(SessionId, ThreadId, Response, Event, CallbackParams);
    end);
end;

function TSessionThreadsRoute.AsyncAwaitStreamEvents(const SessionId,
  ThreadId: string; const Event: TSessionStreamEvent;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamEvents(SessionId, ThreadId, Event, CallbackParams);
    end);
end;

{ TSessionThreadsAsynchronousSupport }

procedure TSessionThreadsAsynchronousSupport.AsynList(const SessionId: string;
  const CallBacks: TFunc<TAsynSessionThreadList>);
begin
  with TAsynCallBackExec<TAsynSessionThreadList, TSessionThreadList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionThreadList
      begin
        Result := Self.List(SessionId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadsAsynchronousSupport.AsynList(const SessionId: string;
  const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionThreadList>);
begin
  with TAsynCallBackExec<TAsynSessionThreadList, TSessionThreadList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionThreadList
      begin
        Result := Self.List(SessionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadsAsynchronousSupport.AsynRetrieve(const SessionId, ThreadId: string;
  const CallBacks: TFunc<TAsynSessionThread>);
begin
  with TAsynCallBackExec<TAsynSessionThread, TSessionThread>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionThread
      begin
        Result := Self.Retrieve(SessionId, ThreadId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadsAsynchronousSupport.AsynStreamRaw(const SessionId, ThreadId: string;
  const CallBacks: TFunc<TAsynSessionStream>);
begin
  with TAsynCallBackExec<TAsynSessionStream, TSessionStream>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionStream
      begin
        Result := Self.StreamRaw(SessionId, ThreadId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadsAsynchronousSupport.AsynStreamRaw(
  const SessionId, ThreadId: string; const Response: TStream;
  Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, Event,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, ThreadId, Response, StreamEvent);
    end);
end;

procedure TSessionThreadsAsynchronousSupport.AsynStreamEvents(
  const SessionId, ThreadId: string; const Event: TSessionStreamEvent;
  const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, nil,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamEvents(SessionId, ThreadId, Event, StreamEvent);
    end);
end;

{ TSessionThreadEventsRoute }

function TSessionThreadEventsRoute.List(const SessionId, ThreadId: string): TSessionEventList;
begin
  Result := API.Get<TSessionEventList>('sessions/' + SessionId + '/threads/' + ThreadId + '/events');
end;

function TSessionThreadEventsRoute.List(const SessionId, ThreadId: string;
  const ParamProc: TSessionSimpleListParamProc): TSessionEventList;
begin
  Result := API.Get<TSessionEventList, TSessionSimpleListParams>('sessions/' + SessionId + '/threads/' + ThreadId + '/events', ParamProc);
end;

function TSessionThreadEventsRoute.StreamRaw(const SessionId, ThreadId: string): TSessionStream;
begin
  Result := TSessionStream.Create(API.Get('sessions/' + SessionId + '/threads/' + ThreadId + '/events/stream'));
end;

function TSessionThreadEventsRoute.StreamRaw(const SessionId, ThreadId: string;
  const Response: TStream; Event: TReceiveDataCallback): Integer;
begin
  Result := API.GetStream('sessions/' + SessionId + '/threads/' + ThreadId + '/events/stream',
    Response, [], 'text/event-stream', Event);
end;

function TSessionThreadEventsRoute.StreamEvents(const SessionId,
  ThreadId: string; const Event: TSessionStreamEvent;
  ReceiveData: TReceiveDataCallback): Integer;
begin
  Result := TSessionStreamEventAdapter.Run(Event, ReceiveData,
    function(const Response: TStream;
      StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, ThreadId, Response, StreamEvent);
    end);
end;

function TSessionThreadEventsRoute.AsyncAwaitList(const SessionId, ThreadId: string;
  const Callbacks: TFunc<TPromiseSessionEventList>): TPromise<TSessionEventList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionEventList>(
    procedure(const CallbackParams: TFunc<TAsynSessionEventList>)
    begin
      Self.AsynList(SessionId, ThreadId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadEventsRoute.AsyncAwaitList(const SessionId, ThreadId: string;
  const ParamProc: TSessionSimpleListParamProc;
  const Callbacks: TFunc<TPromiseSessionEventList>): TPromise<TSessionEventList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionEventList>(
    procedure(const CallbackParams: TFunc<TAsynSessionEventList>)
    begin
      Self.AsynList(SessionId, ThreadId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadEventsRoute.AsyncAwaitStreamRaw(const SessionId, ThreadId: string;
  const Callbacks: TFunc<TPromiseSessionStream>): TPromise<TSessionStream>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TSessionStream>(
    procedure(const CallbackParams: TFunc<TAsynSessionStream>)
    begin
      Self.AsynStreamRaw(SessionId, ThreadId, CallbackParams);
    end,
    Callbacks);
end;

function TSessionThreadEventsRoute.AsyncAwaitStreamRaw(const SessionId, ThreadId: string;
  const Response: TStream; Event: TReceiveDataCallback;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamRaw(SessionId, ThreadId, Response, Event, CallbackParams);
    end);
end;

function TSessionThreadEventsRoute.AsyncAwaitStreamEvents(const SessionId,
  ThreadId: string; const Event: TSessionStreamEvent;
  const Callbacks: TFunc<TPromiseSessionStreamStatus>): TPromise<TSessionStreamStatus>;
begin
  Result := TSessionStreamRawAsyncSupport.CreatePromise(Callbacks,
    procedure(const CallbackParams: TFunc<TAsynSessionStreamStatus>)
    begin
      Self.AsynStreamEvents(SessionId, ThreadId, Event, CallbackParams);
    end);
end;

{ TSessionThreadEventsAsynchronousSupport }

procedure TSessionThreadEventsAsynchronousSupport.AsynList(const SessionId, ThreadId: string;
  const CallBacks: TFunc<TAsynSessionEventList>);
begin
  with TAsynCallBackExec<TAsynSessionEventList, TSessionEventList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionEventList
      begin
        Result := Self.List(SessionId, ThreadId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadEventsAsynchronousSupport.AsynList(const SessionId, ThreadId: string;
  const ParamProc: TSessionSimpleListParamProc; const CallBacks: TFunc<TAsynSessionEventList>);
begin
  with TAsynCallBackExec<TAsynSessionEventList, TSessionEventList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionEventList
      begin
        Result := Self.List(SessionId, ThreadId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadEventsAsynchronousSupport.AsynStreamRaw(const SessionId, ThreadId: string;
  const CallBacks: TFunc<TAsynSessionStream>);
begin
  with TAsynCallBackExec<TAsynSessionStream, TSessionStream>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TSessionStream
      begin
        Result := Self.StreamRaw(SessionId, ThreadId);
      end);
  finally
    Free;
  end;
end;

procedure TSessionThreadEventsAsynchronousSupport.AsynStreamRaw(
  const SessionId, ThreadId: string; const Response: TStream;
  Event: TReceiveDataCallback; const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, Event,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamRaw(SessionId, ThreadId, Response, StreamEvent);
    end);
end;

procedure TSessionThreadEventsAsynchronousSupport.AsynStreamEvents(
  const SessionId, ThreadId: string; const Event: TSessionStreamEvent;
  const CallBacks: TFunc<TAsynSessionStreamStatus>);
begin
  TSessionStreamRawAsyncSupport.Run(Self, CallBacks, nil,
    function(const StreamEvent: TReceiveDataCallback): Integer
    begin
      Result := Self.StreamEvents(SessionId, ThreadId, Event, StreamEvent);
    end);
end;

end.
