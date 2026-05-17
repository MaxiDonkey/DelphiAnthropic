unit Anthropic.MemoryStore;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Json.Types,
  Anthropic.API.Params, Anthropic.API, Anthropic.Types,
  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  TMemoryStoreCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the human-readable memory store name.
    /// </summary>
    function Name(const Value: string): TMemoryStoreCreateParams;

    /// <summary>
    /// Sets the optional memory store description.
    /// </summary>
    function Description(const Value: string): TMemoryStoreCreateParams;

    /// <summary>
    /// Sets arbitrary memory store metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TMemoryStoreCreateParams; overload;

    /// <summary>
    /// Adds or replaces a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TMemoryStoreCreateParams; overload;

    class function New: TMemoryStoreCreateParams;
  end;

  TMemoryStoreUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Replaces the human-readable memory store name.
    /// </summary>
    function Name(const Value: string): TMemoryStoreUpdateParams;

    /// <summary>
    /// Replaces the memory store description. Pass an empty string to clear it.
    /// </summary>
    function Description(const Value: string): TMemoryStoreUpdateParams;

    /// <summary>
    /// Patches memory store metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TMemoryStoreUpdateParams; overload;

    /// <summary>
    /// Upserts a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TMemoryStoreUpdateParams; overload;

    /// <summary>
    /// Deletes a single metadata key by sending JSON null for that key.
    /// </summary>
    function DeleteMetadata(const Key: string): TMemoryStoreUpdateParams;

    class function New: TMemoryStoreUpdateParams;
  end;

  TMemoryStoreListParams = class(TUrlParam)
  public
    /// <summary>
    /// Filters stores created at or after the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtGte(const Value: string): TMemoryStoreListParams;

    /// <summary>
    /// Filters stores created at or before the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtLte(const Value: string): TMemoryStoreListParams;

    /// <summary>
    /// Includes archived stores in the list response.
    /// </summary>
    function IncludeArchived(const Value: Boolean): TMemoryStoreListParams;

    /// <summary>
    /// Sets the maximum number of stores returned per page.
    /// </summary>
    function Limit(const Value: Integer): TMemoryStoreListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TMemoryStoreListParams;

    class function New: TMemoryStoreListParams;
  end;

  TMemoryViewParams = class(TUrlParam)
  public
    /// <summary>
    /// Selects the memory representation view. Valid API values are basic and full.
    /// </summary>
    function View(const Value: string): TMemoryViewParams;

    /// <summary>
    /// Selects the basic memory representation.
    /// </summary>
    function Basic: TMemoryViewParams;

    /// <summary>
    /// Selects the full memory representation, including content when available.
    /// </summary>
    function Full: TMemoryViewParams;

    class function New: TMemoryViewParams;
  end;

  TMemoryCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the UTF-8 text content for the memory.
    /// </summary>
    function Content(const Value: string): TMemoryCreateParams;

    /// <summary>
    /// Sets the hierarchical path for the memory.
    /// </summary>
    function Path(const Value: string): TMemoryCreateParams;

    class function New: TMemoryCreateParams;
  end;

  TMemoryPreconditionParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the precondition type. The current API value is content_sha256.
    /// </summary>
    function &Type(const Value: string = 'content_sha256'): TMemoryPreconditionParams;

    /// <summary>
    /// Sets the expected SHA-256 digest of the stored memory content.
    /// </summary>
    function ContentSHA256(const Value: string): TMemoryPreconditionParams;

    class function New: TMemoryPreconditionParams;
  end;

  TMemoryUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Replaces the UTF-8 text content for the memory.
    /// </summary>
    function Content(const Value: string): TMemoryUpdateParams;

    /// <summary>
    /// Renames the memory by replacing its hierarchical path.
    /// </summary>
    function Path(const Value: string): TMemoryUpdateParams;

    /// <summary>
    /// Sets the optimistic-concurrency precondition for the update.
    /// </summary>
    function Precondition(const Value: TMemoryPreconditionParams): TMemoryUpdateParams;

    class function New: TMemoryUpdateParams;
  end;

  TMemoryListParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the rollup depth for directory-like listing.
    /// </summary>
    function Depth(const Value: Integer): TMemoryListParams;

    /// <summary>
    /// Sets the maximum number of memories or prefixes returned per page.
    /// </summary>
    function Limit(const Value: Integer): TMemoryListParams;

    /// <summary>
    /// Sets the list order. Valid API values are asc and desc.
    /// </summary>
    function Order(const Value: string): TMemoryListParams;

    /// <summary>
    /// Sets the field used for ordering.
    /// </summary>
    function OrderBy(const Value: string): TMemoryListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TMemoryListParams;

    /// <summary>
    /// Filters memories by raw path prefix.
    /// </summary>
    function PathPrefix(const Value: string): TMemoryListParams;

    /// <summary>
    /// Selects the memory representation view. Valid API values are basic and full.
    /// </summary>
    function View(const Value: string): TMemoryListParams;

    /// <summary>
    /// Selects ascending order.
    /// </summary>
    function Asc: TMemoryListParams;

    /// <summary>
    /// Selects descending order.
    /// </summary>
    function Desc: TMemoryListParams;

    /// <summary>
    /// Selects the basic memory representation.
    /// </summary>
    function Basic: TMemoryListParams;

    /// <summary>
    /// Selects the full memory representation, including content when available.
    /// </summary>
    function Full: TMemoryListParams;

    class function New: TMemoryListParams;
  end;

  TMemoryDeleteParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the expected content SHA-256 digest required for deletion.
    /// </summary>
    function ExpectedContentSHA256(const Value: string): TMemoryDeleteParams;

    class function New: TMemoryDeleteParams;
  end;

  TMemoryVersionListParams = class(TUrlParam)
  public
    /// <summary>
    /// Filters versions written by a specific API key identifier.
    /// </summary>
    function ApiKeyId(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Filters versions created at or after the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtGte(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Filters versions created at or before the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtLte(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Sets the maximum number of versions returned per page.
    /// </summary>
    function Limit(const Value: Integer): TMemoryVersionListParams;

    /// <summary>
    /// Filters versions for a specific memory identifier.
    /// </summary>
    function MemoryId(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Filters versions by operation. Valid API values are created, modified, and deleted.
    /// </summary>
    function Operation(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Filters versions written by a specific session identifier.
    /// </summary>
    function SessionId(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Selects the memory-version representation view. Valid API values are basic and full.
    /// </summary>
    function View(const Value: string): TMemoryVersionListParams;

    /// <summary>
    /// Selects versions created by create operations.
    /// </summary>
    function Created: TMemoryVersionListParams;

    /// <summary>
    /// Selects versions created by modification operations.
    /// </summary>
    function Modified: TMemoryVersionListParams;

    /// <summary>
    /// Selects versions created by delete operations.
    /// </summary>
    function Deleted: TMemoryVersionListParams;

    /// <summary>
    /// Selects the basic memory-version representation.
    /// </summary>
    function Basic: TMemoryVersionListParams;

    /// <summary>
    /// Selects the full memory-version representation, including content when available.
    /// </summary>
    function Full: TMemoryVersionListParams;

    class function New: TMemoryVersionListParams;
  end;

  TMemoryStoreCreateParamProc = TProc<TMemoryStoreCreateParams>;
  TMemoryStoreUpdateParamProc = TProc<TMemoryStoreUpdateParams>;
  TMemoryStoreListParamProc = TProc<TMemoryStoreListParams>;

  TMemoryCreateParamProc = TProc<TMemoryCreateParams>;
  TMemoryUpdateParamProc = TProc<TMemoryUpdateParams>;
  TMemoryListParamProc = TProc<TMemoryListParams>;
  TMemoryViewParamProc = TProc<TMemoryViewParams>;
  TMemoryDeleteParamProc = TProc<TMemoryDeleteParams>;

  TMemoryVersionListParamProc = TProc<TMemoryVersionListParams>;
  TMemoryVersionRetrieveParamProc = TProc<TMemoryViewParams>;

  TMemoryStore = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    FDescription: string;
    [JSONMarshalled(False)]
    FMetadata: string;
    FName: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Unique memory store identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 timestamp at which the store was archived, when applicable.
    /// </summary>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// RFC 3339 timestamp at which the store was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Human-readable store description.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Raw JSON object containing arbitrary store metadata.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;

    /// <summary>
    /// Human-readable store name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Object type, normally memory_store.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which the store was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
  end;

  TMemoryStoreDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Deleted memory store identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Deletion confirmation type, normally memory_store_deleted.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TMemoryStoreList = class(TJSONFingerprint)
  private
    FData: TArray<TMemoryStore>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Memory stores returned by the current page.
    /// </summary>
    property Data: TArray<TMemoryStore> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TMemoryListItem = class(TJSONFingerprint)
  protected
    FPath: string;
    FType: string;
  public
    /// <summary>
    /// Memory path or rolled-up prefix path.
    /// </summary>
    property Path: string read FPath write FPath;

    /// <summary>
    /// Object type, either memory or memory_prefix.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Returns true when this item is a concrete memory object.
    /// </summary>
    function IsMemory: Boolean;

    /// <summary>
    /// Returns true when this item is a rolled-up memory prefix marker.
    /// </summary>
    function IsMemoryPrefix: Boolean;
  end;

  TMemoryPrefix = class(TMemoryListItem);

  TMemory = class(TMemoryListItem)
  private
    FId: string;
    [JsonNameAttribute('content_sha256')]
    FContentSHA256: string;
    [JsonNameAttribute('content_size_bytes')]
    FContentSizeBytes: Int64;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('memory_store_id')]
    FMemoryStoreId: string;
    [JsonNameAttribute('memory_version_id')]
    FMemoryVersionId: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    FContent: string;
  public
    /// <summary>
    /// Unique memory identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Lowercase hex SHA-256 digest of the UTF-8 content bytes.
    /// </summary>
    property ContentSHA256: string read FContentSHA256 write FContentSHA256;

    /// <summary>
    /// Size of the UTF-8 content in bytes.
    /// </summary>
    property ContentSizeBytes: Int64 read FContentSizeBytes write FContentSizeBytes;

    /// <summary>
    /// RFC 3339 timestamp at which the memory was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Identifier of the memory store containing this memory.
    /// </summary>
    property MemoryStoreId: string read FMemoryStoreId write FMemoryStoreId;

    /// <summary>
    /// Identifier of the current head memory version.
    /// </summary>
    property MemoryVersionId: string read FMemoryVersionId write FMemoryVersionId;

    /// <summary>
    /// RFC 3339 timestamp at which the memory was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    /// <summary>
    /// UTF-8 text content when view=full; empty when absent or null.
    /// </summary>
    property Content: string read FContent write FContent;
  end;

  TMemoryDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Deleted memory identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Deletion confirmation type, normally memory_deleted.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TMemoryList = class(TJSONFingerprint)
  private
    [JSONMarshalled(False)]
    FData: TArray<TMemoryListItem>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Page of memory objects and, when depth is set, memory_prefix rollup markers.
    /// </summary>
    property Data: TArray<TMemoryListItem> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TMemoryActor = class(TJSONFingerprint)
  private
    [JsonNameAttribute('api_key_id')]
    FApiKeyId: string;
    [JsonNameAttribute('session_id')]
    FSessionId: string;
    FType: string;
    [JsonNameAttribute('user_id')]
    FUserId: string;
  public
    /// <summary>
    /// API key identifier for api_actor attribution.
    /// </summary>
    property ApiKeyId: string read FApiKeyId write FApiKeyId;

    /// <summary>
    /// Session identifier for session_actor attribution.
    /// </summary>
    property SessionId: string read FSessionId write FSessionId;

    /// <summary>
    /// Actor type: api_actor, session_actor, or user_actor.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// User identifier for user_actor attribution.
    /// </summary>
    property UserId: string read FUserId write FUserId;

    function IsAPIActor: Boolean;
    function IsSessionActor: Boolean;
    function IsUserActor: Boolean;
  end;

  TMemoryVersion = class(TJSONFingerprint)
  private
    FId: string;
    FContent: string;
    [JsonNameAttribute('content_sha256')]
    FContentSHA256: string;
    [JSONMarshalled(False)]
    [JsonNameAttribute('content_size_bytes')]
    FContentSizeBytes: Int64;
    [JSONMarshalled(False)]
    FHasContentSizeBytes: Boolean;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('created_by')]
    FCreatedBy: TMemoryActor;
    [JsonNameAttribute('memory_id')]
    FMemoryId: string;
    [JsonNameAttribute('memory_store_id')]
    FMemoryStoreId: string;
    FOperation: string;
    FPath: string;
    [JsonNameAttribute('redacted_at')]
    FRedactedAt: string;
    [JsonNameAttribute('redacted_by')]
    FRedactedBy: TMemoryActor;
    FType: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Unique memory version identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// UTF-8 text content as of this version when view=full and not redacted/deleted.
    /// </summary>
    property Content: string read FContent write FContent;

    /// <summary>
    /// Lowercase hex SHA-256 digest for this version content when available.
    /// </summary>
    property ContentSHA256: string read FContentSHA256 write FContentSHA256;

    /// <summary>
    /// Size of this version content in bytes when available.
    /// </summary>
    property ContentSizeBytes: Int64 read FContentSizeBytes write FContentSizeBytes;

    /// <summary>
    /// Indicates whether the content_size_bytes field was populated by the API.
    /// </summary>
    property HasContentSizeBytes: Boolean read FHasContentSizeBytes write FHasContentSizeBytes;

    /// <summary>
    /// RFC 3339 timestamp at which the version was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Actor that created this version.
    /// </summary>
    property CreatedBy: TMemoryActor read FCreatedBy write FCreatedBy;

    /// <summary>
    /// Identifier of the memory this version snapshots.
    /// </summary>
    property MemoryId: string read FMemoryId write FMemoryId;

    /// <summary>
    /// Identifier of the memory store containing this version.
    /// </summary>
    property MemoryStoreId: string read FMemoryStoreId write FMemoryStoreId;

    /// <summary>
    /// Mutation operation recorded by this version: created, modified, or deleted.
    /// </summary>
    property Operation: string read FOperation write FOperation;

    /// <summary>
    /// Memory path at the time of this version when available.
    /// </summary>
    property Path: string read FPath write FPath;

    /// <summary>
    /// RFC 3339 timestamp at which this version was redacted, when applicable.
    /// </summary>
    property RedactedAt: string read FRedactedAt write FRedactedAt;

    /// <summary>
    /// Actor that redacted this version, when applicable.
    /// </summary>
    property RedactedBy: TMemoryActor read FRedactedBy write FRedactedBy;

    /// <summary>
    /// Object type, normally memory_version.
    /// </summary>
    property &Type: string read FType write FType;

    destructor Destroy; override;
  end;

  TMemoryVersionList = class(TJSONFingerprint)
  private
    FData: TArray<TMemoryVersion>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Memory versions returned by the current page.
    /// </summary>
    property Data: TArray<TMemoryVersion> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TAsynMemoryStore = TAsynCallBack<TMemoryStore>;
  TPromiseMemoryStore = TPromiseCallback<TMemoryStore>;
  TAsynMemoryStoreList = TAsynCallBack<TMemoryStoreList>;
  TPromiseMemoryStoreList = TPromiseCallback<TMemoryStoreList>;
  TAsynMemoryStoreDeleted = TAsynCallBack<TMemoryStoreDeleted>;
  TPromiseMemoryStoreDeleted = TPromiseCallback<TMemoryStoreDeleted>;

  TAsynMemory = TAsynCallBack<TMemory>;
  TPromiseMemory = TPromiseCallback<TMemory>;
  TAsynMemoryList = TAsynCallBack<TMemoryList>;
  TPromiseMemoryList = TPromiseCallback<TMemoryList>;
  TAsynMemoryDeleted = TAsynCallBack<TMemoryDeleted>;
  TPromiseMemoryDeleted = TPromiseCallback<TMemoryDeleted>;

  TAsynMemoryVersion = TAsynCallBack<TMemoryVersion>;
  TPromiseMemoryVersion = TPromiseCallback<TMemoryVersion>;
  TAsynMemoryVersionList = TAsynCallBack<TMemoryVersionList>;
  TPromiseMemoryVersionList = TPromiseCallback<TMemoryVersionList>;

  TMemoriesAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc): TMemory; overload; virtual; abstract;
    function Create(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc; const QueryParamProc: TMemoryViewParamProc): TMemory; overload; virtual; abstract;
    function List(const MemoryStoreId: string): TMemoryList; overload; virtual; abstract;
    function List(const MemoryStoreId: string; const ParamProc: TMemoryListParamProc): TMemoryList; overload; virtual; abstract;
    function Retrieve(const MemoryStoreId, MemoryId: string): TMemory; overload; virtual; abstract;
    function Retrieve(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryViewParamProc): TMemory; overload; virtual; abstract;
    function Update(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc): TMemory; overload; virtual; abstract;
    function Update(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc; const QueryParamProc: TMemoryViewParamProc): TMemory; overload; virtual; abstract;
    function Delete(const MemoryStoreId, MemoryId: string): TMemoryDeleted; overload; virtual; abstract;
    function Delete(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryDeleteParamProc): TMemoryDeleted; overload; virtual; abstract;
  end;

  TMemoriesAsynchronousSupport = class(TMemoriesAbstractSupport)
  protected
    procedure AsynCreate(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynCreate(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc; const QueryParamProc: TMemoryViewParamProc; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynList(const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryList>); overload;
    procedure AsynList(const MemoryStoreId: string; const ParamProc: TMemoryListParamProc; const CallBacks: TFunc<TAsynMemoryList>); overload;
    procedure AsynRetrieve(const MemoryStoreId, MemoryId: string; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynRetrieve(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryViewParamProc; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynUpdate(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynUpdate(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc; const QueryParamProc: TMemoryViewParamProc; const CallBacks: TFunc<TAsynMemory>); overload;
    procedure AsynDelete(const MemoryStoreId, MemoryId: string; const CallBacks: TFunc<TAsynMemoryDeleted>); overload;
    procedure AsynDelete(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryDeleteParamProc; const CallBacks: TFunc<TAsynMemoryDeleted>); overload;
  end;

  TMemoriesRoute = class(TMemoriesAsynchronousSupport)
  public
    /// <summary>
    /// Creates a memory in the specified memory store.
    /// </summary>
    function Create(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc): TMemory; overload; override;

    /// <summary>
    /// Creates a memory and returns it using the requested representation view.
    /// </summary>
    function Create(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc; const QueryParamProc: TMemoryViewParamProc): TMemory; overload; override;

    /// <summary>
    /// Lists memories in the specified memory store.
    /// </summary>
    function List(const MemoryStoreId: string): TMemoryList; overload; override;

    /// <summary>
    /// Lists memories in the specified memory store using filters and pagination.
    /// </summary>
    function List(const MemoryStoreId: string; const ParamProc: TMemoryListParamProc): TMemoryList; overload; override;

    /// <summary>
    /// Retrieves a memory by identifier from the specified memory store.
    /// </summary>
    function Retrieve(const MemoryStoreId, MemoryId: string): TMemory; overload; override;

    /// <summary>
    /// Retrieves a memory by identifier using the requested representation view.
    /// </summary>
    function Retrieve(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryViewParamProc): TMemory; overload; override;

    /// <summary>
    /// Updates a memory content, path, or precondition in the specified memory store.
    /// </summary>
    function Update(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc): TMemory; overload; override;

    /// <summary>
    /// Updates a memory and returns it using the requested representation view.
    /// </summary>
    function Update(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc; const QueryParamProc: TMemoryViewParamProc): TMemory; overload; override;

    /// <summary>
    /// Deletes a memory from the specified memory store.
    /// </summary>
    function Delete(const MemoryStoreId, MemoryId: string): TMemoryDeleted; overload; override;

    /// <summary>
    /// Deletes a memory using an optional content SHA-256 precondition.
    /// </summary>
    function Delete(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryDeleteParamProc): TMemoryDeleted; overload; override;

    /// <summary>
    /// Asynchronously creates a memory in the specified memory store.
    /// </summary>
    function AsyncAwaitCreate(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously creates a memory using the requested representation view.
    /// </summary>
    function AsyncAwaitCreate(const MemoryStoreId: string; const ParamProc: TMemoryCreateParamProc; const QueryParamProc: TMemoryViewParamProc;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously lists memories in the specified memory store.
    /// </summary>
    function AsyncAwaitList(const MemoryStoreId: string;
      const Callbacks: TFunc<TPromiseMemoryList> = nil): TPromise<TMemoryList>; overload;

    /// <summary>
    /// Asynchronously lists memories using filters and pagination.
    /// </summary>
    function AsyncAwaitList(const MemoryStoreId: string; const ParamProc: TMemoryListParamProc;
      const Callbacks: TFunc<TPromiseMemoryList> = nil): TPromise<TMemoryList>; overload;

    /// <summary>
    /// Asynchronously retrieves a memory by identifier from the specified memory store.
    /// </summary>
    function AsyncAwaitRetrieve(const MemoryStoreId, MemoryId: string;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously retrieves a memory using the requested representation view.
    /// </summary>
    function AsyncAwaitRetrieve(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryViewParamProc;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously updates a memory in the specified memory store.
    /// </summary>
    function AsyncAwaitUpdate(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously updates a memory using the requested representation view.
    /// </summary>
    function AsyncAwaitUpdate(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryUpdateParamProc; const QueryParamProc: TMemoryViewParamProc;
      const Callbacks: TFunc<TPromiseMemory> = nil): TPromise<TMemory>; overload;

    /// <summary>
    /// Asynchronously deletes a memory from the specified memory store.
    /// </summary>
    function AsyncAwaitDelete(const MemoryStoreId, MemoryId: string;
      const Callbacks: TFunc<TPromiseMemoryDeleted> = nil): TPromise<TMemoryDeleted>; overload;

    /// <summary>
    /// Asynchronously deletes a memory using an optional content SHA-256 precondition.
    /// </summary>
    function AsyncAwaitDelete(const MemoryStoreId, MemoryId: string; const ParamProc: TMemoryDeleteParamProc;
      const Callbacks: TFunc<TPromiseMemoryDeleted> = nil): TPromise<TMemoryDeleted>; overload;
  end;

  TMemoryVersionsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function List(const MemoryStoreId: string): TMemoryVersionList; overload; virtual; abstract;
    function List(const MemoryStoreId: string; const ParamProc: TMemoryVersionListParamProc): TMemoryVersionList; overload; virtual; abstract;
    function Retrieve(const MemoryStoreId, MemoryVersionId: string): TMemoryVersion; overload; virtual; abstract;
    function Retrieve(const MemoryStoreId, MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc): TMemoryVersion; overload; virtual; abstract;
    function Redact(const MemoryStoreId, MemoryVersionId: string): TMemoryVersion; overload; virtual; abstract;
  end;

  TMemoryVersionsAsynchronousSupport = class(TMemoryVersionsAbstractSupport)
  protected
    procedure AsynList(const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryVersionList>); overload;
    procedure AsynList(const MemoryStoreId: string; const ParamProc: TMemoryVersionListParamProc; const CallBacks: TFunc<TAsynMemoryVersionList>); overload;
    procedure AsynRetrieve(const MemoryStoreId, MemoryVersionId: string; const CallBacks: TFunc<TAsynMemoryVersion>); overload;
    procedure AsynRetrieve(const MemoryStoreId, MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc; const CallBacks: TFunc<TAsynMemoryVersion>); overload;
    procedure AsynRedact(const MemoryStoreId, MemoryVersionId: string; const CallBacks: TFunc<TAsynMemoryVersion>); overload;
  end;

  TMemoryVersionsRoute = class(TMemoryVersionsAsynchronousSupport)
  public
    /// <summary>
    /// Lists memory versions in the specified memory store.
    /// </summary>
    function List(const MemoryStoreId: string): TMemoryVersionList; overload; override;

    /// <summary>
    /// Lists memory versions in the specified memory store using filters and pagination.
    /// </summary>
    function List(const MemoryStoreId: string; const ParamProc: TMemoryVersionListParamProc): TMemoryVersionList; overload; override;

    /// <summary>
    /// Retrieves a memory version by identifier from the specified memory store.
    /// </summary>
    function Retrieve(const MemoryStoreId, MemoryVersionId: string): TMemoryVersion; overload; override;

    /// <summary>
    /// Retrieves a memory version using the requested representation view.
    /// </summary>
    function Retrieve(const MemoryStoreId, MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc): TMemoryVersion; overload; override;

    /// <summary>
    /// Redacts a memory version in the specified memory store.
    /// </summary>
    function Redact(const MemoryStoreId, MemoryVersionId: string): TMemoryVersion; overload; override;

    /// <summary>
    /// Asynchronously lists memory versions in the specified memory store.
    /// </summary>
    function AsyncAwaitList(const MemoryStoreId: string;
      const Callbacks: TFunc<TPromiseMemoryVersionList> = nil): TPromise<TMemoryVersionList>; overload;

    /// <summary>
    /// Asynchronously lists memory versions using filters and pagination.
    /// </summary>
    function AsyncAwaitList(const MemoryStoreId: string; const ParamProc: TMemoryVersionListParamProc;
      const Callbacks: TFunc<TPromiseMemoryVersionList> = nil): TPromise<TMemoryVersionList>; overload;

    /// <summary>
    /// Asynchronously retrieves a memory version by identifier.
    /// </summary>
    function AsyncAwaitRetrieve(const MemoryStoreId, MemoryVersionId: string;
      const Callbacks: TFunc<TPromiseMemoryVersion> = nil): TPromise<TMemoryVersion>; overload;

    /// <summary>
    /// Asynchronously retrieves a memory version using the requested representation view.
    /// </summary>
    function AsyncAwaitRetrieve(const MemoryStoreId, MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc;
      const Callbacks: TFunc<TPromiseMemoryVersion> = nil): TPromise<TMemoryVersion>; overload;

    /// <summary>
    /// Asynchronously redacts a memory version in the specified memory store.
    /// </summary>
    function AsyncAwaitRedact(const MemoryStoreId, MemoryVersionId: string;
      const Callbacks: TFunc<TPromiseMemoryVersion> = nil): TPromise<TMemoryVersion>; overload;
  end;

  TMemoryStoresAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const ParamProc: TMemoryStoreCreateParamProc): TMemoryStore; overload; virtual; abstract;
    function List: TMemoryStoreList; overload; virtual; abstract;
    function List(const ParamProc: TMemoryStoreListParamProc): TMemoryStoreList; overload; virtual; abstract;
    function Retrieve(const MemoryStoreId: string): TMemoryStore; overload; virtual; abstract;
    function Update(const MemoryStoreId: string; const ParamProc: TMemoryStoreUpdateParamProc): TMemoryStore; overload; virtual; abstract;
    function Delete(const MemoryStoreId: string): TMemoryStoreDeleted; overload; virtual; abstract;
    function Archive(const MemoryStoreId: string): TMemoryStore; overload; virtual; abstract;
  end;

  TMemoryStoresAsynchronousSupport = class(TMemoryStoresAbstractSupport)
  protected
    procedure AsynCreate(const ParamProc: TMemoryStoreCreateParamProc; const CallBacks: TFunc<TAsynMemoryStore>); overload;
    procedure AsynList(const CallBacks: TFunc<TAsynMemoryStoreList>); overload;
    procedure AsynList(const ParamProc: TMemoryStoreListParamProc; const CallBacks: TFunc<TAsynMemoryStoreList>); overload;
    procedure AsynRetrieve(const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryStore>); overload;
    procedure AsynUpdate(const MemoryStoreId: string; const ParamProc: TMemoryStoreUpdateParamProc; const CallBacks: TFunc<TAsynMemoryStore>); overload;
    procedure AsynDelete(const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryStoreDeleted>); overload;
    procedure AsynArchive(const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryStore>); overload;
  end;

  TMemoryStoresRoute = class(TMemoryStoresAsynchronousSupport)
  private
    FMemories: TMemoriesRoute;
    FMemoryVersions: TMemoryVersionsRoute;
    function GetMemoriesRoute: TMemoriesRoute;
    function GetMemoryVersionsRoute: TMemoryVersionsRoute;
  public
    /// <summary>
    /// Provides access to memory operations scoped to a memory store.
    /// </summary>
    property Memories: TMemoriesRoute read GetMemoriesRoute;

    /// <summary>
    /// Provides access to memory version operations scoped to a memory store.
    /// </summary>
    property MemoryVersions: TMemoryVersionsRoute read GetMemoryVersionsRoute;

    /// <summary>
    /// Creates a memory store.
    /// </summary>
    function Create(const ParamProc: TMemoryStoreCreateParamProc): TMemoryStore; overload; override;

    /// <summary>
    /// Lists memory stores.
    /// </summary>
    function List: TMemoryStoreList; overload; override;

    /// <summary>
    /// Lists memory stores using filters and pagination.
    /// </summary>
    function List(const ParamProc: TMemoryStoreListParamProc): TMemoryStoreList; overload; override;

    /// <summary>
    /// Retrieves a memory store by identifier.
    /// </summary>
    function Retrieve(const MemoryStoreId: string): TMemoryStore; overload; override;

    /// <summary>
    /// Updates a memory store name, description, or metadata.
    /// </summary>
    function Update(const MemoryStoreId: string; const ParamProc: TMemoryStoreUpdateParamProc): TMemoryStore; overload; override;

    /// <summary>
    /// Deletes a memory store and its memories.
    /// </summary>
    function Delete(const MemoryStoreId: string): TMemoryStoreDeleted; overload; override;

    /// <summary>
    /// Archives a memory store.
    /// </summary>
    function Archive(const MemoryStoreId: string): TMemoryStore; overload; override;

    /// <summary>
    /// Asynchronously creates a memory store.
    /// </summary>
    function AsyncAwaitCreate(const ParamProc: TMemoryStoreCreateParamProc;
      const Callbacks: TFunc<TPromiseMemoryStore> = nil): TPromise<TMemoryStore>; overload;

    /// <summary>
    /// Asynchronously lists memory stores.
    /// </summary>
    function AsyncAwaitList(const Callbacks: TFunc<TPromiseMemoryStoreList> = nil): TPromise<TMemoryStoreList>; overload;

    /// <summary>
    /// Asynchronously lists memory stores using filters and pagination.
    /// </summary>
    function AsyncAwaitList(const ParamProc: TMemoryStoreListParamProc;
      const Callbacks: TFunc<TPromiseMemoryStoreList> = nil): TPromise<TMemoryStoreList>; overload;

    /// <summary>
    /// Asynchronously retrieves a memory store by identifier.
    /// </summary>
    function AsyncAwaitRetrieve(const MemoryStoreId: string;
      const Callbacks: TFunc<TPromiseMemoryStore> = nil): TPromise<TMemoryStore>; overload;

    /// <summary>
    /// Asynchronously updates a memory store name, description, or metadata.
    /// </summary>
    function AsyncAwaitUpdate(const MemoryStoreId: string; const ParamProc: TMemoryStoreUpdateParamProc;
      const Callbacks: TFunc<TPromiseMemoryStore> = nil): TPromise<TMemoryStore>; overload;

    /// <summary>
    /// Asynchronously deletes a memory store and its memories.
    /// </summary>
    function AsyncAwaitDelete(const MemoryStoreId: string;
      const Callbacks: TFunc<TPromiseMemoryStoreDeleted> = nil): TPromise<TMemoryStoreDeleted>; overload;

    /// <summary>
    /// Asynchronously archives a memory store.
    /// </summary>
    function AsyncAwaitArchive(const MemoryStoreId: string;
      const Callbacks: TFunc<TPromiseMemoryStore> = nil): TPromise<TMemoryStore>; overload;

    destructor Destroy; override;
  end;

implementation

uses
  Anthropic.API.JsonSafeReader;

type
  TMemoryStoreResponseHydrator = record
  private
    class function ArrayItemJson(const Root: TJsonReader; const ArrayPath: string;
      const Index: Integer): string; static;
    class function TryReadInt64(const Root: TJsonReader; const Path: string;
      out Value: Int64): Boolean; static;
  public
    class procedure HydrateMemoryStore(const Store: TMemoryStore); static;
    class procedure HydrateMemoryStoreList(const List: TMemoryStoreList); static;
    class procedure HydrateMemoryList(const List: TMemoryList); static;
    class procedure HydrateMemoryVersion(const Version: TMemoryVersion); static;
    class procedure HydrateMemoryVersionList(const List: TMemoryVersionList); static;
  end;

  TMemoryStoreRouteQuery = record
  public
    class function WithView(const Path: string;
      const ParamProc: TMemoryViewParamProc): string; static;
    class function WithDelete(const Path: string;
      const ParamProc: TMemoryDeleteParamProc): string; static;
  end;

{ TMemoryStoreRouteQuery }

class function TMemoryStoreRouteQuery.WithView(const Path: string;
  const ParamProc: TMemoryViewParamProc): string;
begin
  Result := Path;
  if not Assigned(ParamProc) then
    Exit;

  var Params := TMemoryViewParams.Create;
  try
    ParamProc(Params);
    Result := Result + Params.ToQueryString;
  finally
    Params.Free;
  end;
end;

class function TMemoryStoreRouteQuery.WithDelete(const Path: string;
  const ParamProc: TMemoryDeleteParamProc): string;
begin
  Result := Path;
  if not Assigned(ParamProc) then
    Exit;

  var Params := TMemoryDeleteParams.Create;
  try
    ParamProc(Params);
    Result := Result + Params.ToQueryString;
  finally
    Params.Free;
  end;
end;

{ TMemoryStoreResponseHydrator }

class function TMemoryStoreResponseHydrator.ArrayItemJson(
  const Root: TJsonReader; const ArrayPath: string;
  const Index: Integer): string;
begin
  Result := EmptyStr;

  if (not Root.IsValid) or ArrayPath.IsEmpty or (Index < 0) then
    Exit;

  Result := Root.ExtractSubJson(Format('%s[%d]', [ArrayPath, Index]));
end;

class function TMemoryStoreResponseHydrator.TryReadInt64(
  const Root: TJsonReader; const Path: string; out Value: Int64): Boolean;
begin
  Value := 0;
  Result := False;

  if (not Root.IsValid) or Path.IsEmpty then
    Exit;

  if Root.IsNullNode(Path) or
     not (Root.IsNumberNode(Path) or Root.IsStringNode(Path)) then
    Exit;

  Result := TryStrToInt64(Root.AsString(Path).Trim, Value);
end;

class procedure TMemoryStoreResponseHydrator.HydrateMemoryStore(
  const Store: TMemoryStore);
begin
  if (Store = nil) or Store.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Store.JSONResponse);
  if not Root.IsValid then
    Exit;

  if Root.IsObjectNode('metadata') then
    Store.FMetadata := Root.ObjectText('metadata', Store.FMetadata)
  else if Root.IsNullNode('metadata') then
    Store.FMetadata := EmptyStr;
end;

class procedure TMemoryStoreResponseHydrator.HydrateMemoryStoreList(
  const List: TMemoryStoreList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  for var Existing in List.FData do
    Existing.Free;
  SetLength(List.FData, 0);

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  var Items: TArray<TMemoryStore>;
  SetLength(Items, Root.Count('data'));
  for var I := 0 to High(Items) do
    begin
      var JsonText := ArrayItemJson(Root, 'data', I);
      if JsonText.Trim.IsEmpty then
        Continue;

      Items[I] := TApiDeserializer.Parse<TMemoryStore>(JsonText, True);
    end;

  List.FData := Items;
end;

class procedure TMemoryStoreResponseHydrator.HydrateMemoryList(
  const List: TMemoryList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  for var Existing in List.FData do
    Existing.Free;
  SetLength(List.FData, 0);

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  var Items: TArray<TMemoryListItem>;
  SetLength(Items, Root.Count('data'));
  for var I := 0 to High(Items) do
    begin
      var JsonText := ArrayItemJson(Root, 'data', I);
      if JsonText.Trim.IsEmpty then
        Continue;

      var Item: TMemoryListItem := nil;
      var ItemRoot := TJsonReader.Parse(JsonText);
      if ItemRoot.IsValid then
        begin
          var ItemType := ItemRoot.AsString('type');
          if SameText(ItemType, 'memory_prefix') then
            Item := TApiDeserializer.Parse<TMemoryPrefix>(JsonText, True)
          else
            Item := TApiDeserializer.Parse<TMemory>(JsonText, True);
        end;

      Items[I] := Item;
    end;

  List.FData := Items;
end;

class procedure TMemoryStoreResponseHydrator.HydrateMemoryVersion(
  const Version: TMemoryVersion);
begin
  if (Version = nil) or Version.JSONResponse.Trim.IsEmpty then
    Exit;

  Version.FHasContentSizeBytes := False;
  Version.FContentSizeBytes := 0;

  var Root := TJsonReader.Parse(Version.JSONResponse);
  if not Root.IsValid then
    Exit;

  var Value: Int64;
  if TryReadInt64(Root, 'content_size_bytes', Value) then
    begin
      Version.FContentSizeBytes := Value;
      Version.FHasContentSizeBytes := True;
    end;
end;

class procedure TMemoryStoreResponseHydrator.HydrateMemoryVersionList(
  const List: TMemoryVersionList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  for var Existing in List.FData do
    Existing.Free;
  SetLength(List.FData, 0);

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  var Items: TArray<TMemoryVersion>;
  SetLength(Items, Root.Count('data'));
  for var I := 0 to High(Items) do
    begin
      var JsonText := ArrayItemJson(Root, 'data', I);
      if JsonText.Trim.IsEmpty then
        Continue;

      Items[I] := TApiDeserializer.Parse<TMemoryVersion>(JsonText, True);
    end;

  List.FData := Items;
end;

{ TMemoryStoreCreateParams }

class function TMemoryStoreCreateParams.New: TMemoryStoreCreateParams;
begin
  Result := TMemoryStoreCreateParams.Create;
end;

function TMemoryStoreCreateParams.Description(
  const Value: string): TMemoryStoreCreateParams;
begin
  Result := TMemoryStoreCreateParams(Add('description', Value));
end;

function TMemoryStoreCreateParams.Metadata(
  const Value: TJSONObject): TMemoryStoreCreateParams;
begin
  Result := TMemoryStoreCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TMemoryStoreCreateParams.Metadata(const Key,
  Value: string): TMemoryStoreCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TMemoryStoreCreateParams.Name(
  const Value: string): TMemoryStoreCreateParams;
begin
  Result := TMemoryStoreCreateParams(Add('name', Value));
end;

{ TMemoryStoreUpdateParams }

class function TMemoryStoreUpdateParams.New: TMemoryStoreUpdateParams;
begin
  Result := TMemoryStoreUpdateParams.Create;
end;

function TMemoryStoreUpdateParams.DeleteMetadata(
  const Key: string): TMemoryStoreUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

function TMemoryStoreUpdateParams.Description(
  const Value: string): TMemoryStoreUpdateParams;
begin
  Result := TMemoryStoreUpdateParams(Add('description', Value));
end;

function TMemoryStoreUpdateParams.Metadata(
  const Value: TJSONObject): TMemoryStoreUpdateParams;
begin
  Result := TMemoryStoreUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TMemoryStoreUpdateParams.Metadata(const Key,
  Value: string): TMemoryStoreUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TMemoryStoreUpdateParams.Name(
  const Value: string): TMemoryStoreUpdateParams;
begin
  Result := TMemoryStoreUpdateParams(Add('name', Value));
end;

{ TMemoryStoreListParams }

class function TMemoryStoreListParams.New: TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams.Create;
end;

function TMemoryStoreListParams.CreatedAtGte(
  const Value: string): TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams(Add('created_at[gte]', Value));
end;

function TMemoryStoreListParams.CreatedAtLte(
  const Value: string): TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams(Add('created_at[lte]', Value));
end;

function TMemoryStoreListParams.IncludeArchived(
  const Value: Boolean): TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams(Add('include_archived', Value));
end;

function TMemoryStoreListParams.Limit(
  const Value: Integer): TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams(Add('limit', Value));
end;

function TMemoryStoreListParams.Page(
  const Value: string): TMemoryStoreListParams;
begin
  Result := TMemoryStoreListParams(Add('page', Value));
end;

{ TMemoryViewParams }

class function TMemoryViewParams.New: TMemoryViewParams;
begin
  Result := TMemoryViewParams.Create;
end;

function TMemoryViewParams.Basic: TMemoryViewParams;
begin
  Result := View('basic');
end;

function TMemoryViewParams.Full: TMemoryViewParams;
begin
  Result := View('full');
end;

function TMemoryViewParams.View(const Value: string): TMemoryViewParams;
begin
  Result := TMemoryViewParams(Add('view', Value));
end;

{ TMemoryCreateParams }

class function TMemoryCreateParams.New: TMemoryCreateParams;
begin
  Result := TMemoryCreateParams.Create;
end;

function TMemoryCreateParams.Content(const Value: string): TMemoryCreateParams;
begin
  Result := TMemoryCreateParams(Add('content', Value));
end;

function TMemoryCreateParams.Path(const Value: string): TMemoryCreateParams;
begin
  Result := TMemoryCreateParams(Add('path', Value));
end;

{ TMemoryPreconditionParams }

class function TMemoryPreconditionParams.New: TMemoryPreconditionParams;
begin
  Result := TMemoryPreconditionParams.Create.&Type();
end;

function TMemoryPreconditionParams.ContentSHA256(
  const Value: string): TMemoryPreconditionParams;
begin
  Result := TMemoryPreconditionParams(Add('content_sha256', Value));
end;

function TMemoryPreconditionParams.&Type(
  const Value: string): TMemoryPreconditionParams;
begin
  Result := TMemoryPreconditionParams(Add('type', Value));
end;

{ TMemoryUpdateParams }

class function TMemoryUpdateParams.New: TMemoryUpdateParams;
begin
  Result := TMemoryUpdateParams.Create;
end;

function TMemoryUpdateParams.Content(const Value: string): TMemoryUpdateParams;
begin
  Result := TMemoryUpdateParams(Add('content', Value));
end;

function TMemoryUpdateParams.Path(const Value: string): TMemoryUpdateParams;
begin
  Result := TMemoryUpdateParams(Add('path', Value));
end;

function TMemoryUpdateParams.Precondition(
  const Value: TMemoryPreconditionParams): TMemoryUpdateParams;
begin
  Result := TMemoryUpdateParams(Add('precondition', Value.Detach));
end;

{ TMemoryListParams }

class function TMemoryListParams.New: TMemoryListParams;
begin
  Result := TMemoryListParams.Create;
end;

function TMemoryListParams.Asc: TMemoryListParams;
begin
  Result := Order('asc');
end;

function TMemoryListParams.Basic: TMemoryListParams;
begin
  Result := View('basic');
end;

function TMemoryListParams.Depth(const Value: Integer): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('depth', Value));
end;

function TMemoryListParams.Desc: TMemoryListParams;
begin
  Result := Order('desc');
end;

function TMemoryListParams.Full: TMemoryListParams;
begin
  Result := View('full');
end;

function TMemoryListParams.Limit(const Value: Integer): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('limit', Value));
end;

function TMemoryListParams.Order(const Value: string): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('order', Value));
end;

function TMemoryListParams.OrderBy(const Value: string): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('order_by', Value));
end;

function TMemoryListParams.Page(const Value: string): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('page', Value));
end;

function TMemoryListParams.PathPrefix(const Value: string): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('path_prefix', Value));
end;

function TMemoryListParams.View(const Value: string): TMemoryListParams;
begin
  Result := TMemoryListParams(Add('view', Value));
end;

{ TMemoryDeleteParams }

class function TMemoryDeleteParams.New: TMemoryDeleteParams;
begin
  Result := TMemoryDeleteParams.Create;
end;

function TMemoryDeleteParams.ExpectedContentSHA256(
  const Value: string): TMemoryDeleteParams;
begin
  Result := TMemoryDeleteParams(Add('expected_content_sha256', Value));
end;

{ TMemoryVersionListParams }

class function TMemoryVersionListParams.New: TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams.Create;
end;

function TMemoryVersionListParams.ApiKeyId(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('api_key_id', Value));
end;

function TMemoryVersionListParams.Basic: TMemoryVersionListParams;
begin
  Result := View('basic');
end;

function TMemoryVersionListParams.Created: TMemoryVersionListParams;
begin
  Result := Operation('created');
end;

function TMemoryVersionListParams.CreatedAtGte(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('created_at[gte]', Value));
end;

function TMemoryVersionListParams.CreatedAtLte(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('created_at[lte]', Value));
end;

function TMemoryVersionListParams.Deleted: TMemoryVersionListParams;
begin
  Result := Operation('deleted');
end;

function TMemoryVersionListParams.Full: TMemoryVersionListParams;
begin
  Result := View('full');
end;

function TMemoryVersionListParams.Limit(
  const Value: Integer): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('limit', Value));
end;

function TMemoryVersionListParams.MemoryId(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('memory_id', Value));
end;

function TMemoryVersionListParams.Modified: TMemoryVersionListParams;
begin
  Result := Operation('modified');
end;

function TMemoryVersionListParams.Operation(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('operation', Value));
end;

function TMemoryVersionListParams.Page(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('page', Value));
end;

function TMemoryVersionListParams.SessionId(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('session_id', Value));
end;

function TMemoryVersionListParams.View(
  const Value: string): TMemoryVersionListParams;
begin
  Result := TMemoryVersionListParams(Add('view', Value));
end;

{ TMemoryStore }

procedure TMemoryStore.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TMemoryStore.ContentUpdate;
begin
  inherited;
  TMemoryStoreResponseHydrator.HydrateMemoryStore(Self);
end;

{ TMemoryStoreList }

procedure TMemoryStoreList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TMemoryStoreList.ContentUpdate;
begin
  inherited;
  TMemoryStoreResponseHydrator.HydrateMemoryStoreList(Self);
end;

destructor TMemoryStoreList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TMemoryListItem }

function TMemoryListItem.IsMemory: Boolean;
begin
  Result := SameText(FType, 'memory');
end;

function TMemoryListItem.IsMemoryPrefix: Boolean;
begin
  Result := SameText(FType, 'memory_prefix');
end;

{ TMemoryList }

procedure TMemoryList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TMemoryList.ContentUpdate;
begin
  inherited;
  TMemoryStoreResponseHydrator.HydrateMemoryList(Self);
end;

destructor TMemoryList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TMemoryActor }

function TMemoryActor.IsAPIActor: Boolean;
begin
  Result := SameText(FType, 'api_actor');
end;

function TMemoryActor.IsSessionActor: Boolean;
begin
  Result := SameText(FType, 'session_actor');
end;

function TMemoryActor.IsUserActor: Boolean;
begin
  Result := SameText(FType, 'user_actor');
end;

{ TMemoryVersion }

procedure TMemoryVersion.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TMemoryVersion.ContentUpdate;
begin
  inherited;
  TMemoryStoreResponseHydrator.HydrateMemoryVersion(Self);
end;

destructor TMemoryVersion.Destroy;
begin
  FCreatedBy.Free;
  FRedactedBy.Free;
  inherited;
end;

{ TMemoryVersionList }

procedure TMemoryVersionList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TMemoryVersionList.ContentUpdate;
begin
  inherited;
  TMemoryStoreResponseHydrator.HydrateMemoryVersionList(Self);
end;

destructor TMemoryVersionList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TMemoriesRoute }

function TMemoriesRoute.Create(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc): TMemory;
begin
  Result := API.Post<TMemory, TMemoryCreateParams>(
    'memory_stores/' + MemoryStoreId + '/memories', ParamProc, True);
end;

function TMemoriesRoute.Create(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc;
  const QueryParamProc: TMemoryViewParamProc): TMemory;
begin
  Result := API.Post<TMemory, TMemoryCreateParams>(
    TMemoryStoreRouteQuery.WithView(
      'memory_stores/' + MemoryStoreId + '/memories',
      QueryParamProc),
    ParamProc,
    True);
end;

function TMemoriesRoute.Delete(const MemoryStoreId,
  MemoryId: string): TMemoryDeleted;
begin
  Result := API.Delete<TMemoryDeleted>(
    'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId);
end;

function TMemoriesRoute.Delete(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryDeleteParamProc): TMemoryDeleted;
begin
  Result := API.Delete<TMemoryDeleted>(
    TMemoryStoreRouteQuery.WithDelete(
      'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId,
      ParamProc));
end;

function TMemoriesRoute.List(const MemoryStoreId: string): TMemoryList;
begin
  Result := API.Get<TMemoryList>(
    'memory_stores/' + MemoryStoreId + '/memories');
end;

function TMemoriesRoute.List(const MemoryStoreId: string;
  const ParamProc: TMemoryListParamProc): TMemoryList;
begin
  Result := API.Get<TMemoryList, TMemoryListParams>(
    'memory_stores/' + MemoryStoreId + '/memories', ParamProc);
end;

function TMemoriesRoute.Retrieve(const MemoryStoreId,
  MemoryId: string): TMemory;
begin
  Result := API.Get<TMemory>(
    'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId);
end;

function TMemoriesRoute.Retrieve(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryViewParamProc): TMemory;
begin
  Result := API.Get<TMemory, TMemoryViewParams>(
    'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId, ParamProc);
end;

function TMemoriesRoute.Update(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryUpdateParamProc): TMemory;
begin
  Result := API.Post<TMemory, TMemoryUpdateParams>(
    'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId, ParamProc, True);
end;

function TMemoriesRoute.Update(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryUpdateParamProc;
  const QueryParamProc: TMemoryViewParamProc): TMemory;
begin
  Result := API.Post<TMemory, TMemoryUpdateParams>(
    TMemoryStoreRouteQuery.WithView(
      'memory_stores/' + MemoryStoreId + '/memories/' + MemoryId,
      QueryParamProc),
    ParamProc,
    True);
end;

function TMemoriesRoute.AsyncAwaitCreate(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc;
  const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynCreate(MemoryStoreId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitCreate(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc;
  const QueryParamProc: TMemoryViewParamProc;
  const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynCreate(MemoryStoreId, ParamProc, QueryParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitDelete(const MemoryStoreId,
  MemoryId: string; const Callbacks: TFunc<TPromiseMemoryDeleted>): TPromise<TMemoryDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryDeleted>(
    procedure(const CallbackParams: TFunc<TAsynMemoryDeleted>)
    begin
      Self.AsynDelete(MemoryStoreId, MemoryId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitDelete(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryDeleteParamProc;
  const Callbacks: TFunc<TPromiseMemoryDeleted>): TPromise<TMemoryDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryDeleted>(
    procedure(const CallbackParams: TFunc<TAsynMemoryDeleted>)
    begin
      Self.AsynDelete(MemoryStoreId, MemoryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitList(const MemoryStoreId: string;
  const Callbacks: TFunc<TPromiseMemoryList>): TPromise<TMemoryList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryList>)
    begin
      Self.AsynList(MemoryStoreId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitList(const MemoryStoreId: string;
  const ParamProc: TMemoryListParamProc;
  const Callbacks: TFunc<TPromiseMemoryList>): TPromise<TMemoryList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryList>)
    begin
      Self.AsynList(MemoryStoreId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitRetrieve(const MemoryStoreId,
  MemoryId: string; const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynRetrieve(MemoryStoreId, MemoryId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitRetrieve(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryViewParamProc;
  const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynRetrieve(MemoryStoreId, MemoryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitUpdate(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryUpdateParamProc;
  const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynUpdate(MemoryStoreId, MemoryId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoriesRoute.AsyncAwaitUpdate(const MemoryStoreId, MemoryId: string;
  const ParamProc: TMemoryUpdateParamProc;
  const QueryParamProc: TMemoryViewParamProc;
  const Callbacks: TFunc<TPromiseMemory>): TPromise<TMemory>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemory>(
    procedure(const CallbackParams: TFunc<TAsynMemory>)
    begin
      Self.AsynUpdate(MemoryStoreId, MemoryId, ParamProc, QueryParamProc, CallbackParams);
    end,
    Callbacks);
end;

{ TMemoriesAsynchronousSupport }

procedure TMemoriesAsynchronousSupport.AsynCreate(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc; const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Create(MemoryStoreId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynCreate(const MemoryStoreId: string;
  const ParamProc: TMemoryCreateParamProc;
  const QueryParamProc: TMemoryViewParamProc;
  const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Create(MemoryStoreId, ParamProc, QueryParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynDelete(const MemoryStoreId,
  MemoryId: string; const CallBacks: TFunc<TAsynMemoryDeleted>);
begin
  with TAsynCallBackExec<TAsynMemoryDeleted, TMemoryDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryDeleted
      begin
        Result := Self.Delete(MemoryStoreId, MemoryId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynDelete(const MemoryStoreId,
  MemoryId: string; const ParamProc: TMemoryDeleteParamProc;
  const CallBacks: TFunc<TAsynMemoryDeleted>);
begin
  with TAsynCallBackExec<TAsynMemoryDeleted, TMemoryDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryDeleted
      begin
        Result := Self.Delete(MemoryStoreId, MemoryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynList(const MemoryStoreId: string;
  const CallBacks: TFunc<TAsynMemoryList>);
begin
  with TAsynCallBackExec<TAsynMemoryList, TMemoryList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryList
      begin
        Result := Self.List(MemoryStoreId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynList(const MemoryStoreId: string;
  const ParamProc: TMemoryListParamProc;
  const CallBacks: TFunc<TAsynMemoryList>);
begin
  with TAsynCallBackExec<TAsynMemoryList, TMemoryList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryList
      begin
        Result := Self.List(MemoryStoreId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynRetrieve(const MemoryStoreId,
  MemoryId: string; const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Retrieve(MemoryStoreId, MemoryId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynRetrieve(const MemoryStoreId,
  MemoryId: string; const ParamProc: TMemoryViewParamProc;
  const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Retrieve(MemoryStoreId, MemoryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynUpdate(const MemoryStoreId,
  MemoryId: string; const ParamProc: TMemoryUpdateParamProc;
  const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Update(MemoryStoreId, MemoryId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoriesAsynchronousSupport.AsynUpdate(const MemoryStoreId,
  MemoryId: string; const ParamProc: TMemoryUpdateParamProc;
  const QueryParamProc: TMemoryViewParamProc;
  const CallBacks: TFunc<TAsynMemory>);
begin
  with TAsynCallBackExec<TAsynMemory, TMemory>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemory
      begin
        Result := Self.Update(MemoryStoreId, MemoryId, ParamProc, QueryParamProc);
      end);
  finally
    Free;
  end;
end;

{ TMemoryVersionsRoute }

function TMemoryVersionsRoute.List(
  const MemoryStoreId: string): TMemoryVersionList;
begin
  Result := API.Get<TMemoryVersionList>(
    'memory_stores/' + MemoryStoreId + '/memory_versions');
end;

function TMemoryVersionsRoute.List(const MemoryStoreId: string;
  const ParamProc: TMemoryVersionListParamProc): TMemoryVersionList;
begin
  Result := API.Get<TMemoryVersionList, TMemoryVersionListParams>(
    'memory_stores/' + MemoryStoreId + '/memory_versions', ParamProc);
end;

function TMemoryVersionsRoute.Redact(const MemoryStoreId,
  MemoryVersionId: string): TMemoryVersion;
begin
  Result := API.Post<TMemoryVersion>(
    'memory_stores/' + MemoryStoreId + '/memory_versions/' + MemoryVersionId + '/redact');
end;

function TMemoryVersionsRoute.Retrieve(const MemoryStoreId,
  MemoryVersionId: string): TMemoryVersion;
begin
  Result := API.Get<TMemoryVersion>(
    'memory_stores/' + MemoryStoreId + '/memory_versions/' + MemoryVersionId);
end;

function TMemoryVersionsRoute.Retrieve(const MemoryStoreId,
  MemoryVersionId: string;
  const ParamProc: TMemoryVersionRetrieveParamProc): TMemoryVersion;
begin
  Result := API.Get<TMemoryVersion, TMemoryViewParams>(
    'memory_stores/' + MemoryStoreId + '/memory_versions/' + MemoryVersionId, ParamProc);
end;

function TMemoryVersionsRoute.AsyncAwaitList(const MemoryStoreId: string;
  const Callbacks: TFunc<TPromiseMemoryVersionList>): TPromise<TMemoryVersionList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryVersionList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryVersionList>)
    begin
      Self.AsynList(MemoryStoreId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryVersionsRoute.AsyncAwaitList(const MemoryStoreId: string;
  const ParamProc: TMemoryVersionListParamProc;
  const Callbacks: TFunc<TPromiseMemoryVersionList>): TPromise<TMemoryVersionList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryVersionList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryVersionList>)
    begin
      Self.AsynList(MemoryStoreId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryVersionsRoute.AsyncAwaitRedact(const MemoryStoreId,
  MemoryVersionId: string;
  const Callbacks: TFunc<TPromiseMemoryVersion>): TPromise<TMemoryVersion>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryVersion>(
    procedure(const CallbackParams: TFunc<TAsynMemoryVersion>)
    begin
      Self.AsynRedact(MemoryStoreId, MemoryVersionId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryVersionsRoute.AsyncAwaitRetrieve(const MemoryStoreId,
  MemoryVersionId: string;
  const Callbacks: TFunc<TPromiseMemoryVersion>): TPromise<TMemoryVersion>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryVersion>(
    procedure(const CallbackParams: TFunc<TAsynMemoryVersion>)
    begin
      Self.AsynRetrieve(MemoryStoreId, MemoryVersionId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryVersionsRoute.AsyncAwaitRetrieve(const MemoryStoreId,
  MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc;
  const Callbacks: TFunc<TPromiseMemoryVersion>): TPromise<TMemoryVersion>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryVersion>(
    procedure(const CallbackParams: TFunc<TAsynMemoryVersion>)
    begin
      Self.AsynRetrieve(MemoryStoreId, MemoryVersionId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

{ TMemoryVersionsAsynchronousSupport }

procedure TMemoryVersionsAsynchronousSupport.AsynList(const MemoryStoreId: string;
  const CallBacks: TFunc<TAsynMemoryVersionList>);
begin
  with TAsynCallBackExec<TAsynMemoryVersionList, TMemoryVersionList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryVersionList
      begin
        Result := Self.List(MemoryStoreId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryVersionsAsynchronousSupport.AsynList(const MemoryStoreId: string;
  const ParamProc: TMemoryVersionListParamProc;
  const CallBacks: TFunc<TAsynMemoryVersionList>);
begin
  with TAsynCallBackExec<TAsynMemoryVersionList, TMemoryVersionList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryVersionList
      begin
        Result := Self.List(MemoryStoreId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryVersionsAsynchronousSupport.AsynRedact(const MemoryStoreId,
  MemoryVersionId: string; const CallBacks: TFunc<TAsynMemoryVersion>);
begin
  with TAsynCallBackExec<TAsynMemoryVersion, TMemoryVersion>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryVersion
      begin
        Result := Self.Redact(MemoryStoreId, MemoryVersionId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryVersionsAsynchronousSupport.AsynRetrieve(const MemoryStoreId,
  MemoryVersionId: string; const CallBacks: TFunc<TAsynMemoryVersion>);
begin
  with TAsynCallBackExec<TAsynMemoryVersion, TMemoryVersion>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryVersion
      begin
        Result := Self.Retrieve(MemoryStoreId, MemoryVersionId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryVersionsAsynchronousSupport.AsynRetrieve(const MemoryStoreId,
  MemoryVersionId: string; const ParamProc: TMemoryVersionRetrieveParamProc;
  const CallBacks: TFunc<TAsynMemoryVersion>);
begin
  with TAsynCallBackExec<TAsynMemoryVersion, TMemoryVersion>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryVersion
      begin
        Result := Self.Retrieve(MemoryStoreId, MemoryVersionId, ParamProc);
      end);
  finally
    Free;
  end;
end;

{ TMemoryStoresRoute }

function TMemoryStoresRoute.Archive(
  const MemoryStoreId: string): TMemoryStore;
begin
  Result := API.Post<TMemoryStore>(
    'memory_stores/' + MemoryStoreId + '/archive');
end;

function TMemoryStoresRoute.Create(
  const ParamProc: TMemoryStoreCreateParamProc): TMemoryStore;
begin
  Result := API.Post<TMemoryStore, TMemoryStoreCreateParams>(
    'memory_stores', ParamProc, True);
end;

function TMemoryStoresRoute.Delete(
  const MemoryStoreId: string): TMemoryStoreDeleted;
begin
  Result := API.Delete<TMemoryStoreDeleted>(
    'memory_stores/' + MemoryStoreId);
end;

destructor TMemoryStoresRoute.Destroy;
begin
  FMemories.Free;
  FMemoryVersions.Free;
  inherited;
end;

function TMemoryStoresRoute.GetMemoriesRoute: TMemoriesRoute;
begin
  if FMemories = nil then
    FMemories := TMemoriesRoute.CreateRoute(API);
  Result := FMemories;
end;

function TMemoryStoresRoute.GetMemoryVersionsRoute: TMemoryVersionsRoute;
begin
  if FMemoryVersions = nil then
    FMemoryVersions := TMemoryVersionsRoute.CreateRoute(API);
  Result := FMemoryVersions;
end;

function TMemoryStoresRoute.List: TMemoryStoreList;
begin
  Result := API.Get<TMemoryStoreList>('memory_stores');
end;

function TMemoryStoresRoute.List(
  const ParamProc: TMemoryStoreListParamProc): TMemoryStoreList;
begin
  Result := API.Get<TMemoryStoreList, TMemoryStoreListParams>(
    'memory_stores', ParamProc);
end;

function TMemoryStoresRoute.Retrieve(
  const MemoryStoreId: string): TMemoryStore;
begin
  Result := API.Get<TMemoryStore>('memory_stores/' + MemoryStoreId);
end;

function TMemoryStoresRoute.Update(const MemoryStoreId: string;
  const ParamProc: TMemoryStoreUpdateParamProc): TMemoryStore;
begin
  Result := API.Post<TMemoryStore, TMemoryStoreUpdateParams>(
    'memory_stores/' + MemoryStoreId, ParamProc, True);
end;

function TMemoryStoresRoute.AsyncAwaitArchive(const MemoryStoreId: string;
  const Callbacks: TFunc<TPromiseMemoryStore>): TPromise<TMemoryStore>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStore>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStore>)
    begin
      Self.AsynArchive(MemoryStoreId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitCreate(
  const ParamProc: TMemoryStoreCreateParamProc;
  const Callbacks: TFunc<TPromiseMemoryStore>): TPromise<TMemoryStore>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStore>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStore>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitDelete(const MemoryStoreId: string;
  const Callbacks: TFunc<TPromiseMemoryStoreDeleted>): TPromise<TMemoryStoreDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStoreDeleted>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStoreDeleted>)
    begin
      Self.AsynDelete(MemoryStoreId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseMemoryStoreList>): TPromise<TMemoryStoreList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStoreList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStoreList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitList(
  const ParamProc: TMemoryStoreListParamProc;
  const Callbacks: TFunc<TPromiseMemoryStoreList>): TPromise<TMemoryStoreList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStoreList>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStoreList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitRetrieve(const MemoryStoreId: string;
  const Callbacks: TFunc<TPromiseMemoryStore>): TPromise<TMemoryStore>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStore>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStore>)
    begin
      Self.AsynRetrieve(MemoryStoreId, CallbackParams);
    end,
    Callbacks);
end;

function TMemoryStoresRoute.AsyncAwaitUpdate(const MemoryStoreId: string;
  const ParamProc: TMemoryStoreUpdateParamProc;
  const Callbacks: TFunc<TPromiseMemoryStore>): TPromise<TMemoryStore>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TMemoryStore>(
    procedure(const CallbackParams: TFunc<TAsynMemoryStore>)
    begin
      Self.AsynUpdate(MemoryStoreId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

{ TMemoryStoresAsynchronousSupport }

procedure TMemoryStoresAsynchronousSupport.AsynArchive(
  const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryStore>);
begin
  with TAsynCallBackExec<TAsynMemoryStore, TMemoryStore>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStore
      begin
        Result := Self.Archive(MemoryStoreId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynCreate(
  const ParamProc: TMemoryStoreCreateParamProc;
  const CallBacks: TFunc<TAsynMemoryStore>);
begin
  with TAsynCallBackExec<TAsynMemoryStore, TMemoryStore>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStore
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynDelete(
  const MemoryStoreId: string;
  const CallBacks: TFunc<TAsynMemoryStoreDeleted>);
begin
  with TAsynCallBackExec<TAsynMemoryStoreDeleted, TMemoryStoreDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStoreDeleted
      begin
        Result := Self.Delete(MemoryStoreId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynList(
  const CallBacks: TFunc<TAsynMemoryStoreList>);
begin
  with TAsynCallBackExec<TAsynMemoryStoreList, TMemoryStoreList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStoreList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynList(
  const ParamProc: TMemoryStoreListParamProc;
  const CallBacks: TFunc<TAsynMemoryStoreList>);
begin
  with TAsynCallBackExec<TAsynMemoryStoreList, TMemoryStoreList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStoreList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynRetrieve(
  const MemoryStoreId: string; const CallBacks: TFunc<TAsynMemoryStore>);
begin
  with TAsynCallBackExec<TAsynMemoryStore, TMemoryStore>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStore
      begin
        Result := Self.Retrieve(MemoryStoreId);
      end);
  finally
    Free;
  end;
end;

procedure TMemoryStoresAsynchronousSupport.AsynUpdate(const MemoryStoreId: string;
  const ParamProc: TMemoryStoreUpdateParamProc;
  const CallBacks: TFunc<TAsynMemoryStore>);
begin
  with TAsynCallBackExec<TAsynMemoryStore, TMemoryStore>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TMemoryStore
      begin
        Result := Self.Update(MemoryStoreId, ParamProc);
      end);
  finally
    Free;
  end;
end;

end.
