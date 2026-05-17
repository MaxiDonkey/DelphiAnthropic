unit Anthropic.Vaults;

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
  TVaultCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the human-readable vault name.
    /// </summary>
    function DisplayName(const Value: string): TVaultCreateParams;

    /// <summary>
    /// Sets arbitrary vault metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TVaultCreateParams; overload;

    /// <summary>
    /// Adds or replaces a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TVaultCreateParams; overload;

    class function New: TVaultCreateParams;
  end;

  TVaultUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Replaces the human-readable vault name.
    /// </summary>
    function DisplayName(const Value: string): TVaultUpdateParams;

    /// <summary>
    /// Patches vault metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TVaultUpdateParams; overload;

    /// <summary>
    /// Upserts a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TVaultUpdateParams; overload;

    /// <summary>
    /// Deletes a single metadata key by sending JSON null for that key.
    /// </summary>
    function DeleteMetadata(const Key: string): TVaultUpdateParams;

    class function New: TVaultUpdateParams;
  end;

  TVaultListParams = class(TUrlParam)
  public
    /// <summary>
    /// Includes archived vaults in the list response.
    /// </summary>
    function IncludeArchived(const Value: Boolean): TVaultListParams;

    /// <summary>
    /// Sets the maximum number of vaults returned per page.
    /// </summary>
    /// <remarks>
    /// The API default is 20 and the maximum is 100.
    /// </remarks>
    function Limit(const Value: Integer): TVaultListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TVaultListParams;

    class function New: TVaultListParams;
  end;

  TVaultCredentialTokenEndpointAuthParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the token endpoint authentication type.
    /// </summary>
    function &Type(const Value: string): TVaultCredentialTokenEndpointAuthParams;

    class function New(const Value: string): TVaultCredentialTokenEndpointAuthParams;
  end;

  TVaultCredentialTokenEndpointAuthNoneParams = class(TVaultCredentialTokenEndpointAuthParams)
  public
    /// <summary>
    /// Sets token endpoint authentication to no client authentication.
    /// </summary>
    function &Type(const Value: string = 'none'): TVaultCredentialTokenEndpointAuthNoneParams;

    class function New: TVaultCredentialTokenEndpointAuthNoneParams;
  end;

  TVaultCredentialTokenEndpointAuthBasicParams = class(TVaultCredentialTokenEndpointAuthParams)
  public
    /// <summary>
    /// Sets token endpoint authentication to HTTP Basic client authentication.
    /// </summary>
    function &Type(const Value: string = 'client_secret_basic'): TVaultCredentialTokenEndpointAuthBasicParams;

    /// <summary>
    /// Sets the OAuth client secret used for HTTP Basic authentication.
    /// </summary>
    function ClientSecret(const Value: string): TVaultCredentialTokenEndpointAuthBasicParams;

    class function New: TVaultCredentialTokenEndpointAuthBasicParams;
  end;

  TVaultCredentialTokenEndpointAuthPostParams = class(TVaultCredentialTokenEndpointAuthParams)
  public
    /// <summary>
    /// Sets token endpoint authentication to POST body client authentication.
    /// </summary>
    function &Type(const Value: string = 'client_secret_post'): TVaultCredentialTokenEndpointAuthPostParams;

    /// <summary>
    /// Sets the OAuth client secret used in the token request body.
    /// </summary>
    function ClientSecret(const Value: string): TVaultCredentialTokenEndpointAuthPostParams;

    class function New: TVaultCredentialTokenEndpointAuthPostParams;
  end;

  TVaultCredentialMCPOAuthRefreshParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the OAuth client identifier.
    /// </summary>
    function ClientId(const Value: string): TVaultCredentialMCPOAuthRefreshParams;

    /// <summary>
    /// Sets the OAuth refresh token.
    /// </summary>
    function RefreshToken(const Value: string): TVaultCredentialMCPOAuthRefreshParams;

    /// <summary>
    /// Sets the OAuth token endpoint URL.
    /// </summary>
    function TokenEndpoint(const Value: string): TVaultCredentialMCPOAuthRefreshParams;

    /// <summary>
    /// Sets the token endpoint client authentication configuration.
    /// </summary>
    function TokenEndpointAuth(const Value: TVaultCredentialTokenEndpointAuthParams): TVaultCredentialMCPOAuthRefreshParams;

    /// <summary>
    /// Sets the optional OAuth resource indicator.
    /// </summary>
    function Resource(const Value: string): TVaultCredentialMCPOAuthRefreshParams;

    /// <summary>
    /// Sets the optional OAuth scope for refresh requests.
    /// </summary>
    function Scope(const Value: string): TVaultCredentialMCPOAuthRefreshParams;

    class function New: TVaultCredentialMCPOAuthRefreshParams;
  end;

  TVaultCredentialAuthCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the credential authentication type.
    /// </summary>
    function &Type(const Value: string): TVaultCredentialAuthCreateParams;

    class function New(const Value: string): TVaultCredentialAuthCreateParams;
  end;

  TVaultCredentialMCPOAuthCreateParams = class(TVaultCredentialAuthCreateParams)
  public
    /// <summary>
    /// Sets this credential as an MCP OAuth credential.
    /// </summary>
    function &Type(const Value: string = 'mcp_oauth'): TVaultCredentialMCPOAuthCreateParams;

    /// <summary>
    /// Sets the OAuth access token.
    /// </summary>
    function AccessToken(const Value: string): TVaultCredentialMCPOAuthCreateParams;

    /// <summary>
    /// Sets the MCP server URL authenticated by this credential.
    /// </summary>
    function MCPServerUrl(const Value: string): TVaultCredentialMCPOAuthCreateParams;

    /// <summary>
    /// Sets the optional OAuth access-token expiration timestamp in RFC 3339 format.
    /// </summary>
    function ExpiresAt(const Value: string): TVaultCredentialMCPOAuthCreateParams;

    /// <summary>
    /// Sets optional OAuth refresh-token support.
    /// </summary>
    function Refresh(const Value: TVaultCredentialMCPOAuthRefreshParams): TVaultCredentialMCPOAuthCreateParams;

    class function New: TVaultCredentialMCPOAuthCreateParams;
  end;

  TVaultCredentialStaticBearerCreateParams = class(TVaultCredentialAuthCreateParams)
  public
    /// <summary>
    /// Sets this credential as a static bearer-token credential.
    /// </summary>
    function &Type(const Value: string = 'static_bearer'): TVaultCredentialStaticBearerCreateParams;

    /// <summary>
    /// Sets the static bearer token value.
    /// </summary>
    function Token(const Value: string): TVaultCredentialStaticBearerCreateParams;

    /// <summary>
    /// Sets the MCP server URL authenticated by this credential.
    /// </summary>
    function MCPServerUrl(const Value: string): TVaultCredentialStaticBearerCreateParams;

    class function New: TVaultCredentialStaticBearerCreateParams;
  end;

  TVaultCredentialTokenEndpointAuthUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the token endpoint authentication type for an update patch.
    /// </summary>
    function &Type(const Value: string): TVaultCredentialTokenEndpointAuthUpdateParams;

    class function New(const Value: string): TVaultCredentialTokenEndpointAuthUpdateParams;
  end;

  TVaultCredentialTokenEndpointAuthBasicUpdateParams = class(TVaultCredentialTokenEndpointAuthUpdateParams)
  public
    /// <summary>
    /// Selects HTTP Basic client authentication for the token endpoint.
    /// </summary>
    function &Type(const Value: string = 'client_secret_basic'): TVaultCredentialTokenEndpointAuthBasicUpdateParams;

    /// <summary>
    /// Updates the OAuth client secret used for HTTP Basic authentication.
    /// </summary>
    function ClientSecret(const Value: string): TVaultCredentialTokenEndpointAuthBasicUpdateParams;

    class function New: TVaultCredentialTokenEndpointAuthBasicUpdateParams;
  end;

  TVaultCredentialTokenEndpointAuthPostUpdateParams = class(TVaultCredentialTokenEndpointAuthUpdateParams)
  public
    /// <summary>
    /// Selects POST body client authentication for the token endpoint.
    /// </summary>
    function &Type(const Value: string = 'client_secret_post'): TVaultCredentialTokenEndpointAuthPostUpdateParams;

    /// <summary>
    /// Updates the OAuth client secret sent in the token request body.
    /// </summary>
    function ClientSecret(const Value: string): TVaultCredentialTokenEndpointAuthPostUpdateParams;

    class function New: TVaultCredentialTokenEndpointAuthPostUpdateParams;
  end;

  TVaultCredentialMCPOAuthRefreshUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Updates the OAuth refresh token.
    /// </summary>
    function RefreshToken(const Value: string): TVaultCredentialMCPOAuthRefreshUpdateParams;

    /// <summary>
    /// Updates the OAuth scope for refresh requests.
    /// </summary>
    function Scope(const Value: string): TVaultCredentialMCPOAuthRefreshUpdateParams;

    /// <summary>
    /// Updates the token endpoint authentication configuration.
    /// </summary>
    function TokenEndpointAuth(const Value: TVaultCredentialTokenEndpointAuthUpdateParams): TVaultCredentialMCPOAuthRefreshUpdateParams;

    class function New: TVaultCredentialMCPOAuthRefreshUpdateParams;
  end;

  TVaultCredentialAuthUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the credential authentication type for an update patch.
    /// </summary>
    function &Type(const Value: string): TVaultCredentialAuthUpdateParams;

    class function New(const Value: string): TVaultCredentialAuthUpdateParams;
  end;

  TVaultCredentialMCPOAuthUpdateParams = class(TVaultCredentialAuthUpdateParams)
  public
    /// <summary>
    /// Selects the MCP OAuth credential update branch.
    /// </summary>
    function &Type(const Value: string = 'mcp_oauth'): TVaultCredentialMCPOAuthUpdateParams;

    /// <summary>
    /// Updates the OAuth access token.
    /// </summary>
    function AccessToken(const Value: string): TVaultCredentialMCPOAuthUpdateParams;

    /// <summary>
    /// Updates the OAuth access-token expiration timestamp in RFC 3339 format.
    /// </summary>
    function ExpiresAt(const Value: string): TVaultCredentialMCPOAuthUpdateParams;

    /// <summary>
    /// Updates OAuth refresh-token support.
    /// </summary>
    function Refresh(const Value: TVaultCredentialMCPOAuthRefreshUpdateParams): TVaultCredentialMCPOAuthUpdateParams;

    class function New: TVaultCredentialMCPOAuthUpdateParams;
  end;

  TVaultCredentialStaticBearerUpdateParams = class(TVaultCredentialAuthUpdateParams)
  public
    /// <summary>
    /// Selects the static bearer-token credential update branch.
    /// </summary>
    function &Type(const Value: string = 'static_bearer'): TVaultCredentialStaticBearerUpdateParams;

    /// <summary>
    /// Updates the static bearer token value.
    /// </summary>
    function Token(const Value: string): TVaultCredentialStaticBearerUpdateParams;

    class function New: TVaultCredentialStaticBearerUpdateParams;
  end;

  TVaultCredentialCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets authentication details for the credential to create.
    /// </summary>
    function Auth(const Value: TVaultCredentialAuthCreateParams): TVaultCredentialCreateParams;

    /// <summary>
    /// Sets the optional human-readable credential name.
    /// </summary>
    function DisplayName(const Value: string): TVaultCredentialCreateParams;

    /// <summary>
    /// Sets arbitrary credential metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TVaultCredentialCreateParams; overload;

    /// <summary>
    /// Adds or replaces a single credential metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TVaultCredentialCreateParams; overload;

    class function New: TVaultCredentialCreateParams;
  end;

  TVaultCredentialUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Patches authentication details for the credential.
    /// </summary>
    function Auth(const Value: TVaultCredentialAuthUpdateParams): TVaultCredentialUpdateParams;

    /// <summary>
    /// Replaces the human-readable credential name.
    /// </summary>
    function DisplayName(const Value: string): TVaultCredentialUpdateParams;

    /// <summary>
    /// Patches credential metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TVaultCredentialUpdateParams; overload;

    /// <summary>
    /// Upserts a single credential metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TVaultCredentialUpdateParams; overload;

    /// <summary>
    /// Deletes a single credential metadata key by sending JSON null for that key.
    /// </summary>
    function DeleteMetadata(const Key: string): TVaultCredentialUpdateParams;

    class function New: TVaultCredentialUpdateParams;
  end;

  TVaultCredentialListParams = class(TUrlParam)
  public
    /// <summary>
    /// Includes archived credentials in the list response.
    /// </summary>
    function IncludeArchived(const Value: Boolean): TVaultCredentialListParams;

    /// <summary>
    /// Sets the maximum number of credentials returned per page.
    /// </summary>
    /// <remarks>
    /// The API default is 20 and the maximum is 100.
    /// </remarks>
    function Limit(const Value: Integer): TVaultCredentialListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TVaultCredentialListParams;

    class function New: TVaultCredentialListParams;
  end;

  TVaultCreateParamProc = TProc<TVaultCreateParams>;
  TVaultUpdateParamProc = TProc<TVaultUpdateParams>;
  TVaultListParamProc = TProc<TVaultListParams>;
  TVaultCredentialCreateParamProc = TProc<TVaultCredentialCreateParams>;
  TVaultCredentialUpdateParamProc = TProc<TVaultCredentialUpdateParams>;
  TVaultCredentialListParamProc = TProc<TVaultCredentialListParams>;

  TVault = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('display_name')]
    FDisplayName: string;
    [JSONMarshalled(False)]
    FMetadata: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Unique vault identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 timestamp at which the vault was archived, when applicable.
    /// </summary>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// RFC 3339 timestamp at which the vault was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Human-readable vault name.
    /// </summary>
    property DisplayName: string read FDisplayName write FDisplayName;

    /// <summary>
    /// Raw JSON object containing arbitrary vault metadata.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;

    /// <summary>
    /// Object type, normally <c>vault</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which the vault was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
  end;

  TVaultDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Deleted vault identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Deletion confirmation type, normally <c>vault_deleted</c>.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TVaultList = class(TJSONFingerprint)
  private
    FData: TArray<TVault>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Vaults returned by the current page.
    /// </summary>
    property Data: TArray<TVault> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TVaultCredentialTokenEndpointAuth = class(TJSONFingerprint)
  private
    FType: string;
  public
    /// <summary>
    /// Token endpoint authentication type.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Returns true when token endpoint authentication is disabled.
    /// </summary>
    function IsNone: Boolean;

    /// <summary>
    /// Returns true when token endpoint authentication uses HTTP Basic client authentication.
    /// </summary>
    function IsClientSecretBasic: Boolean;

    /// <summary>
    /// Returns true when token endpoint authentication uses POST body client authentication.
    /// </summary>
    function IsClientSecretPost: Boolean;
  end;

  TVaultCredentialTokenEndpointAuthNone = class(TVaultCredentialTokenEndpointAuth);
  TVaultCredentialTokenEndpointAuthBasic = class(TVaultCredentialTokenEndpointAuth);
  TVaultCredentialTokenEndpointAuthPost = class(TVaultCredentialTokenEndpointAuth);

  TVaultCredentialMCPOAuthRefresh = class(TJSONFingerprint)
  private
    [JsonNameAttribute('client_id')]
    FClientId: string;
    [JsonNameAttribute('token_endpoint')]
    FTokenEndpoint: string;
    [JsonNameAttribute('token_endpoint_auth')]
    FTokenEndpointAuth: TVaultCredentialTokenEndpointAuth;
    FResource: string;
    FScope: string;
  public
    /// <summary>
    /// OAuth client identifier.
    /// </summary>
    property ClientId: string read FClientId write FClientId;

    /// <summary>
    /// OAuth token endpoint URL used to refresh the access token.
    /// </summary>
    property TokenEndpoint: string read FTokenEndpoint write FTokenEndpoint;

    /// <summary>
    /// Token endpoint authentication configuration.
    /// </summary>
    property TokenEndpointAuth: TVaultCredentialTokenEndpointAuth read FTokenEndpointAuth write FTokenEndpointAuth;

    /// <summary>
    /// OAuth resource indicator.
    /// </summary>
    property Resource: string read FResource write FResource;

    /// <summary>
    /// OAuth scope for refresh requests.
    /// </summary>
    property Scope: string read FScope write FScope;

    destructor Destroy; override;
  end;

  TVaultCredentialAuth = class(TJSONFingerprint)
  private
    [JsonNameAttribute('mcp_server_url')]
    FMCPServerUrl: string;
    FType: string;
    [JsonNameAttribute('expires_at')]
    FExpiresAt: string;
    FRefresh: TVaultCredentialMCPOAuthRefresh;
  public
    /// <summary>
    /// MCP server URL authenticated by this credential.
    /// </summary>
    property MCPServerUrl: string read FMCPServerUrl write FMCPServerUrl;

    /// <summary>
    /// Credential authentication type.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// OAuth access-token expiration timestamp in RFC 3339 format, when available.
    /// </summary>
    property ExpiresAt: string read FExpiresAt write FExpiresAt;

    /// <summary>
    /// OAuth refresh-token configuration returned for MCP OAuth credentials.
    /// </summary>
    property Refresh: TVaultCredentialMCPOAuthRefresh read FRefresh write FRefresh;

    /// <summary>
    /// Returns true when this is an MCP OAuth credential.
    /// </summary>
    function IsMCPOAuth: Boolean;

    /// <summary>
    /// Returns true when this is a static bearer-token credential.
    /// </summary>
    function IsStaticBearer: Boolean;

    destructor Destroy; override;
  end;

  TVaultCredentialMCPOAuthAuth = class(TVaultCredentialAuth);
  TVaultCredentialStaticBearerAuth = class(TVaultCredentialAuth);

  TVaultCredential = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    FAuth: TVaultCredentialAuth;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('display_name')]
    FDisplayName: string;
    [JSONMarshalled(False)]
    FMetadata: string;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    [JsonNameAttribute('vault_id')]
    FVaultId: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Unique credential identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 timestamp at which the credential was archived, when applicable.
    /// </summary>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// Credential authentication details. Sensitive fields are not returned by the API.
    /// </summary>
    property Auth: TVaultCredentialAuth read FAuth write FAuth;

    /// <summary>
    /// RFC 3339 timestamp at which the credential was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Optional human-readable credential name.
    /// </summary>
    property DisplayName: string read FDisplayName write FDisplayName;

    /// <summary>
    /// Raw JSON object containing arbitrary credential metadata.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;

    /// <summary>
    /// Object type, normally <c>vault_credential</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which the credential was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    /// <summary>
    /// Identifier of the vault that contains this credential.
    /// </summary>
    property VaultId: string read FVaultId write FVaultId;

    destructor Destroy; override;
  end;

  TVaultCredentialDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Deleted credential identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Deletion confirmation type, normally <c>vault_credential_deleted</c>.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TVaultCredentialList = class(TJSONFingerprint)
  private
    FData: TArray<TVaultCredential>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Credentials returned by the current page.
    /// </summary>
    property Data: TArray<TVaultCredential> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TVaultCredentialRefreshHTTPResponse = class(TJSONFingerprint)
  private
    FBody: string;
    [JsonNameAttribute('body_truncated')]
    FBodyTruncated: Boolean;
    [JsonNameAttribute('content_type')]
    FContentType: string;
    [JsonNameAttribute('status_code')]
    FStatusCode: Integer;
  public
    /// <summary>
    /// Scrubbed response body captured during credential validation.
    /// </summary>
    property Body: string read FBody write FBody;

    /// <summary>
    /// Whether the response body was truncated.
    /// </summary>
    property BodyTruncated: Boolean read FBodyTruncated write FBodyTruncated;

    /// <summary>
    /// Response content type.
    /// </summary>
    property ContentType: string read FContentType write FContentType;

    /// <summary>
    /// HTTP status code.
    /// </summary>
    property StatusCode: Integer read FStatusCode write FStatusCode;
  end;

  TVaultCredentialMCPProbe = class(TJSONFingerprint)
  private
    [JsonNameAttribute('http_response')]
    FHTTPResponse: TVaultCredentialRefreshHTTPResponse;
    FMethod: string;
  public
    /// <summary>
    /// HTTP response captured during the MCP validation probe.
    /// </summary>
    property HTTPResponse: TVaultCredentialRefreshHTTPResponse read FHTTPResponse write FHTTPResponse;

    /// <summary>
    /// MCP method associated with the failing probe step.
    /// </summary>
    property Method: string read FMethod write FMethod;

    destructor Destroy; override;
  end;

  TVaultCredentialRefreshObject = class(TJSONFingerprint)
  private
    [JsonNameAttribute('http_response')]
    FHTTPResponse: TVaultCredentialRefreshHTTPResponse;
    FStatus: string;
  public
    /// <summary>
    /// HTTP response captured during the refresh-token exchange, when available.
    /// </summary>
    property HTTPResponse: TVaultCredentialRefreshHTTPResponse read FHTTPResponse write FHTTPResponse;

    /// <summary>
    /// Refresh-token exchange status.
    /// </summary>
    property Status: string read FStatus write FStatus;

    destructor Destroy; override;
  end;

  TVaultCredentialValidation = class(TJSONFingerprint)
  private
    [JsonNameAttribute('credential_id')]
    FCredentialId: string;
    [JsonNameAttribute('has_refresh_token')]
    FHasRefreshToken: Boolean;
    [JsonNameAttribute('mcp_probe')]
    FMCPProbe: TVaultCredentialMCPProbe;
    FRefresh: TVaultCredentialRefreshObject;
    FStatus: string;
    FType: string;
    [JsonNameAttribute('validated_at')]
    FValidatedAt: string;
    [JsonNameAttribute('vault_id')]
    FVaultId: string;
  public
    /// <summary>
    /// Identifier of the credential that was validated.
    /// </summary>
    property CredentialId: string read FCredentialId write FCredentialId;

    /// <summary>
    /// Whether the credential has a refresh token configured.
    /// </summary>
    property HasRefreshToken: Boolean read FHasRefreshToken write FHasRefreshToken;

    /// <summary>
    /// MCP server probe details.
    /// </summary>
    property MCPProbe: TVaultCredentialMCPProbe read FMCPProbe write FMCPProbe;

    /// <summary>
    /// Refresh-token exchange outcome.
    /// </summary>
    property Refresh: TVaultCredentialRefreshObject read FRefresh write FRefresh;

    /// <summary>
    /// Overall validation status: <c>valid</c>, <c>invalid</c>, or <c>unknown</c>.
    /// </summary>
    property Status: string read FStatus write FStatus;

    /// <summary>
    /// Object type, normally <c>vault_credential_validation</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which validation was performed.
    /// </summary>
    property ValidatedAt: string read FValidatedAt write FValidatedAt;

    /// <summary>
    /// Identifier of the vault that contains the credential.
    /// </summary>
    property VaultId: string read FVaultId write FVaultId;

    destructor Destroy; override;
  end;

  TAsynVault = TAsynCallBack<TVault>;
  TPromiseVault = TPromiseCallback<TVault>;
  TAsynVaultList = TAsynCallBack<TVaultList>;
  TPromiseVaultList = TPromiseCallback<TVaultList>;
  TAsynVaultDeleted = TAsynCallBack<TVaultDeleted>;
  TPromiseVaultDeleted = TPromiseCallback<TVaultDeleted>;

  TAsynVaultCredential = TAsynCallBack<TVaultCredential>;
  TPromiseVaultCredential = TPromiseCallback<TVaultCredential>;
  TAsynVaultCredentialList = TAsynCallBack<TVaultCredentialList>;
  TPromiseVaultCredentialList = TPromiseCallback<TVaultCredentialList>;
  TAsynVaultCredentialDeleted = TAsynCallBack<TVaultCredentialDeleted>;
  TPromiseVaultCredentialDeleted = TPromiseCallback<TVaultCredentialDeleted>;
  TAsynVaultCredentialValidation = TAsynCallBack<TVaultCredentialValidation>;
  TPromiseVaultCredentialValidation = TPromiseCallback<TVaultCredentialValidation>;

  TVaultCredentialsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const VaultId: string;
      const ParamProc: TVaultCredentialCreateParamProc): TVaultCredential; overload; virtual; abstract;
    function List(const VaultId: string): TVaultCredentialList; overload; virtual; abstract;
    function List(const VaultId: string;
      const ParamProc: TVaultCredentialListParamProc): TVaultCredentialList; overload; virtual; abstract;
    function Retrieve(const VaultId, CredentialId: string): TVaultCredential; overload; virtual; abstract;
    function Update(const VaultId, CredentialId: string;
      const ParamProc: TVaultCredentialUpdateParamProc): TVaultCredential; overload; virtual; abstract;
    function Delete(const VaultId, CredentialId: string): TVaultCredentialDeleted; overload; virtual; abstract;
    function Archive(const VaultId, CredentialId: string): TVaultCredential; overload; virtual; abstract;
    function Validate(const VaultId, CredentialId: string): TVaultCredentialValidation; overload; virtual; abstract;
  end;

  TVaultCredentialsAsynchronousSupport = class(TVaultCredentialsAbstractSupport)
  protected
    procedure AsynCreate(const VaultId: string; const ParamProc: TVaultCredentialCreateParamProc; const CallBacks: TFunc<TAsynVaultCredential>); overload;
    procedure AsynList(const VaultId: string; const CallBacks: TFunc<TAsynVaultCredentialList>); overload;
    procedure AsynList(const VaultId: string; const ParamProc: TVaultCredentialListParamProc; const CallBacks: TFunc<TAsynVaultCredentialList>); overload;
    procedure AsynRetrieve(const VaultId, CredentialId: string; const CallBacks: TFunc<TAsynVaultCredential>); overload;
    procedure AsynUpdate(const VaultId, CredentialId: string; const ParamProc: TVaultCredentialUpdateParamProc; const CallBacks: TFunc<TAsynVaultCredential>); overload;
    procedure AsynDelete(const VaultId, CredentialId: string; const CallBacks: TFunc<TAsynVaultCredentialDeleted>); overload;
    procedure AsynArchive(const VaultId, CredentialId: string; const CallBacks: TFunc<TAsynVaultCredential>); overload;
    procedure AsynValidate(const VaultId, CredentialId: string; const CallBacks: TFunc<TAsynVaultCredentialValidation>); overload;
  end;

  TVaultCredentialsRoute = class(TVaultCredentialsAsynchronousSupport)
  public
    /// <summary>
    /// Creates a credential in the specified vault.
    /// </summary>
    function Create(const VaultId: string;
      const ParamProc: TVaultCredentialCreateParamProc): TVaultCredential; overload; override;

    /// <summary>
    /// Lists credentials in the specified vault.
    /// </summary>
    function List(const VaultId: string): TVaultCredentialList; overload; override;

    /// <summary>
    /// Lists credentials in the specified vault using query parameters.
    /// </summary>
    function List(const VaultId: string;
      const ParamProc: TVaultCredentialListParamProc): TVaultCredentialList; overload; override;

    /// <summary>
    /// Retrieves a credential from the specified vault.
    /// </summary>
    function Retrieve(const VaultId, CredentialId: string): TVaultCredential; overload; override;

    /// <summary>
    /// Updates a credential in the specified vault.
    /// </summary>
    function Update(const VaultId, CredentialId: string;
      const ParamProc: TVaultCredentialUpdateParamProc): TVaultCredential; overload; override;

    /// <summary>
    /// Deletes a credential from the specified vault.
    /// </summary>
    function Delete(const VaultId, CredentialId: string): TVaultCredentialDeleted; overload; override;

    /// <summary>
    /// Archives a credential in the specified vault.
    /// </summary>
    function Archive(const VaultId, CredentialId: string): TVaultCredential; overload; override;

    /// <summary>
    /// Validates an MCP OAuth credential against its configured MCP server.
    /// </summary>
    function Validate(const VaultId, CredentialId: string): TVaultCredentialValidation; overload; override;

    /// <summary>
    /// Asynchronously creates a credential in the specified vault.
    /// </summary>
    function AsyncAwaitCreate(const VaultId: string; const ParamProc: TVaultCredentialCreateParamProc;
      const Callbacks: TFunc<TPromiseVaultCredential> = nil): TPromise<TVaultCredential>; overload;

    /// <summary>
    /// Asynchronously lists credentials in the specified vault.
    /// </summary>
    function AsyncAwaitList(const VaultId: string;
      const Callbacks: TFunc<TPromiseVaultCredentialList> = nil): TPromise<TVaultCredentialList>; overload;

    /// <summary>
    /// Asynchronously lists credentials in the specified vault using query parameters.
    /// </summary>
    function AsyncAwaitList(const VaultId: string; const ParamProc: TVaultCredentialListParamProc;
      const Callbacks: TFunc<TPromiseVaultCredentialList> = nil): TPromise<TVaultCredentialList>; overload;

    /// <summary>
    /// Asynchronously retrieves a credential from the specified vault.
    /// </summary>
    function AsyncAwaitRetrieve(const VaultId, CredentialId: string;
      const Callbacks: TFunc<TPromiseVaultCredential> = nil): TPromise<TVaultCredential>; overload;

    /// <summary>
    /// Asynchronously updates a credential in the specified vault.
    /// </summary>
    function AsyncAwaitUpdate(const VaultId, CredentialId: string; const ParamProc: TVaultCredentialUpdateParamProc;
      const Callbacks: TFunc<TPromiseVaultCredential> = nil): TPromise<TVaultCredential>; overload;

    /// <summary>
    /// Asynchronously deletes a credential from the specified vault.
    /// </summary>
    function AsyncAwaitDelete(const VaultId, CredentialId: string;
      const Callbacks: TFunc<TPromiseVaultCredentialDeleted> = nil): TPromise<TVaultCredentialDeleted>; overload;

    /// <summary>
    /// Asynchronously archives a credential in the specified vault.
    /// </summary>
    function AsyncAwaitArchive(const VaultId, CredentialId: string;
      const Callbacks: TFunc<TPromiseVaultCredential> = nil): TPromise<TVaultCredential>; overload;

    /// <summary>
    /// Asynchronously validates an MCP OAuth credential against its configured MCP server.
    /// </summary>
    function AsyncAwaitValidate(const VaultId, CredentialId: string;
      const Callbacks: TFunc<TPromiseVaultCredentialValidation> = nil): TPromise<TVaultCredentialValidation>; overload;
  end;

  TVaultsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Create(const ParamProc: TVaultCreateParamProc): TVault; overload; virtual; abstract;
    function List: TVaultList; overload; virtual; abstract;
    function List(const ParamProc: TVaultListParamProc): TVaultList; overload; virtual; abstract;
    function Retrieve(const VaultId: string): TVault; overload; virtual; abstract;
    function Update(const VaultId: string; const ParamProc: TVaultUpdateParamProc): TVault; overload; virtual; abstract;
    function Delete(const VaultId: string): TVaultDeleted; overload; virtual; abstract;
    function Archive(const VaultId: string): TVault; overload; virtual; abstract;
  end;

  TVaultsAsynchronousSupport = class(TVaultsAbstractSupport)
  protected
    procedure AsynCreate(const ParamProc: TVaultCreateParamProc; const CallBacks: TFunc<TAsynVault>); overload;
    procedure AsynList(const CallBacks: TFunc<TAsynVaultList>); overload;
    procedure AsynList(const ParamProc: TVaultListParamProc; const CallBacks: TFunc<TAsynVaultList>); overload;
    procedure AsynRetrieve(const VaultId: string; const CallBacks: TFunc<TAsynVault>); overload;
    procedure AsynUpdate(const VaultId: string; const ParamProc: TVaultUpdateParamProc; const CallBacks: TFunc<TAsynVault>); overload;
    procedure AsynDelete(const VaultId: string; const CallBacks: TFunc<TAsynVaultDeleted>); overload;
    procedure AsynArchive(const VaultId: string; const CallBacks: TFunc<TAsynVault>); overload;
  end;

  TVaultsRoute = class(TVaultsAsynchronousSupport)
  private
    FCredentials: TVaultCredentialsRoute;
    function GetCredentialsRoute: TVaultCredentialsRoute;
  public
    /// <summary>
    /// Provides access to vault credential operations.
    /// </summary>
    /// <remarks>
    /// The route instance is created lazily and shares the same underlying API instance.
    /// </remarks>
    property Credentials: TVaultCredentialsRoute read GetCredentialsRoute;

    /// <summary>
    /// Creates a vault with the supplied request parameters.
    /// </summary>
    function Create(const ParamProc: TVaultCreateParamProc): TVault; overload; override;

    /// <summary>
    /// Lists vaults.
    /// </summary>
    function List: TVaultList; overload; override;

    /// <summary>
    /// Lists vaults using query parameters.
    /// </summary>
    function List(const ParamProc: TVaultListParamProc): TVaultList; overload; override;

    /// <summary>
    /// Retrieves a vault.
    /// </summary>
    function Retrieve(const VaultId: string): TVault; overload; override;

    /// <summary>
    /// Updates a vault and returns the updated vault.
    /// </summary>
    function Update(const VaultId: string; const ParamProc: TVaultUpdateParamProc): TVault; overload; override;

    /// <summary>
    /// Deletes a vault and returns the deletion confirmation.
    /// </summary>
    function Delete(const VaultId: string): TVaultDeleted; overload; override;

    /// <summary>
    /// Archives a vault and returns the archived vault representation.
    /// </summary>
    function Archive(const VaultId: string): TVault; overload; override;

    /// <summary>
    /// Asynchronously creates a vault with the supplied request parameters.
    /// </summary>
    function AsyncAwaitCreate(const ParamProc: TVaultCreateParamProc;
      const Callbacks: TFunc<TPromiseVault> = nil): TPromise<TVault>; overload;

    /// <summary>
    /// Asynchronously lists vaults.
    /// </summary>
    function AsyncAwaitList(const Callbacks: TFunc<TPromiseVaultList> = nil): TPromise<TVaultList>; overload;

    /// <summary>
    /// Asynchronously lists vaults using query parameters.
    /// </summary>
    function AsyncAwaitList(const ParamProc: TVaultListParamProc;
      const Callbacks: TFunc<TPromiseVaultList> = nil): TPromise<TVaultList>; overload;

    /// <summary>
    /// Asynchronously retrieves a vault.
    /// </summary>
    function AsyncAwaitRetrieve(const VaultId: string;
      const Callbacks: TFunc<TPromiseVault> = nil): TPromise<TVault>; overload;

    /// <summary>
    /// Asynchronously updates a vault and returns the updated vault.
    /// </summary>
    function AsyncAwaitUpdate(const VaultId: string; const ParamProc: TVaultUpdateParamProc;
      const Callbacks: TFunc<TPromiseVault> = nil): TPromise<TVault>; overload;

    /// <summary>
    /// Asynchronously deletes a vault and returns the deletion confirmation.
    /// </summary>
    function AsyncAwaitDelete(const VaultId: string;
      const Callbacks: TFunc<TPromiseVaultDeleted> = nil): TPromise<TVaultDeleted>; overload;

    /// <summary>
    /// Asynchronously archives a vault and returns the archived vault representation.
    /// </summary>
    function AsyncAwaitArchive(const VaultId: string;
      const Callbacks: TFunc<TPromiseVault> = nil): TPromise<TVault>; overload;

    destructor Destroy; override;
  end;

implementation

uses
  Anthropic.API.JsonSafeReader;

type
  TVaultResponseHydrator = class
  public
    class procedure HydrateVault(const Vault: TVault); static;
    class procedure HydrateVaultList(const List: TVaultList); static;
    class procedure HydrateCredential(const Credential: TVaultCredential); static;
    class procedure HydrateCredentialList(const List: TVaultCredentialList); static;
  end;

{ TVaultResponseHydrator }

class procedure TVaultResponseHydrator.HydrateVault(const Vault: TVault);
begin
  if (Vault = nil) or Vault.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Vault.JSONResponse);
  if not Root.IsValid then
    Exit;

  Vault.FMetadata := Root.ExtractSubJson('metadata', Vault.FMetadata);
end;

class procedure TVaultResponseHydrator.HydrateVaultList(const List: TVaultList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    begin
      if List.FData[I] = nil then
        Continue;

      var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
      if JsonText.IsEmpty then
        Continue;

      List.FData[I].JSONResponse := JsonText;
      List.FData[I].InternalFinalizeDeserialize;
    end;
end;

class procedure TVaultResponseHydrator.HydrateCredential(const Credential: TVaultCredential);
begin
  if (Credential = nil) or Credential.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Credential.JSONResponse);
  if not Root.IsValid then
    Exit;

  Credential.FMetadata := Root.ExtractSubJson('metadata', Credential.FMetadata);
end;

class procedure TVaultResponseHydrator.HydrateCredentialList(const List: TVaultCredentialList);
begin
  if (List = nil) or List.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(List.JSONResponse);
  if not Root.IsValid then
    Exit;

  for var I := 0 to High(List.FData) do
    begin
      if List.FData[I] = nil then
        Continue;

      var JsonText := Root.ExtractSubJson(Format('data[%d]', [I]));
      if JsonText.IsEmpty then
        Continue;

      List.FData[I].JSONResponse := JsonText;
      List.FData[I].InternalFinalizeDeserialize;
    end;
end;

{ TVaultCreateParams }

class function TVaultCreateParams.New: TVaultCreateParams;
begin
  Result := TVaultCreateParams.Create;
end;

function TVaultCreateParams.DisplayName(const Value: string): TVaultCreateParams;
begin
  Result := TVaultCreateParams(Add('display_name', Value));
end;

function TVaultCreateParams.Metadata(const Value: TJSONObject): TVaultCreateParams;
begin
  Result := TVaultCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TVaultCreateParams.Metadata(const Key, Value: string): TVaultCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

{ TVaultUpdateParams }

class function TVaultUpdateParams.New: TVaultUpdateParams;
begin
  Result := TVaultUpdateParams.Create;
end;

function TVaultUpdateParams.DisplayName(const Value: string): TVaultUpdateParams;
begin
  Result := TVaultUpdateParams(Add('display_name', Value));
end;

function TVaultUpdateParams.Metadata(const Value: TJSONObject): TVaultUpdateParams;
begin
  Result := TVaultUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TVaultUpdateParams.Metadata(const Key, Value: string): TVaultUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TVaultUpdateParams.DeleteMetadata(const Key: string): TVaultUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

{ TVaultListParams }

class function TVaultListParams.New: TVaultListParams;
begin
  Result := TVaultListParams.Create;
end;

function TVaultListParams.IncludeArchived(const Value: Boolean): TVaultListParams;
begin
  Result := TVaultListParams(Add('include_archived', Value));
end;

function TVaultListParams.Limit(const Value: Integer): TVaultListParams;
begin
  Result := TVaultListParams(Add('limit', Value));
end;

function TVaultListParams.Page(const Value: string): TVaultListParams;
begin
  Result := TVaultListParams(Add('page', Value));
end;

{ TVaultCredentialTokenEndpointAuthParams }

class function TVaultCredentialTokenEndpointAuthParams.New(const Value: string): TVaultCredentialTokenEndpointAuthParams;
begin
  Result := TVaultCredentialTokenEndpointAuthParams.Create.&Type(Value);
end;

function TVaultCredentialTokenEndpointAuthParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthParams;
begin
  Result := TVaultCredentialTokenEndpointAuthParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthNoneParams }

class function TVaultCredentialTokenEndpointAuthNoneParams.New: TVaultCredentialTokenEndpointAuthNoneParams;
begin
  Result := TVaultCredentialTokenEndpointAuthNoneParams.Create.&Type();
end;

function TVaultCredentialTokenEndpointAuthNoneParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthNoneParams;
begin
  Result := TVaultCredentialTokenEndpointAuthNoneParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthBasicParams }

class function TVaultCredentialTokenEndpointAuthBasicParams.New: TVaultCredentialTokenEndpointAuthBasicParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicParams.Create.&Type();
end;

function TVaultCredentialTokenEndpointAuthBasicParams.ClientSecret(
  const Value: string): TVaultCredentialTokenEndpointAuthBasicParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicParams(Add('client_secret', Value));
end;

function TVaultCredentialTokenEndpointAuthBasicParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthBasicParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthPostParams }

class function TVaultCredentialTokenEndpointAuthPostParams.New: TVaultCredentialTokenEndpointAuthPostParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostParams.Create.&Type();
end;

function TVaultCredentialTokenEndpointAuthPostParams.ClientSecret(
  const Value: string): TVaultCredentialTokenEndpointAuthPostParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostParams(Add('client_secret', Value));
end;

function TVaultCredentialTokenEndpointAuthPostParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthPostParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostParams(Add('type', Value));
end;

{ TVaultCredentialMCPOAuthRefreshParams }

class function TVaultCredentialMCPOAuthRefreshParams.New: TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams.Create;
end;

function TVaultCredentialMCPOAuthRefreshParams.ClientId(
  const Value: string): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('client_id', Value));
end;

function TVaultCredentialMCPOAuthRefreshParams.RefreshToken(
  const Value: string): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('refresh_token', Value));
end;

function TVaultCredentialMCPOAuthRefreshParams.Resource(
  const Value: string): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('resource', Value));
end;

function TVaultCredentialMCPOAuthRefreshParams.Scope(
  const Value: string): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('scope', Value));
end;

function TVaultCredentialMCPOAuthRefreshParams.TokenEndpoint(
  const Value: string): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('token_endpoint', Value));
end;

function TVaultCredentialMCPOAuthRefreshParams.TokenEndpointAuth(
  const Value: TVaultCredentialTokenEndpointAuthParams): TVaultCredentialMCPOAuthRefreshParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshParams(Add('token_endpoint_auth', Value.Detach));
end;

{ TVaultCredentialAuthCreateParams }

class function TVaultCredentialAuthCreateParams.New(const Value: string): TVaultCredentialAuthCreateParams;
begin
  Result := TVaultCredentialAuthCreateParams.Create.&Type(Value);
end;

function TVaultCredentialAuthCreateParams.&Type(
  const Value: string): TVaultCredentialAuthCreateParams;
begin
  Result := TVaultCredentialAuthCreateParams(Add('type', Value));
end;

{ TVaultCredentialMCPOAuthCreateParams }

class function TVaultCredentialMCPOAuthCreateParams.New: TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams.Create.&Type();
end;

function TVaultCredentialMCPOAuthCreateParams.AccessToken(
  const Value: string): TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams(Add('access_token', Value));
end;

function TVaultCredentialMCPOAuthCreateParams.ExpiresAt(
  const Value: string): TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams(Add('expires_at', Value));
end;

function TVaultCredentialMCPOAuthCreateParams.MCPServerUrl(
  const Value: string): TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams(Add('mcp_server_url', Value));
end;

function TVaultCredentialMCPOAuthCreateParams.Refresh(
  const Value: TVaultCredentialMCPOAuthRefreshParams): TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams(Add('refresh', Value.Detach));
end;

function TVaultCredentialMCPOAuthCreateParams.&Type(
  const Value: string): TVaultCredentialMCPOAuthCreateParams;
begin
  Result := TVaultCredentialMCPOAuthCreateParams(Add('type', Value));
end;

{ TVaultCredentialStaticBearerCreateParams }

class function TVaultCredentialStaticBearerCreateParams.New: TVaultCredentialStaticBearerCreateParams;
begin
  Result := TVaultCredentialStaticBearerCreateParams.Create.&Type();
end;

function TVaultCredentialStaticBearerCreateParams.MCPServerUrl(
  const Value: string): TVaultCredentialStaticBearerCreateParams;
begin
  Result := TVaultCredentialStaticBearerCreateParams(Add('mcp_server_url', Value));
end;

function TVaultCredentialStaticBearerCreateParams.Token(
  const Value: string): TVaultCredentialStaticBearerCreateParams;
begin
  Result := TVaultCredentialStaticBearerCreateParams(Add('token', Value));
end;

function TVaultCredentialStaticBearerCreateParams.&Type(
  const Value: string): TVaultCredentialStaticBearerCreateParams;
begin
  Result := TVaultCredentialStaticBearerCreateParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthUpdateParams }

class function TVaultCredentialTokenEndpointAuthUpdateParams.New(const Value: string): TVaultCredentialTokenEndpointAuthUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthUpdateParams.Create.&Type(Value);
end;

function TVaultCredentialTokenEndpointAuthUpdateParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthUpdateParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthBasicUpdateParams }

class function TVaultCredentialTokenEndpointAuthBasicUpdateParams.New: TVaultCredentialTokenEndpointAuthBasicUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicUpdateParams.Create.&Type();
end;

function TVaultCredentialTokenEndpointAuthBasicUpdateParams.ClientSecret(
  const Value: string): TVaultCredentialTokenEndpointAuthBasicUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicUpdateParams(Add('client_secret', Value));
end;

function TVaultCredentialTokenEndpointAuthBasicUpdateParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthBasicUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthBasicUpdateParams(Add('type', Value));
end;

{ TVaultCredentialTokenEndpointAuthPostUpdateParams }

class function TVaultCredentialTokenEndpointAuthPostUpdateParams.New: TVaultCredentialTokenEndpointAuthPostUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostUpdateParams.Create.&Type();
end;

function TVaultCredentialTokenEndpointAuthPostUpdateParams.ClientSecret(
  const Value: string): TVaultCredentialTokenEndpointAuthPostUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostUpdateParams(Add('client_secret', Value));
end;

function TVaultCredentialTokenEndpointAuthPostUpdateParams.&Type(
  const Value: string): TVaultCredentialTokenEndpointAuthPostUpdateParams;
begin
  Result := TVaultCredentialTokenEndpointAuthPostUpdateParams(Add('type', Value));
end;

{ TVaultCredentialMCPOAuthRefreshUpdateParams }

class function TVaultCredentialMCPOAuthRefreshUpdateParams.New: TVaultCredentialMCPOAuthRefreshUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshUpdateParams.Create;
end;

function TVaultCredentialMCPOAuthRefreshUpdateParams.RefreshToken(
  const Value: string): TVaultCredentialMCPOAuthRefreshUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshUpdateParams(Add('refresh_token', Value));
end;

function TVaultCredentialMCPOAuthRefreshUpdateParams.Scope(
  const Value: string): TVaultCredentialMCPOAuthRefreshUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshUpdateParams(Add('scope', Value));
end;

function TVaultCredentialMCPOAuthRefreshUpdateParams.TokenEndpointAuth(
  const Value: TVaultCredentialTokenEndpointAuthUpdateParams): TVaultCredentialMCPOAuthRefreshUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthRefreshUpdateParams(Add('token_endpoint_auth', Value.Detach));
end;

{ TVaultCredentialAuthUpdateParams }

class function TVaultCredentialAuthUpdateParams.New(const Value: string): TVaultCredentialAuthUpdateParams;
begin
  Result := TVaultCredentialAuthUpdateParams.Create.&Type(Value);
end;

function TVaultCredentialAuthUpdateParams.&Type(
  const Value: string): TVaultCredentialAuthUpdateParams;
begin
  Result := TVaultCredentialAuthUpdateParams(Add('type', Value));
end;

{ TVaultCredentialMCPOAuthUpdateParams }

class function TVaultCredentialMCPOAuthUpdateParams.New: TVaultCredentialMCPOAuthUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthUpdateParams.Create.&Type();
end;

function TVaultCredentialMCPOAuthUpdateParams.AccessToken(
  const Value: string): TVaultCredentialMCPOAuthUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthUpdateParams(Add('access_token', Value));
end;

function TVaultCredentialMCPOAuthUpdateParams.ExpiresAt(
  const Value: string): TVaultCredentialMCPOAuthUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthUpdateParams(Add('expires_at', Value));
end;

function TVaultCredentialMCPOAuthUpdateParams.Refresh(
  const Value: TVaultCredentialMCPOAuthRefreshUpdateParams): TVaultCredentialMCPOAuthUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthUpdateParams(Add('refresh', Value.Detach));
end;

function TVaultCredentialMCPOAuthUpdateParams.&Type(
  const Value: string): TVaultCredentialMCPOAuthUpdateParams;
begin
  Result := TVaultCredentialMCPOAuthUpdateParams(Add('type', Value));
end;

{ TVaultCredentialStaticBearerUpdateParams }

class function TVaultCredentialStaticBearerUpdateParams.New: TVaultCredentialStaticBearerUpdateParams;
begin
  Result := TVaultCredentialStaticBearerUpdateParams.Create.&Type();
end;

function TVaultCredentialStaticBearerUpdateParams.Token(
  const Value: string): TVaultCredentialStaticBearerUpdateParams;
begin
  Result := TVaultCredentialStaticBearerUpdateParams(Add('token', Value));
end;

function TVaultCredentialStaticBearerUpdateParams.&Type(
  const Value: string): TVaultCredentialStaticBearerUpdateParams;
begin
  Result := TVaultCredentialStaticBearerUpdateParams(Add('type', Value));
end;

{ TVaultCredentialCreateParams }

class function TVaultCredentialCreateParams.New: TVaultCredentialCreateParams;
begin
  Result := TVaultCredentialCreateParams.Create;
end;

function TVaultCredentialCreateParams.Auth(
  const Value: TVaultCredentialAuthCreateParams): TVaultCredentialCreateParams;
begin
  Result := TVaultCredentialCreateParams(Add('auth', Value.Detach));
end;

function TVaultCredentialCreateParams.DisplayName(
  const Value: string): TVaultCredentialCreateParams;
begin
  Result := TVaultCredentialCreateParams(Add('display_name', Value));
end;

function TVaultCredentialCreateParams.Metadata(
  const Value: TJSONObject): TVaultCredentialCreateParams;
begin
  Result := TVaultCredentialCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TVaultCredentialCreateParams.Metadata(
  const Key, Value: string): TVaultCredentialCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

{ TVaultCredentialUpdateParams }

class function TVaultCredentialUpdateParams.New: TVaultCredentialUpdateParams;
begin
  Result := TVaultCredentialUpdateParams.Create;
end;

function TVaultCredentialUpdateParams.Auth(
  const Value: TVaultCredentialAuthUpdateParams): TVaultCredentialUpdateParams;
begin
  Result := TVaultCredentialUpdateParams(Add('auth', Value.Detach));
end;

function TVaultCredentialUpdateParams.DeleteMetadata(
  const Key: string): TVaultCredentialUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

function TVaultCredentialUpdateParams.DisplayName(
  const Value: string): TVaultCredentialUpdateParams;
begin
  Result := TVaultCredentialUpdateParams(Add('display_name', Value));
end;

function TVaultCredentialUpdateParams.Metadata(
  const Value: TJSONObject): TVaultCredentialUpdateParams;
begin
  Result := TVaultCredentialUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TVaultCredentialUpdateParams.Metadata(
  const Key, Value: string): TVaultCredentialUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

{ TVaultCredentialListParams }

class function TVaultCredentialListParams.New: TVaultCredentialListParams;
begin
  Result := TVaultCredentialListParams.Create;
end;

function TVaultCredentialListParams.IncludeArchived(
  const Value: Boolean): TVaultCredentialListParams;
begin
  Result := TVaultCredentialListParams(Add('include_archived', Value));
end;

function TVaultCredentialListParams.Limit(
  const Value: Integer): TVaultCredentialListParams;
begin
  Result := TVaultCredentialListParams(Add('limit', Value));
end;

function TVaultCredentialListParams.Page(
  const Value: string): TVaultCredentialListParams;
begin
  Result := TVaultCredentialListParams(Add('page', Value));
end;

{ TVault }

procedure TVault.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TVault.ContentUpdate;
begin
  inherited;
  TVaultResponseHydrator.HydrateVault(Self);
end;

{ TVaultList }

procedure TVaultList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TVaultList.ContentUpdate;
begin
  inherited;
  TVaultResponseHydrator.HydrateVaultList(Self);
end;

destructor TVaultList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TVaultCredentialTokenEndpointAuth }

function TVaultCredentialTokenEndpointAuth.IsClientSecretBasic: Boolean;
begin
  Result := SameText(FType, 'client_secret_basic');
end;

function TVaultCredentialTokenEndpointAuth.IsClientSecretPost: Boolean;
begin
  Result := SameText(FType, 'client_secret_post');
end;

function TVaultCredentialTokenEndpointAuth.IsNone: Boolean;
begin
  Result := SameText(FType, 'none');
end;

{ TVaultCredentialMCPOAuthRefresh }

destructor TVaultCredentialMCPOAuthRefresh.Destroy;
begin
  FTokenEndpointAuth.Free;
  inherited;
end;

{ TVaultCredentialAuth }

destructor TVaultCredentialAuth.Destroy;
begin
  FRefresh.Free;
  inherited;
end;

function TVaultCredentialAuth.IsMCPOAuth: Boolean;
begin
  Result := SameText(FType, 'mcp_oauth');
end;

function TVaultCredentialAuth.IsStaticBearer: Boolean;
begin
  Result := SameText(FType, 'static_bearer');
end;

{ TVaultCredential }

procedure TVaultCredential.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TVaultCredential.ContentUpdate;
begin
  inherited;
  TVaultResponseHydrator.HydrateCredential(Self);
end;

destructor TVaultCredential.Destroy;
begin
  FAuth.Free;
  inherited;
end;

{ TVaultCredentialList }

procedure TVaultCredentialList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TVaultCredentialList.ContentUpdate;
begin
  inherited;
  TVaultResponseHydrator.HydrateCredentialList(Self);
end;

destructor TVaultCredentialList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TVaultCredentialMCPProbe }

destructor TVaultCredentialMCPProbe.Destroy;
begin
  FHTTPResponse.Free;
  inherited;
end;

{ TVaultCredentialRefreshObject }

destructor TVaultCredentialRefreshObject.Destroy;
begin
  FHTTPResponse.Free;
  inherited;
end;

{ TVaultCredentialValidation }

destructor TVaultCredentialValidation.Destroy;
begin
  FMCPProbe.Free;
  FRefresh.Free;
  inherited;
end;

{ TVaultCredentialsRoute }

function TVaultCredentialsRoute.Archive(const VaultId,
  CredentialId: string): TVaultCredential;
begin
  Result := API.Post<TVaultCredential>('vaults/' + VaultId + '/credentials/' + CredentialId + '/archive');
end;

function TVaultCredentialsRoute.Create(const VaultId: string;
  const ParamProc: TVaultCredentialCreateParamProc): TVaultCredential;
begin
  Result := API.Post<TVaultCredential, TVaultCredentialCreateParams>('vaults/' + VaultId + '/credentials', ParamProc, True);
end;

function TVaultCredentialsRoute.Delete(const VaultId,
  CredentialId: string): TVaultCredentialDeleted;
begin
  Result := API.Delete<TVaultCredentialDeleted>('vaults/' + VaultId + '/credentials/' + CredentialId);
end;

function TVaultCredentialsRoute.List(const VaultId: string): TVaultCredentialList;
begin
  Result := API.Get<TVaultCredentialList>('vaults/' + VaultId + '/credentials');
end;

function TVaultCredentialsRoute.List(const VaultId: string;
  const ParamProc: TVaultCredentialListParamProc): TVaultCredentialList;
begin
  Result := API.Get<TVaultCredentialList, TVaultCredentialListParams>('vaults/' + VaultId + '/credentials', ParamProc);
end;

function TVaultCredentialsRoute.Retrieve(const VaultId,
  CredentialId: string): TVaultCredential;
begin
  Result := API.Get<TVaultCredential>('vaults/' + VaultId + '/credentials/' + CredentialId);
end;

function TVaultCredentialsRoute.Update(const VaultId, CredentialId: string;
  const ParamProc: TVaultCredentialUpdateParamProc): TVaultCredential;
begin
  Result := API.Post<TVaultCredential, TVaultCredentialUpdateParams>('vaults/' + VaultId + '/credentials/' + CredentialId, ParamProc, True);
end;

function TVaultCredentialsRoute.Validate(const VaultId,
  CredentialId: string): TVaultCredentialValidation;
begin
  Result := API.Post<TVaultCredentialValidation>('vaults/' + VaultId + '/credentials/' + CredentialId + '/mcp_oauth_validate');
end;

function TVaultCredentialsRoute.AsyncAwaitArchive(const VaultId,
  CredentialId: string;
  const Callbacks: TFunc<TPromiseVaultCredential>): TPromise<TVaultCredential>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredential>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredential>)
    begin
      Self.AsynArchive(VaultId, CredentialId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitCreate(const VaultId: string;
  const ParamProc: TVaultCredentialCreateParamProc;
  const Callbacks: TFunc<TPromiseVaultCredential>): TPromise<TVaultCredential>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredential>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredential>)
    begin
      Self.AsynCreate(VaultId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitDelete(const VaultId,
  CredentialId: string;
  const Callbacks: TFunc<TPromiseVaultCredentialDeleted>): TPromise<TVaultCredentialDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredentialDeleted>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredentialDeleted>)
    begin
      Self.AsynDelete(VaultId, CredentialId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitList(const VaultId: string;
  const Callbacks: TFunc<TPromiseVaultCredentialList>): TPromise<TVaultCredentialList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredentialList>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredentialList>)
    begin
      Self.AsynList(VaultId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitList(const VaultId: string;
  const ParamProc: TVaultCredentialListParamProc;
  const Callbacks: TFunc<TPromiseVaultCredentialList>): TPromise<TVaultCredentialList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredentialList>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredentialList>)
    begin
      Self.AsynList(VaultId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitRetrieve(const VaultId,
  CredentialId: string;
  const Callbacks: TFunc<TPromiseVaultCredential>): TPromise<TVaultCredential>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredential>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredential>)
    begin
      Self.AsynRetrieve(VaultId, CredentialId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitUpdate(const VaultId,
  CredentialId: string; const ParamProc: TVaultCredentialUpdateParamProc;
  const Callbacks: TFunc<TPromiseVaultCredential>): TPromise<TVaultCredential>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredential>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredential>)
    begin
      Self.AsynUpdate(VaultId, CredentialId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TVaultCredentialsRoute.AsyncAwaitValidate(const VaultId,
  CredentialId: string;
  const Callbacks: TFunc<TPromiseVaultCredentialValidation>): TPromise<TVaultCredentialValidation>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultCredentialValidation>(
    procedure(const CallbackParams: TFunc<TAsynVaultCredentialValidation>)
    begin
      Self.AsynValidate(VaultId, CredentialId, CallbackParams);
    end,
    Callbacks);
end;

{ TVaultCredentialsAsynchronousSupport }

procedure TVaultCredentialsAsynchronousSupport.AsynArchive(const VaultId,
  CredentialId: string; const CallBacks: TFunc<TAsynVaultCredential>);
begin
  with TAsynCallBackExec<TAsynVaultCredential, TVaultCredential>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredential
      begin
        Result := Self.Archive(VaultId, CredentialId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynCreate(const VaultId: string;
  const ParamProc: TVaultCredentialCreateParamProc;
  const CallBacks: TFunc<TAsynVaultCredential>);
begin
  with TAsynCallBackExec<TAsynVaultCredential, TVaultCredential>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredential
      begin
        Result := Self.Create(VaultId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynDelete(const VaultId,
  CredentialId: string; const CallBacks: TFunc<TAsynVaultCredentialDeleted>);
begin
  with TAsynCallBackExec<TAsynVaultCredentialDeleted, TVaultCredentialDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredentialDeleted
      begin
        Result := Self.Delete(VaultId, CredentialId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynList(const VaultId: string;
  const CallBacks: TFunc<TAsynVaultCredentialList>);
begin
  with TAsynCallBackExec<TAsynVaultCredentialList, TVaultCredentialList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredentialList
      begin
        Result := Self.List(VaultId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynList(const VaultId: string;
  const ParamProc: TVaultCredentialListParamProc;
  const CallBacks: TFunc<TAsynVaultCredentialList>);
begin
  with TAsynCallBackExec<TAsynVaultCredentialList, TVaultCredentialList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredentialList
      begin
        Result := Self.List(VaultId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynRetrieve(const VaultId,
  CredentialId: string; const CallBacks: TFunc<TAsynVaultCredential>);
begin
  with TAsynCallBackExec<TAsynVaultCredential, TVaultCredential>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredential
      begin
        Result := Self.Retrieve(VaultId, CredentialId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynUpdate(const VaultId,
  CredentialId: string; const ParamProc: TVaultCredentialUpdateParamProc;
  const CallBacks: TFunc<TAsynVaultCredential>);
begin
  with TAsynCallBackExec<TAsynVaultCredential, TVaultCredential>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredential
      begin
        Result := Self.Update(VaultId, CredentialId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TVaultCredentialsAsynchronousSupport.AsynValidate(const VaultId,
  CredentialId: string;
  const CallBacks: TFunc<TAsynVaultCredentialValidation>);
begin
  with TAsynCallBackExec<TAsynVaultCredentialValidation, TVaultCredentialValidation>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultCredentialValidation
      begin
        Result := Self.Validate(VaultId, CredentialId);
      end);
  finally
    Free;
  end;
end;

{ TVaultsRoute }

function TVaultsRoute.Archive(const VaultId: string): TVault;
begin
  Result := API.Post<TVault>('vaults/' + VaultId + '/archive');
end;

function TVaultsRoute.Create(const ParamProc: TVaultCreateParamProc): TVault;
begin
  Result := API.Post<TVault, TVaultCreateParams>('vaults', ParamProc, True);
end;

function TVaultsRoute.Delete(const VaultId: string): TVaultDeleted;
begin
  Result := API.Delete<TVaultDeleted>('vaults/' + VaultId);
end;

destructor TVaultsRoute.Destroy;
begin
  FCredentials.Free;
  inherited;
end;

function TVaultsRoute.GetCredentialsRoute: TVaultCredentialsRoute;
begin
  if FCredentials = nil then
    FCredentials := TVaultCredentialsRoute.CreateRoute(API);
  Result := FCredentials;
end;

function TVaultsRoute.List: TVaultList;
begin
  Result := API.Get<TVaultList>('vaults');
end;

function TVaultsRoute.List(const ParamProc: TVaultListParamProc): TVaultList;
begin
  Result := API.Get<TVaultList, TVaultListParams>('vaults', ParamProc);
end;

function TVaultsRoute.Retrieve(const VaultId: string): TVault;
begin
  Result := API.Get<TVault>('vaults/' + VaultId);
end;

function TVaultsRoute.Update(const VaultId: string;
  const ParamProc: TVaultUpdateParamProc): TVault;
begin
  Result := API.Post<TVault, TVaultUpdateParams>('vaults/' + VaultId, ParamProc, True);
end;

function TVaultsRoute.AsyncAwaitArchive(const VaultId: string;
  const Callbacks: TFunc<TPromiseVault>): TPromise<TVault>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVault>(
    procedure(const CallbackParams: TFunc<TAsynVault>)
    begin
      Self.AsynArchive(VaultId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitCreate(const ParamProc: TVaultCreateParamProc;
  const Callbacks: TFunc<TPromiseVault>): TPromise<TVault>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVault>(
    procedure(const CallbackParams: TFunc<TAsynVault>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitDelete(const VaultId: string;
  const Callbacks: TFunc<TPromiseVaultDeleted>): TPromise<TVaultDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultDeleted>(
    procedure(const CallbackParams: TFunc<TAsynVaultDeleted>)
    begin
      Self.AsynDelete(VaultId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseVaultList>): TPromise<TVaultList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultList>(
    procedure(const CallbackParams: TFunc<TAsynVaultList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitList(const ParamProc: TVaultListParamProc;
  const Callbacks: TFunc<TPromiseVaultList>): TPromise<TVaultList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVaultList>(
    procedure(const CallbackParams: TFunc<TAsynVaultList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitRetrieve(const VaultId: string;
  const Callbacks: TFunc<TPromiseVault>): TPromise<TVault>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVault>(
    procedure(const CallbackParams: TFunc<TAsynVault>)
    begin
      Self.AsynRetrieve(VaultId, CallbackParams);
    end,
    Callbacks);
end;

function TVaultsRoute.AsyncAwaitUpdate(const VaultId: string;
  const ParamProc: TVaultUpdateParamProc;
  const Callbacks: TFunc<TPromiseVault>): TPromise<TVault>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TVault>(
    procedure(const CallbackParams: TFunc<TAsynVault>)
    begin
      Self.AsynUpdate(VaultId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

{ TVaultsAsynchronousSupport }

procedure TVaultsAsynchronousSupport.AsynArchive(const VaultId: string;
  const CallBacks: TFunc<TAsynVault>);
begin
  with TAsynCallBackExec<TAsynVault, TVault>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVault
      begin
        Result := Self.Archive(VaultId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynCreate(
  const ParamProc: TVaultCreateParamProc; const CallBacks: TFunc<TAsynVault>);
begin
  with TAsynCallBackExec<TAsynVault, TVault>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVault
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynDelete(const VaultId: string;
  const CallBacks: TFunc<TAsynVaultDeleted>);
begin
  with TAsynCallBackExec<TAsynVaultDeleted, TVaultDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultDeleted
      begin
        Result := Self.Delete(VaultId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynList(
  const CallBacks: TFunc<TAsynVaultList>);
begin
  with TAsynCallBackExec<TAsynVaultList, TVaultList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynList(const ParamProc: TVaultListParamProc;
  const CallBacks: TFunc<TAsynVaultList>);
begin
  with TAsynCallBackExec<TAsynVaultList, TVaultList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVaultList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynRetrieve(const VaultId: string;
  const CallBacks: TFunc<TAsynVault>);
begin
  with TAsynCallBackExec<TAsynVault, TVault>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVault
      begin
        Result := Self.Retrieve(VaultId);
      end);
  finally
    Free;
  end;
end;

procedure TVaultsAsynchronousSupport.AsynUpdate(const VaultId: string;
  const ParamProc: TVaultUpdateParamProc; const CallBacks: TFunc<TAsynVault>);
begin
  with TAsynCallBackExec<TAsynVault, TVault>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TVault
      begin
        Result := Self.Update(VaultId, ParamProc);
      end);
  finally
    Free;
  end;
end;

end.
