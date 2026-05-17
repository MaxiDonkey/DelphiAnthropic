unit Anthropic.Environment;

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
  TEnvironmentNetworkParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the networking policy type.
    /// </summary>
    /// <remarks>
    /// Known API values are <c>unrestricted</c> and <c>limited</c>.
    /// </remarks>
    function &Type(const Value: string): TEnvironmentNetworkParams;

    class function New(const Value: string): TEnvironmentNetworkParams;
  end;

  TEnvironmentUnrestrictedNetworkParams = class(TEnvironmentNetworkParams)
  public
    /// <summary>
    /// Sets unrestricted network access for the environment.
    /// </summary>
    function &Type(const Value: string = 'unrestricted'): TEnvironmentUnrestrictedNetworkParams;

    class function New: TEnvironmentUnrestrictedNetworkParams;
  end;

  TEnvironmentLimitedNetworkParams = class(TEnvironmentNetworkParams)
  public
    /// <summary>
    /// Sets limited network access for the environment.
    /// </summary>
    function &Type(const Value: string = 'limited'): TEnvironmentLimitedNetworkParams;

    /// <summary>
    /// Permits outbound access to MCP server endpoints configured on the Agent.
    /// </summary>
    /// <remarks>
    /// The API default is <c>false</c> when the field is omitted.
    /// </remarks>
    function AllowMCPServers(const Value: Boolean): TEnvironmentLimitedNetworkParams;

    /// <summary>
    /// Permits outbound access to public package registries such as PyPI and npm.
    /// </summary>
    /// <remarks>
    /// The API default is <c>false</c> when the field is omitted.
    /// </remarks>
    function AllowPackageManagers(const Value: Boolean): TEnvironmentLimitedNetworkParams;

    /// <summary>
    /// Sets the explicit list of hosts reachable from the container.
    /// </summary>
    function AllowedHosts(const Value: TArray<string>): TEnvironmentLimitedNetworkParams;

    /// <summary>
    /// Sends an empty allowed-hosts list.
    /// </summary>
    function ClearAllowedHosts: TEnvironmentLimitedNetworkParams;

    class function New: TEnvironmentLimitedNetworkParams;
  end;

  TEnvironmentPackagesParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the package configuration type.
    /// </summary>
    function &Type(const Value: string = 'packages'): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Ubuntu/Debian packages to install.
    /// </summary>
    function Apt(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Rust packages to install.
    /// </summary>
    function Cargo(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Ruby packages to install.
    /// </summary>
    function Gem(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Go packages to install.
    /// </summary>
    function Go(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Node.js packages to install.
    /// </summary>
    function Npm(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sets Python packages to install.
    /// </summary>
    /// <remarks>
    /// Version pins should use the package manager semantics, for example <c>package==1.0.0</c> for pip.
    /// </remarks>
    function Pip(const Value: TArray<string>): TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty apt package list.
    /// </summary>
    function ClearApt: TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty cargo package list.
    /// </summary>
    function ClearCargo: TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty gem package list.
    /// </summary>
    function ClearGem: TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty go package list.
    /// </summary>
    function ClearGo: TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty npm package list.
    /// </summary>
    function ClearNpm: TEnvironmentPackagesParams;

    /// <summary>
    /// Sends an empty pip package list.
    /// </summary>
    function ClearPip: TEnvironmentPackagesParams;

    class function New: TEnvironmentPackagesParams;
  end;

  TEnvironmentCloudConfigParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the environment configuration type.
    /// </summary>
    function &Type(const Value: string = 'cloud'): TEnvironmentCloudConfigParams;

    /// <summary>
    /// Sets the network configuration policy.
    /// </summary>
    /// <remarks>
    /// Omit this field on update to preserve the existing networking policy.
    /// </remarks>
    function Networking(const Value: TEnvironmentNetworkParams): TEnvironmentCloudConfigParams;

    /// <summary>
    /// Sets the package manager configuration.
    /// </summary>
    function Packages(const Value: TEnvironmentPackagesParams): TEnvironmentCloudConfigParams;

    class function New: TEnvironmentCloudConfigParams;
  end;

  TEnvironmentCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the human-readable environment name.
    /// </summary>
    function Name(const Value: string): TEnvironmentCreateParams;

    /// <summary>
    /// Sets the optional cloud environment configuration.
    /// </summary>
    function Config(const Value: TEnvironmentCloudConfigParams): TEnvironmentCreateParams;

    /// <summary>
    /// Sets the optional environment description.
    /// </summary>
    function Description(const Value: string): TEnvironmentCreateParams;

    /// <summary>
    /// Sets arbitrary environment metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TEnvironmentCreateParams; overload;

    /// <summary>
    /// Adds or replaces a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TEnvironmentCreateParams; overload;

    class function New: TEnvironmentCreateParams;
  end;

  TEnvironmentUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Replaces the environment name.
    /// </summary>
    function Name(const Value: string): TEnvironmentUpdateParams;

    /// <summary>
    /// Replaces the environment description.
    /// </summary>
    function Description(const Value: string): TEnvironmentUpdateParams;

    /// <summary>
    /// Clears the environment description by sending JSON null.
    /// </summary>
    function ClearDescription: TEnvironmentUpdateParams;

    /// <summary>
    /// Patches the cloud environment configuration.
    /// </summary>
    /// <remarks>
    /// Omitted nested fields preserve the existing values on the server.
    /// </remarks>
    function Config(const Value: TEnvironmentCloudConfigParams): TEnvironmentUpdateParams;

    /// <summary>
    /// Patches environment metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TEnvironmentUpdateParams; overload;

    /// <summary>
    /// Upserts a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TEnvironmentUpdateParams; overload;

    /// <summary>
    /// Deletes a single metadata key by sending JSON null for that key.
    /// </summary>
    function DeleteMetadata(const Key: string): TEnvironmentUpdateParams;

    /// <summary>
    /// Deletes a single metadata key by sending an empty string for that key.
    /// </summary>
    function DeleteMetadataWithEmptyString(const Key: string): TEnvironmentUpdateParams;

    class function New: TEnvironmentUpdateParams;
  end;

  TEnvironmentListParams = class(TUrlParam)
  public
    /// <summary>
    /// Includes archived environments in the list response.
    /// </summary>
    function IncludeArchived(const Value: Boolean): TEnvironmentListParams;

    /// <summary>
    /// Sets the maximum number of environments returned per page.
    /// </summary>
    function Limit(const Value: Integer): TEnvironmentListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TEnvironmentListParams;

    class function New: TEnvironmentListParams;
  end;

  TEnvironmentListParamProc = TProc<TEnvironmentListParams>;
  TEnvironmentCreateParamProc = TProc<TEnvironmentCreateParams>;
  TEnvironmentUpdateParamProc = TProc<TEnvironmentUpdateParams>;

  TEnvironmentNetwork = class(TJSONFingerprint)
  private
    FType: string;
    [JsonNameAttribute('allow_mcp_servers')]
    FAllowMCPServers: Boolean;
    [JsonNameAttribute('allow_package_managers')]
    FAllowPackageManagers: Boolean;
    [JsonNameAttribute('allowed_hosts')]
    FAllowedHosts: TArray<string>;
  public
    /// <summary>
    /// Network policy type, normally <c>unrestricted</c> or <c>limited</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Whether outbound access to configured MCP server endpoints is allowed.
    /// </summary>
    property AllowMCPServers: Boolean read FAllowMCPServers write FAllowMCPServers;

    /// <summary>
    /// Whether outbound access to public package registries is allowed.
    /// </summary>
    property AllowPackageManagers: Boolean read FAllowPackageManagers write FAllowPackageManagers;

    /// <summary>
    /// Explicit list of domains the container can reach.
    /// </summary>
    property AllowedHosts: TArray<string> read FAllowedHosts write FAllowedHosts;

    /// <summary>
    /// Returns true when the resolved policy is unrestricted.
    /// </summary>
    function IsUnrestricted: Boolean;

    /// <summary>
    /// Returns true when the resolved policy is limited.
    /// </summary>
    function IsLimited: Boolean;
  end;

  TEnvironmentUnrestrictedNetwork = class(TEnvironmentNetwork);
  TEnvironmentLimitedNetwork = class(TEnvironmentNetwork);

  TEnvironmentPackages = class(TJSONFingerprint)
  private
    FApt: TArray<string>;
    FCargo: TArray<string>;
    FGem: TArray<string>;
    FGo: TArray<string>;
    FNpm: TArray<string>;
    FPip: TArray<string>;
    FType: string;
  public
    /// <summary>
    /// Ubuntu/Debian packages installed in the environment.
    /// </summary>
    property Apt: TArray<string> read FApt write FApt;

    /// <summary>
    /// Rust packages installed in the environment.
    /// </summary>
    property Cargo: TArray<string> read FCargo write FCargo;

    /// <summary>
    /// Ruby packages installed in the environment.
    /// </summary>
    property Gem: TArray<string> read FGem write FGem;

    /// <summary>
    /// Go packages installed in the environment.
    /// </summary>
    property Go: TArray<string> read FGo write FGo;

    /// <summary>
    /// Node.js packages installed in the environment.
    /// </summary>
    property Npm: TArray<string> read FNpm write FNpm;

    /// <summary>
    /// Python packages installed in the environment.
    /// </summary>
    property Pip: TArray<string> read FPip write FPip;

    /// <summary>
    /// Package configuration type, normally <c>packages</c>.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TEnvironmentCloudConfig = class(TJSONFingerprint)
  private
    FType: string;
    FNetworking: TEnvironmentNetwork;
    FPackages: TEnvironmentPackages;
  public
    /// <summary>
    /// Environment configuration type, normally <c>cloud</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Resolved network configuration policy.
    /// </summary>
    property Networking: TEnvironmentNetwork read FNetworking write FNetworking;

    /// <summary>
    /// Resolved package manager configuration.
    /// </summary>
    property Packages: TEnvironmentPackages read FPackages write FPackages;

    destructor Destroy; override;
  end;

  TEnvironment = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    FConfig: TEnvironmentCloudConfig;
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
    /// Unique environment identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 timestamp at which the environment was archived, when applicable.
    /// </summary>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// Resolved cloud environment configuration.
    /// </summary>
    property Config: TEnvironmentCloudConfig read FConfig write FConfig;

    /// <summary>
    /// RFC 3339 timestamp at which the environment was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Human-readable environment description.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Raw JSON text for the environment metadata object.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;

    /// <summary>
    /// Human-readable environment name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Object type. For environments, this is <c>environment</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which the environment was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    destructor Destroy; override;
  end;

  TEnvironmentList = class(TJSONFingerprint)
  private
    FData: TArray<TEnvironment>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Page of environments returned by the API.
    /// </summary>
    property Data: TArray<TEnvironment> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TEnvironmentDeleteResponse = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Deleted environment identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Delete response type, normally <c>environment_deleted</c>.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TAsynEnvironment = TAsynCallBack<TEnvironment>;
  TPromiseEnvironment = TPromiseCallback<TEnvironment>;
  TAsynEnvironmentList = TAsynCallBack<TEnvironmentList>;
  TPromiseEnvironmentList = TPromiseCallback<TEnvironmentList>;
  TAsynEnvironmentDeleteResponse = TAsynCallBack<TEnvironmentDeleteResponse>;
  TPromiseEnvironmentDeleteResponse = TPromiseCallback<TEnvironmentDeleteResponse>;

  TEnvironmentsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    /// <summary>
    /// Creates an environment.
    /// </summary>
    function Create(const ParamProc: TEnvironmentCreateParamProc): TEnvironment; overload; virtual; abstract;

    /// <summary>
    /// Lists environments.
    /// </summary>
    function List: TEnvironmentList; overload; virtual; abstract;

    /// <summary>
    /// Lists environments using query parameters.
    /// </summary>
    function List(const ParamProc: TEnvironmentListParamProc): TEnvironmentList; overload; virtual; abstract;

    /// <summary>
    /// Retrieves an environment.
    /// </summary>
    function Retrieve(const EnvironmentId: string): TEnvironment; overload; virtual; abstract;

    /// <summary>
    /// Updates an environment.
    /// </summary>
    function Update(const EnvironmentId: string; const ParamProc: TEnvironmentUpdateParamProc): TEnvironment; overload; virtual; abstract;

    /// <summary>
    /// Deletes an environment.
    /// </summary>
    function Delete(const EnvironmentId: string): TEnvironmentDeleteResponse; overload; virtual; abstract;

    /// <summary>
    /// Archives an environment.
    /// </summary>
    function Archive(const EnvironmentId: string): TEnvironment; overload; virtual; abstract;
  end;

  TEnvironmentsAsynchronousSupport = class(TEnvironmentsAbstractSupport)
  protected
    procedure AsynCreate(const ParamProc: TEnvironmentCreateParamProc; const CallBacks: TFunc<TAsynEnvironment>); overload;
    procedure AsynList(const CallBacks: TFunc<TAsynEnvironmentList>); overload;
    procedure AsynList(const ParamProc: TEnvironmentListParamProc; const CallBacks: TFunc<TAsynEnvironmentList>); overload;
    procedure AsynRetrieve(const EnvironmentId: string; const CallBacks: TFunc<TAsynEnvironment>); overload;
    procedure AsynUpdate(const EnvironmentId: string; const ParamProc: TEnvironmentUpdateParamProc; const CallBacks: TFunc<TAsynEnvironment>); overload;
    procedure AsynDelete(const EnvironmentId: string; const CallBacks: TFunc<TAsynEnvironmentDeleteResponse>); overload;
    procedure AsynArchive(const EnvironmentId: string; const CallBacks: TFunc<TAsynEnvironment>); overload;
  end;

  TEnvironmentsRoute = class(TEnvironmentsAsynchronousSupport)
  public
    /// <summary>
    /// Creates an environment with the supplied request parameters.
    /// </summary>
    function Create(const ParamProc: TEnvironmentCreateParamProc): TEnvironment; overload; override;

    /// <summary>
    /// Lists environments.
    /// </summary>
    function List: TEnvironmentList; overload; override;

    /// <summary>
    /// Lists environments with query parameters.
    /// </summary>
    function List(const ParamProc: TEnvironmentListParamProc): TEnvironmentList; overload; override;

    /// <summary>
    /// Retrieves an environment.
    /// </summary>
    function Retrieve(const EnvironmentId: string): TEnvironment; overload; override;

    /// <summary>
    /// Updates an environment and returns the updated representation.
    /// </summary>
    function Update(const EnvironmentId: string; const ParamProc: TEnvironmentUpdateParamProc): TEnvironment; overload; override;

    /// <summary>
    /// Deletes an environment and returns the deletion confirmation.
    /// </summary>
    function Delete(const EnvironmentId: string): TEnvironmentDeleteResponse; overload; override;

    /// <summary>
    /// Archives an environment and returns the archived representation.
    /// </summary>
    function Archive(const EnvironmentId: string): TEnvironment; overload; override;

    /// <summary>
    /// Asynchronously creates a managed environment.
    /// </summary>
    function AsyncAwaitCreate(const ParamProc: TEnvironmentCreateParamProc; const Callbacks: TFunc<TPromiseEnvironment> = nil): TPromise<TEnvironment>; overload;

    /// <summary>
    /// Asynchronously lists managed environments.
    /// </summary>
    function AsyncAwaitList(const Callbacks: TFunc<TPromiseEnvironmentList> = nil): TPromise<TEnvironmentList>; overload;

    /// <summary>
    /// Asynchronously lists managed environments using query parameters.
    /// </summary>
    function AsyncAwaitList(const ParamProc: TEnvironmentListParamProc;
      const Callbacks: TFunc<TPromiseEnvironmentList> = nil): TPromise<TEnvironmentList>; overload;

    /// <summary>
    /// Asynchronously retrieves a managed environment.
    /// </summary>
    function AsyncAwaitRetrieve(const EnvironmentId: string;
      const Callbacks: TFunc<TPromiseEnvironment> = nil): TPromise<TEnvironment>; overload;

    /// <summary>
    /// Asynchronously updates a managed environment.
    /// </summary>
    function AsyncAwaitUpdate(const EnvironmentId: string;
      const ParamProc: TEnvironmentUpdateParamProc;
      const Callbacks: TFunc<TPromiseEnvironment> = nil): TPromise<TEnvironment>; overload;

    /// <summary>
    /// Asynchronously deletes a managed environment.
    /// </summary>
    function AsyncAwaitDelete(const EnvironmentId: string;
      const Callbacks: TFunc<TPromiseEnvironmentDeleteResponse> = nil): TPromise<TEnvironmentDeleteResponse>; overload;

    /// <summary>
    /// Asynchronously archives a managed environment.
    /// </summary>
    function AsyncAwaitArchive(const EnvironmentId: string;
      const Callbacks: TFunc<TPromiseEnvironment> = nil): TPromise<TEnvironment>; overload;
  end;

implementation

uses
  Anthropic.API.JsonSafeReader;

type
  TEnvironmentResponseHydrator = class
  public
    class procedure HydrateEnvironment(const Environment: TEnvironment); static;
    class procedure HydrateEnvironmentList(const List: TEnvironmentList); static;
  end;

{ TEnvironmentResponseHydrator }

class procedure TEnvironmentResponseHydrator.HydrateEnvironment(const Environment: TEnvironment);
begin
  if (Environment = nil) or Environment.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Environment.JSONResponse);
  if not Root.IsValid then
    Exit;

  Environment.FMetadata := Root.ExtractSubJson('metadata', Environment.FMetadata);
end;

class procedure TEnvironmentResponseHydrator.HydrateEnvironmentList(const List: TEnvironmentList);
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

{ TEnvironmentNetworkParams }

class function TEnvironmentNetworkParams.New(const Value: string): TEnvironmentNetworkParams;
begin
  Result := TEnvironmentNetworkParams.Create.&Type(Value);
end;

function TEnvironmentNetworkParams.&Type(const Value: string): TEnvironmentNetworkParams;
begin
  Result := TEnvironmentNetworkParams(Add('type', Value));
end;

{ TEnvironmentUnrestrictedNetworkParams }

class function TEnvironmentUnrestrictedNetworkParams.New: TEnvironmentUnrestrictedNetworkParams;
begin
  Result := TEnvironmentUnrestrictedNetworkParams.Create.&Type();
end;

function TEnvironmentUnrestrictedNetworkParams.&Type(const Value: string): TEnvironmentUnrestrictedNetworkParams;
begin
  Result := TEnvironmentUnrestrictedNetworkParams(Add('type', Value));
end;

{ TEnvironmentLimitedNetworkParams }

class function TEnvironmentLimitedNetworkParams.New: TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams.Create.&Type();
end;

function TEnvironmentLimitedNetworkParams.&Type(const Value: string): TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams(Add('type', Value));
end;

function TEnvironmentLimitedNetworkParams.AllowMCPServers(
  const Value: Boolean): TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams(Add('allow_mcp_servers', Value));
end;

function TEnvironmentLimitedNetworkParams.AllowPackageManagers(
  const Value: Boolean): TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams(Add('allow_package_managers', Value));
end;

function TEnvironmentLimitedNetworkParams.AllowedHosts(
  const Value: TArray<string>): TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams(Add('allowed_hosts', Value));
end;

function TEnvironmentLimitedNetworkParams.ClearAllowedHosts: TEnvironmentLimitedNetworkParams;
begin
  Result := TEnvironmentLimitedNetworkParams(Add('allowed_hosts', TJSONArray.Create));
end;

{ TEnvironmentPackagesParams }

class function TEnvironmentPackagesParams.New: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams.Create.&Type();
end;

function TEnvironmentPackagesParams.&Type(const Value: string): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('type', Value));
end;

function TEnvironmentPackagesParams.Apt(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('apt', Value));
end;

function TEnvironmentPackagesParams.Cargo(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('cargo', Value));
end;

function TEnvironmentPackagesParams.Gem(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('gem', Value));
end;

function TEnvironmentPackagesParams.Go(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('go', Value));
end;

function TEnvironmentPackagesParams.Npm(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('npm', Value));
end;

function TEnvironmentPackagesParams.Pip(const Value: TArray<string>): TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('pip', Value));
end;

function TEnvironmentPackagesParams.ClearApt: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('apt', TJSONArray.Create));
end;

function TEnvironmentPackagesParams.ClearCargo: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('cargo', TJSONArray.Create));
end;

function TEnvironmentPackagesParams.ClearGem: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('gem', TJSONArray.Create));
end;

function TEnvironmentPackagesParams.ClearGo: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('go', TJSONArray.Create));
end;

function TEnvironmentPackagesParams.ClearNpm: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('npm', TJSONArray.Create));
end;

function TEnvironmentPackagesParams.ClearPip: TEnvironmentPackagesParams;
begin
  Result := TEnvironmentPackagesParams(Add('pip', TJSONArray.Create));
end;

{ TEnvironmentCloudConfigParams }

class function TEnvironmentCloudConfigParams.New: TEnvironmentCloudConfigParams;
begin
  Result := TEnvironmentCloudConfigParams.Create.&Type();
end;

function TEnvironmentCloudConfigParams.&Type(const Value: string): TEnvironmentCloudConfigParams;
begin
  Result := TEnvironmentCloudConfigParams(Add('type', Value));
end;

function TEnvironmentCloudConfigParams.Networking(
  const Value: TEnvironmentNetworkParams): TEnvironmentCloudConfigParams;
begin
  Result := TEnvironmentCloudConfigParams(Add('networking', Value.Detach));
end;

function TEnvironmentCloudConfigParams.Packages(
  const Value: TEnvironmentPackagesParams): TEnvironmentCloudConfigParams;
begin
  Result := TEnvironmentCloudConfigParams(Add('packages', Value.Detach));
end;

{ TEnvironmentCreateParams }

class function TEnvironmentCreateParams.New: TEnvironmentCreateParams;
begin
  Result := TEnvironmentCreateParams.Create;
end;

function TEnvironmentCreateParams.Name(const Value: string): TEnvironmentCreateParams;
begin
  Result := TEnvironmentCreateParams(Add('name', Value));
end;

function TEnvironmentCreateParams.Config(
  const Value: TEnvironmentCloudConfigParams): TEnvironmentCreateParams;
begin
  Result := TEnvironmentCreateParams(Add('config', Value.Detach));
end;

function TEnvironmentCreateParams.Description(const Value: string): TEnvironmentCreateParams;
begin
  Result := TEnvironmentCreateParams(Add('description', Value));
end;

function TEnvironmentCreateParams.Metadata(const Value: TJSONObject): TEnvironmentCreateParams;
begin
  Result := TEnvironmentCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TEnvironmentCreateParams.Metadata(const Key, Value: string): TEnvironmentCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

{ TEnvironmentUpdateParams }

class function TEnvironmentUpdateParams.New: TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams.Create;
end;

function TEnvironmentUpdateParams.Name(const Value: string): TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams(Add('name', Value));
end;

function TEnvironmentUpdateParams.Description(const Value: string): TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams(Add('description', Value));
end;

function TEnvironmentUpdateParams.ClearDescription: TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams(Add('description', TJSONNull.Create));
end;

function TEnvironmentUpdateParams.Config(
  const Value: TEnvironmentCloudConfigParams): TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams(Add('config', Value.Detach));
end;

function TEnvironmentUpdateParams.Metadata(const Value: TJSONObject): TEnvironmentUpdateParams;
begin
  Result := TEnvironmentUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TEnvironmentUpdateParams.Metadata(const Key, Value: string): TEnvironmentUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TEnvironmentUpdateParams.DeleteMetadata(const Key: string): TEnvironmentUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

function TEnvironmentUpdateParams.DeleteMetadataWithEmptyString(const Key: string): TEnvironmentUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, '');
  Result := Self;
end;

{ TEnvironmentListParams }

class function TEnvironmentListParams.New: TEnvironmentListParams;
begin
  Result := TEnvironmentListParams.Create;
end;

function TEnvironmentListParams.IncludeArchived(const Value: Boolean): TEnvironmentListParams;
begin
  Result := TEnvironmentListParams(Add('include_archived', Value));
end;

function TEnvironmentListParams.Limit(const Value: Integer): TEnvironmentListParams;
begin
  Result := TEnvironmentListParams(Add('limit', Value));
end;

function TEnvironmentListParams.Page(const Value: string): TEnvironmentListParams;
begin
  Result := TEnvironmentListParams(Add('page', Value));
end;

{ TEnvironmentNetwork }

function TEnvironmentNetwork.IsLimited: Boolean;
begin
  Result := SameText(FType, 'limited');
end;

function TEnvironmentNetwork.IsUnrestricted: Boolean;
begin
  Result := SameText(FType, 'unrestricted');
end;

{ TEnvironmentCloudConfig }

destructor TEnvironmentCloudConfig.Destroy;
begin
  FNetworking.Free;
  FPackages.Free;
  inherited;
end;

{ TEnvironment }

procedure TEnvironment.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TEnvironment.ContentUpdate;
begin
  inherited;
  TEnvironmentResponseHydrator.HydrateEnvironment(Self);
end;

destructor TEnvironment.Destroy;
begin
  FConfig.Free;
  inherited;
end;

{ TEnvironmentList }

procedure TEnvironmentList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TEnvironmentList.ContentUpdate;
begin
  inherited;
  TEnvironmentResponseHydrator.HydrateEnvironmentList(Self);
end;

destructor TEnvironmentList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TEnvironmentsRoute }

function TEnvironmentsRoute.Create(const ParamProc: TEnvironmentCreateParamProc): TEnvironment;
begin
  Result := API.Post<TEnvironment, TEnvironmentCreateParams>('environments', ParamProc, True);
end;

function TEnvironmentsRoute.List: TEnvironmentList;
begin
  Result := API.Get<TEnvironmentList>('environments');
end;

function TEnvironmentsRoute.List(const ParamProc: TEnvironmentListParamProc): TEnvironmentList;
begin
  Result := API.Get<TEnvironmentList, TEnvironmentListParams>('environments', ParamProc);
end;

function TEnvironmentsRoute.Retrieve(const EnvironmentId: string): TEnvironment;
begin
  Result := API.Get<TEnvironment>('environments/' + EnvironmentId);
end;

function TEnvironmentsRoute.Update(const EnvironmentId: string;
  const ParamProc: TEnvironmentUpdateParamProc): TEnvironment;
begin
  Result := API.Post<TEnvironment, TEnvironmentUpdateParams>('environments/' + EnvironmentId, ParamProc, True);
end;

function TEnvironmentsRoute.Delete(const EnvironmentId: string): TEnvironmentDeleteResponse;
begin
  Result := API.Delete<TEnvironmentDeleteResponse>('environments/' + EnvironmentId);
end;

function TEnvironmentsRoute.Archive(const EnvironmentId: string): TEnvironment;
begin
  Result := API.Post<TEnvironment>('environments/' + EnvironmentId + '/archive');
end;

function TEnvironmentsRoute.AsyncAwaitCreate(const ParamProc: TEnvironmentCreateParamProc;
  const Callbacks: TFunc<TPromiseEnvironment>): TPromise<TEnvironment>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironment>(
    procedure(const CallbackParams: TFunc<TAsynEnvironment>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseEnvironmentList>): TPromise<TEnvironmentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironmentList>(
    procedure(const CallbackParams: TFunc<TAsynEnvironmentList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitList(const ParamProc: TEnvironmentListParamProc;
  const Callbacks: TFunc<TPromiseEnvironmentList>): TPromise<TEnvironmentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironmentList>(
    procedure(const CallbackParams: TFunc<TAsynEnvironmentList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitRetrieve(const EnvironmentId: string;
  const Callbacks: TFunc<TPromiseEnvironment>): TPromise<TEnvironment>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironment>(
    procedure(const CallbackParams: TFunc<TAsynEnvironment>)
    begin
      Self.AsynRetrieve(EnvironmentId, CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitUpdate(const EnvironmentId: string;
  const ParamProc: TEnvironmentUpdateParamProc;
  const Callbacks: TFunc<TPromiseEnvironment>): TPromise<TEnvironment>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironment>(
    procedure(const CallbackParams: TFunc<TAsynEnvironment>)
    begin
      Self.AsynUpdate(EnvironmentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitDelete(const EnvironmentId: string;
  const Callbacks: TFunc<TPromiseEnvironmentDeleteResponse>): TPromise<TEnvironmentDeleteResponse>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironmentDeleteResponse>(
    procedure(const CallbackParams: TFunc<TAsynEnvironmentDeleteResponse>)
    begin
      Self.AsynDelete(EnvironmentId, CallbackParams);
    end,
    Callbacks);
end;

function TEnvironmentsRoute.AsyncAwaitArchive(const EnvironmentId: string;
  const Callbacks: TFunc<TPromiseEnvironment>): TPromise<TEnvironment>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TEnvironment>(
    procedure(const CallbackParams: TFunc<TAsynEnvironment>)
    begin
      Self.AsynArchive(EnvironmentId, CallbackParams);
    end,
    Callbacks);
end;

{ TEnvironmentsAsynchronousSupport }

procedure TEnvironmentsAsynchronousSupport.AsynCreate(const ParamProc: TEnvironmentCreateParamProc;
  const CallBacks: TFunc<TAsynEnvironment>);
begin
  with TAsynCallBackExec<TAsynEnvironment, TEnvironment>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironment
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynList(
  const CallBacks: TFunc<TAsynEnvironmentList>);
begin
  with TAsynCallBackExec<TAsynEnvironmentList, TEnvironmentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironmentList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynList(const ParamProc: TEnvironmentListParamProc;
  const CallBacks: TFunc<TAsynEnvironmentList>);
begin
  with TAsynCallBackExec<TAsynEnvironmentList, TEnvironmentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironmentList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynRetrieve(const EnvironmentId: string;
  const CallBacks: TFunc<TAsynEnvironment>);
begin
  with TAsynCallBackExec<TAsynEnvironment, TEnvironment>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironment
      begin
        Result := Self.Retrieve(EnvironmentId);
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynUpdate(const EnvironmentId: string;
  const ParamProc: TEnvironmentUpdateParamProc;
  const CallBacks: TFunc<TAsynEnvironment>);
begin
  with TAsynCallBackExec<TAsynEnvironment, TEnvironment>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironment
      begin
        Result := Self.Update(EnvironmentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynDelete(const EnvironmentId: string;
  const CallBacks: TFunc<TAsynEnvironmentDeleteResponse>);
begin
  with TAsynCallBackExec<TAsynEnvironmentDeleteResponse, TEnvironmentDeleteResponse>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironmentDeleteResponse
      begin
        Result := Self.Delete(EnvironmentId);
      end);
  finally
    Free;
  end;
end;

procedure TEnvironmentsAsynchronousSupport.AsynArchive(const EnvironmentId: string;
  const CallBacks: TFunc<TAsynEnvironment>);
begin
  with TAsynCallBackExec<TAsynEnvironment, TEnvironment>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TEnvironment
      begin
        Result := Self.Archive(EnvironmentId);
      end);
  finally
    Free;
  end;
end;

end.
