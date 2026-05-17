unit Anthropic.Agents;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Json.Types,
  Anthropic.API.Params, Anthropic.API, Anthropic.Types, Anthropic.Schema,
  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  TAgentModelConfigParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the model identifier used to power the Agent.
    /// </summary>
    /// <remarks>
    /// Accepts Anthropic model identifiers such as <c>claude-opus-4-7</c>,
    /// pinned model identifiers, or future model identifiers accepted by the API.
    /// </remarks>
    function Id(const Value: string): TAgentModelConfigParams;

    /// <summary>
    /// Sets the optional inference speed mode for the model configuration.
    /// </summary>
    /// <remarks>
    /// Valid API values are currently <c>standard</c> and <c>fast</c>. Not all
    /// models support every speed mode; invalid combinations are rejected by the API.
    /// </remarks>
    function Speed(const Value: string): TAgentModelConfigParams;

    class function New: TAgentModelConfigParams;
  end;

  TAgentMCPServerParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the MCP server type.
    /// </summary>
    /// <remarks>
    /// The current API value is <c>url</c>. The parameter remains explicit to keep
    /// the request builder open to future server kinds.
    /// </remarks>
    function &Type(const Value: string = 'url'): TAgentMCPServerParams;

    /// <summary>
    /// Sets the unique MCP server name referenced by MCP toolset configurations.
    /// </summary>
    /// <remarks>
    /// The name must be unique within the Agent's <c>mcp_servers</c> array.
    /// </remarks>
    function Name(const Value: string): TAgentMCPServerParams;

    /// <summary>
    /// Sets the endpoint URL for the MCP server.
    /// </summary>
    function Url(const Value: string): TAgentMCPServerParams;

    class function New: TAgentMCPServerParams;
  end;

  TAgentRosterEntryParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the multiagent roster entry type.
    /// </summary>
    /// <remarks>
    /// Known API values include <c>agent</c> and <c>self</c>.
    /// </remarks>
    function &Type(const Value: string): TAgentRosterEntryParams;

    class function New(const Value: string): TAgentRosterEntryParams;
  end;

  TAgentReferenceParams = class(TAgentRosterEntryParams)
  public
    /// <summary>
    /// Sets this roster entry as a reference to another Agent.
    /// </summary>
    function &Type(const Value: string = 'agent'): TAgentReferenceParams;

    /// <summary>
    /// Sets the referenced Agent identifier.
    /// </summary>
    function Id(const Value: string): TAgentReferenceParams;

    /// <summary>
    /// Sets the specific Agent version to use.
    /// </summary>
    /// <remarks>
    /// Omit this field to use the latest version. When provided, it must be at least 1.
    /// </remarks>
    function Version(const Value: Integer): TAgentReferenceParams;

    class function New: TAgentReferenceParams;
  end;

  TAgentSelfReferenceParams = class(TAgentRosterEntryParams)
  public
    /// <summary>
    /// Sets this roster entry as the special <c>self</c> reference.
    /// </summary>
    /// <remarks>
    /// The server resolves <c>self</c> to the Agent that owns the coordinator configuration.
    /// </remarks>
    function &Type(const Value: string = 'self'): TAgentSelfReferenceParams;

    class function New: TAgentSelfReferenceParams;
  end;

  TAgentMultiagentParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the multiagent topology type.
    /// </summary>
    /// <remarks>
    /// The current API value is <c>coordinator</c>.
    /// </remarks>
    function &Type(const Value: string = 'coordinator'): TAgentMultiagentParams;

    /// <summary>
    /// Sets the coordinator roster using short-form Agent identifiers.
    /// </summary>
    /// <remarks>
    /// The roster must contain between 1 and 20 distinct entries after server-side resolution.
    /// </remarks>
    function Agents(const Value: TArray<string>): TAgentMultiagentParams; overload;

    /// <summary>
    /// Sets the coordinator roster using explicit roster entry objects.
    /// </summary>
    /// <remarks>
    /// Entries may reference Agents by id/version or use the <c>self</c> sentinel.
    /// </remarks>
    function Agents(const Value: TArray<TAgentRosterEntryParams>): TAgentMultiagentParams; overload;

    class function New: TAgentMultiagentParams;
  end;

  TAgentSkillParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the skill type.
    /// </summary>
    /// <remarks>
    /// Known API values include <c>anthropic</c> and <c>custom</c>.
    /// </remarks>
    function &Type(const Value: string): TAgentSkillParams;

    /// <summary>
    /// Sets the skill identifier.
    /// </summary>
    /// <remarks>
    /// Anthropic-managed skills use identifiers such as <c>xlsx</c>. Custom skills use
    /// tagged identifiers such as <c>skill_...</c>.
    /// </remarks>
    function SkillId(const Value: string): TAgentSkillParams;

    /// <summary>
    /// Pins the skill version.
    /// </summary>
    /// <remarks>
    /// Omit this field to let the API use the latest version.
    /// </remarks>
    function Version(const Value: string): TAgentSkillParams;

    class function New(const Value: string): TAgentSkillParams;
  end;

  TAgentAnthropicSkillParams = class(TAgentSkillParams)
  public
    /// <summary>
    /// Sets this skill reference as an Anthropic-managed skill.
    /// </summary>
    function &Type(const Value: string = 'anthropic'): TAgentAnthropicSkillParams;

    class function New: TAgentAnthropicSkillParams;
  end;

  TAgentCustomSkillParams = class(TAgentSkillParams)
  public
    /// <summary>
    /// Sets this skill reference as a user-created custom skill.
    /// </summary>
    function &Type(const Value: string = 'custom'): TAgentCustomSkillParams;

    class function New: TAgentCustomSkillParams;
  end;

  TAgentPermissionPolicyParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the tool permission policy type.
    /// </summary>
    /// <remarks>
    /// Known API values include <c>always_allow</c> and <c>always_ask</c>.
    /// </remarks>
    function &Type(const Value: string): TAgentPermissionPolicyParams;

    class function New(const Value: string): TAgentPermissionPolicyParams;
  end;

  TAgentAlwaysAllowPolicyParams = class(TAgentPermissionPolicyParams)
  public
    /// <summary>
    /// Sets a policy that automatically approves matching tool calls.
    /// </summary>
    function &Type(const Value: string = 'always_allow'): TAgentAlwaysAllowPolicyParams;

    class function New: TAgentAlwaysAllowPolicyParams;
  end;

  TAgentAlwaysAskPolicyParams = class(TAgentPermissionPolicyParams)
  public
    /// <summary>
    /// Sets a policy that requires confirmation before matching tool calls.
    /// </summary>
    function &Type(const Value: string = 'always_ask'): TAgentAlwaysAskPolicyParams;

    class function New: TAgentAlwaysAskPolicyParams;
  end;

  TAgentToolConfigParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the built-in or MCP tool name to configure.
    /// </summary>
    function Name(const Value: string): TAgentToolConfigParams;

    /// <summary>
    /// Enables or disables this tool configuration override.
    /// </summary>
    function Enabled(const Value: Boolean): TAgentToolConfigParams;

    /// <summary>
    /// Sets the permission policy override for this tool.
    /// </summary>
    function PermissionPolicy(const Value: TAgentPermissionPolicyParams): TAgentToolConfigParams;

    class function New: TAgentToolConfigParams;
  end;

  TAgentToolsetDefaultConfigParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets whether tools are enabled by default in the toolset.
    /// </summary>
    function Enabled(const Value: Boolean): TAgentToolsetDefaultConfigParams;

    /// <summary>
    /// Sets the default permission policy for tools in the toolset.
    /// </summary>
    function PermissionPolicy(const Value: TAgentPermissionPolicyParams): TAgentToolsetDefaultConfigParams;

    class function New: TAgentToolsetDefaultConfigParams;
  end;

  TAgentToolParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the Agent tool configuration type.
    /// </summary>
    /// <remarks>
    /// Known API values include <c>agent_toolset_20260401</c>, <c>mcp_toolset</c>, and <c>custom</c>.
    /// </remarks>
    function &Type(const Value: string): TAgentToolParams;

    class function New(const Value: string): TAgentToolParams;
  end;

  TAgentBuiltInToolsetParams = class(TAgentToolParams)
  public
    /// <summary>
    /// Sets this tool configuration as the built-in Agent toolset.
    /// </summary>
    function &Type(const Value: string = 'agent_toolset_20260401'): TAgentBuiltInToolsetParams;

    /// <summary>
    /// Sets per-tool overrides for built-in Agent tools.
    /// </summary>
    /// <remarks>
    /// Built-in tool names currently include <c>bash</c>, <c>edit</c>, <c>read</c>,
    /// <c>write</c>, <c>glob</c>, <c>grep</c>, <c>web_fetch</c>, and <c>web_search</c>.
    /// </remarks>
    function Configs(const Value: TArray<TAgentToolConfigParams>): TAgentBuiltInToolsetParams;

    /// <summary>
    /// Sets the default configuration for all built-in Agent tools.
    /// </summary>
    function DefaultConfig(const Value: TAgentToolsetDefaultConfigParams): TAgentBuiltInToolsetParams;

    class function New: TAgentBuiltInToolsetParams;
  end;

  TAgentMCPToolsetParams = class(TAgentToolParams)
  public
    /// <summary>
    /// Sets this tool configuration as an MCP toolset.
    /// </summary>
    function &Type(const Value: string = 'mcp_toolset'): TAgentMCPToolsetParams;

    /// <summary>
    /// Sets the MCP server name from which tools are exposed.
    /// </summary>
    /// <remarks>
    /// This value must match a server name declared in <c>mcp_servers</c>.
    /// </remarks>
    function MCPServerName(const Value: string): TAgentMCPToolsetParams;

    /// <summary>
    /// Sets per-tool overrides for tools exposed by the MCP server.
    /// </summary>
    function Configs(const Value: TArray<TAgentToolConfigParams>): TAgentMCPToolsetParams;

    /// <summary>
    /// Sets the default configuration for all tools from the MCP server.
    /// </summary>
    function DefaultConfig(const Value: TAgentToolsetDefaultConfigParams): TAgentMCPToolsetParams;

    class function New: TAgentMCPToolsetParams;
  end;

  TAgentCustomToolParams = class(TAgentToolParams)
  public
    /// <summary>
    /// Sets this tool configuration as a client-executed custom tool.
    /// </summary>
    function &Type(const Value: string = 'custom'): TAgentCustomToolParams;

    /// <summary>
    /// Sets the custom tool name.
    /// </summary>
    /// <remarks>
    /// Names may contain letters, digits, underscores, and hyphens.
    /// </remarks>
    function Name(const Value: string): TAgentCustomToolParams;

    /// <summary>
    /// Sets the custom tool description shown to the Agent.
    /// </summary>
    function Description(const Value: string): TAgentCustomToolParams;

    /// <summary>
    /// Sets the custom tool input schema using <c>Anthropic.Schema.TSchemaParams</c>.
    /// </summary>
    function InputSchema(const Value: TSchemaParams): TAgentCustomToolParams; overload;

    /// <summary>
    /// Sets the custom tool input schema from a JSON object.
    /// </summary>
    function InputSchema(const Value: TJSONObject): TAgentCustomToolParams; overload;

    /// <summary>
    /// Sets the custom tool input schema from a JSON object string.
    /// </summary>
    function InputSchema(const Value: string): TAgentCustomToolParams; overload;

    class function New: TAgentCustomToolParams;
  end;

  TAgentCreateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the model identifier for the Agent.
    /// </summary>
    function Model(const Value: string): TAgentCreateParams; overload;

    /// <summary>
    /// Sets the model identifier and additional model configuration.
    /// </summary>
    function Model(const Value: TAgentModelConfigParams): TAgentCreateParams; overload;

    /// <summary>
    /// Sets the human-readable Agent name.
    /// </summary>
    /// <remarks>
    /// The API accepts 1 to 256 characters.
    /// </remarks>
    function Name(const Value: string): TAgentCreateParams;

    /// <summary>
    /// Sets the optional Agent description.
    /// </summary>
    /// <remarks>
    /// The API accepts up to 2048 characters.
    /// </remarks>
    function Description(const Value: string): TAgentCreateParams;

    /// <summary>
    /// Sets the MCP servers this Agent connects to.
    /// </summary>
    /// <remarks>
    /// The API accepts at most 20 MCP servers. Names must be unique.
    /// </remarks>
    function MCPServers(const Value: TArray<TAgentMCPServerParams>): TAgentCreateParams;

    /// <summary>
    /// Sets arbitrary Agent metadata from a JSON object.
    /// </summary>
    /// <remarks>
    /// The API accepts up to 16 key-value pairs, keys up to 64 characters, and values up to 512 characters.
    /// </remarks>
    function Metadata(const Value: TJSONObject): TAgentCreateParams; overload;

    /// <summary>
    /// Adds or replaces a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TAgentCreateParams; overload;

    /// <summary>
    /// Sets the optional coordinator multiagent topology.
    /// </summary>
    function Multiagent(const Value: TAgentMultiagentParams): TAgentCreateParams;

    /// <summary>
    /// Sets the skills available to the Agent.
    /// </summary>
    /// <remarks>
    /// The API accepts at most 20 skills.
    /// </remarks>
    function Skills(const Value: TArray<TAgentSkillParams>): TAgentCreateParams;

    /// <summary>
    /// Sets the Agent system prompt.
    /// </summary>
    /// <remarks>
    /// The API accepts up to 100,000 characters.
    /// </remarks>
    function System(const Value: string): TAgentCreateParams;

    /// <summary>
    /// Sets the tool configurations available to the Agent.
    /// </summary>
    /// <remarks>
    /// The API accepts at most 128 tools across all toolsets.
    /// </remarks>
    function Tools(const Value: TArray<TAgentToolParams>): TAgentCreateParams;

    class function New: TAgentCreateParams;
  end;

  TAgentUpdateParams = class(TJSONParam)
  public
    /// <summary>
    /// Sets the current Agent version used for optimistic concurrency control.
    /// </summary>
    function Version(const Value: Integer): TAgentUpdateParams;

    /// <summary>
    /// Replaces the Agent model by model identifier.
    /// </summary>
    function Model(const Value: string): TAgentUpdateParams; overload;

    /// <summary>
    /// Replaces the Agent model using an explicit model configuration object.
    /// </summary>
    function Model(const Value: TAgentModelConfigParams): TAgentUpdateParams; overload;

    /// <summary>
    /// Replaces the Agent name.
    /// </summary>
    function Name(const Value: string): TAgentUpdateParams;

    /// <summary>
    /// Replaces the Agent description.
    /// </summary>
    function Description(const Value: string): TAgentUpdateParams; overload;

    /// <summary>
    /// Clears the Agent description by sending JSON null.
    /// </summary>
    function ClearDescription: TAgentUpdateParams;

    /// <summary>
    /// Replaces all MCP servers for the Agent.
    /// </summary>
    function MCPServers(const Value: TArray<TAgentMCPServerParams>): TAgentUpdateParams;

    /// <summary>
    /// Clears MCP servers by sending an empty array.
    /// </summary>
    function ClearMCPServers: TAgentUpdateParams;

    /// <summary>
    /// Patches Agent metadata from a JSON object.
    /// </summary>
    function Metadata(const Value: TJSONObject): TAgentUpdateParams; overload;

    /// <summary>
    /// Upserts a single metadata key-value pair.
    /// </summary>
    function Metadata(const Key, Value: string): TAgentUpdateParams; overload;

    /// <summary>
    /// Deletes a single metadata key by sending JSON null for that key.
    /// </summary>
    function DeleteMetadata(const Key: string): TAgentUpdateParams;

    /// <summary>
    /// Replaces the Agent multiagent topology.
    /// </summary>
    function Multiagent(const Value: TAgentMultiagentParams): TAgentUpdateParams;

    /// <summary>
    /// Replaces all skills for the Agent.
    /// </summary>
    function Skills(const Value: TArray<TAgentSkillParams>): TAgentUpdateParams;

    /// <summary>
    /// Clears skills by sending an empty array.
    /// </summary>
    function ClearSkills: TAgentUpdateParams;

    /// <summary>
    /// Replaces the Agent system prompt.
    /// </summary>
    function System(const Value: string): TAgentUpdateParams; overload;

    /// <summary>
    /// Clears the Agent system prompt by sending JSON null.
    /// </summary>
    function ClearSystem: TAgentUpdateParams;

    /// <summary>
    /// Replaces all tool configurations for the Agent.
    /// </summary>
    function Tools(const Value: TArray<TAgentToolParams>): TAgentUpdateParams;

    /// <summary>
    /// Clears tools by sending an empty array.
    /// </summary>
    function ClearTools: TAgentUpdateParams;

    class function New: TAgentUpdateParams;
  end;

  TAgentListParams = class(TUrlParam)
  public
    /// <summary>
    /// Filters Agents created at or after the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtGte(const Value: string): TAgentListParams;

    /// <summary>
    /// Filters Agents created at or before the specified RFC 3339 timestamp.
    /// </summary>
    function CreatedAtLte(const Value: string): TAgentListParams;

    /// <summary>
    /// Includes archived Agents in the list response.
    /// </summary>
    /// <remarks>
    /// The API defaults this option to false.
    /// </remarks>
    function IncludeArchived(const Value: Boolean): TAgentListParams;

    /// <summary>
    /// Sets the maximum number of Agents returned per page.
    /// </summary>
    /// <remarks>
    /// The API default is 20 and the maximum is 100.
    /// </remarks>
    function Limit(const Value: Integer): TAgentListParams;

    /// <summary>
    /// Sets the opaque pagination cursor from a previous response.
    /// </summary>
    function Page(const Value: string): TAgentListParams;

    class function New: TAgentListParams;
  end;

  TAgentRetrieveParams = class(TUrlParam)
  public
    /// <summary>
    /// Selects a specific Agent version to retrieve.
    /// </summary>
    /// <remarks>
    /// Omit this parameter to retrieve the most recent version.
    /// </remarks>
    function Version(const Value: Integer): TAgentRetrieveParams;

    class function New: TAgentRetrieveParams;
  end;

  TAgentListParamProc = TProc<TAgentListParams>;
  TAgentRetrieveParamProc = TProc<TAgentRetrieveParams>;
  TAgentCreateParamProc = TProc<TAgentCreateParams>;
  TAgentUpdateParamProc = TProc<TAgentUpdateParams>;

  TAgentModelConfig = class(TJSONFingerprint)
  private
    FId: string;
    FSpeed: string;
  public
    /// <summary>
    /// Model identifier used by the Agent.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Optional inference speed mode resolved for the Agent model.
    /// </summary>
    property Speed: string read FSpeed write FSpeed;
  end;

  TAgentMCPServer = class(TJSONFingerprint)
  private
    FName: string;
    FType: string;
    FUrl: string;
  public
    /// <summary>
    /// Unique MCP server name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// MCP server type.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// MCP server endpoint URL.
    /// </summary>
    property Url: string read FUrl write FUrl;
  end;

  TAgentReference = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
    FVersion: Integer;
  public
    /// <summary>
    /// Referenced Agent identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// Reference type, normally <c>agent</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Resolved Agent version.
    /// </summary>
    property Version: Integer read FVersion write FVersion;
  end;

  TAgentMultiagent = class(TJSONFingerprint)
  private
    FAgents: TArray<TAgentReference>;
    FType: string;
  public
    /// <summary>
    /// Resolved coordinator roster with concrete Agent references.
    /// </summary>
    property Agents: TArray<TAgentReference> read FAgents write FAgents;

    /// <summary>
    /// Multiagent topology type.
    /// </summary>
    property &Type: string read FType write FType;

    destructor Destroy; override;
  end;

  TAgentSkill = class(TJSONFingerprint)
  private
    [JsonNameAttribute('skill_id')]
    FSkillId: string;
    FType: string;
    FVersion: string;
  public
    /// <summary>
    /// Resolved skill identifier.
    /// </summary>
    property SkillId: string read FSkillId write FSkillId;

    /// <summary>
    /// Skill type, such as <c>anthropic</c> or <c>custom</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Resolved skill version.
    /// </summary>
    property Version: string read FVersion write FVersion;
  end;

  TAgentAnthropicSkill = class(TAgentSkill);
  TAgentCustomSkill = class(TAgentSkill);

  TAgentPermissionPolicy = class(TJSONFingerprint)
  private
    FType: string;
  public
    /// <summary>
    /// Permission policy type, such as <c>always_allow</c> or <c>always_ask</c>.
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TAgentAlwaysAllowPolicy = class(TAgentPermissionPolicy);
  TAgentAlwaysAskPolicy = class(TAgentPermissionPolicy);

  TAgentToolConfig = class(TJSONFingerprint)
  private
    FName: string;
    FEnabled: Boolean;
    [JsonNameAttribute('permission_policy')]
    FPermissionPolicy: TAgentPermissionPolicy;
  public
    /// <summary>
    /// Tool name to which this configuration applies.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Whether the tool is enabled in this override.
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;

    /// <summary>
    /// Permission policy override for this tool.
    /// </summary>
    property PermissionPolicy: TAgentPermissionPolicy read FPermissionPolicy write FPermissionPolicy;

    destructor Destroy; override;
  end;

  TAgentToolsetDefaultConfig = class(TJSONFingerprint)
  private
    FEnabled: Boolean;
    [JsonNameAttribute('permission_policy')]
    FPermissionPolicy: TAgentPermissionPolicy;
  public
    /// <summary>
    /// Whether tools are enabled by default.
    /// </summary>
    property Enabled: Boolean read FEnabled write FEnabled;

    /// <summary>
    /// Default permission policy for the toolset.
    /// </summary>
    property PermissionPolicy: TAgentPermissionPolicy read FPermissionPolicy write FPermissionPolicy;

    destructor Destroy; override;
  end;

  TAgentTool = class(TJSONFingerprint)
  private
    FType: string;
    FConfigs: TArray<TAgentToolConfig>;
    [JsonNameAttribute('default_config')]
    FDefaultConfig: TAgentToolsetDefaultConfig;
    [JsonNameAttribute('mcp_server_name')]
    FMCPServerName: string;
    FDescription: string;
    [JSONMarshalled(False)]
    [JsonNameAttribute('input_schema')]
    FInputSchema: string;
    FName: string;
  public
    /// <summary>
    /// Tool configuration type.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Per-tool configuration overrides.
    /// </summary>
    property Configs: TArray<TAgentToolConfig> read FConfigs write FConfigs;

    /// <summary>
    /// Default configuration for tools in the toolset.
    /// </summary>
    property DefaultConfig: TAgentToolsetDefaultConfig read FDefaultConfig write FDefaultConfig;

    /// <summary>
    /// MCP server name for MCP toolsets.
    /// </summary>
    property MCPServerName: string read FMCPServerName write FMCPServerName;

    /// <summary>
    /// Custom tool description.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// Custom tool input schema.
    /// </summary>
    property InputSchema: string read FInputSchema write FInputSchema;

    /// <summary>
    /// Custom tool name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Returns true when this tool is a built-in Agent toolset configuration.
    /// </summary>
    function IsBuiltInToolset: Boolean;

    /// <summary>
    /// Returns true when this tool is an MCP toolset configuration.
    /// </summary>
    function IsMCPToolset: Boolean;

    /// <summary>
    /// Returns true when this tool is a custom tool configuration.
    /// </summary>
    function IsCustomTool: Boolean;

    destructor Destroy; override;
  end;

  TAgentBuiltInToolset = class(TAgentTool);
  TAgentMCPToolset = class(TAgentTool);
  TAgentCustomTool = class(TAgentTool);

  TAgent = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    FDescription: string;
    [JsonNameAttribute('mcp_servers')]
    FMCPServers: TArray<TAgentMCPServer>;
    [JSONMarshalled(False)]
    FMetadata: string;
    FModel: TAgentModelConfig;
    FMultiagent: TAgentMultiagent;
    FName: string;
    FSkills: TArray<TAgentSkill>;
    FSystem: string;
    FTools: TArray<TAgentTool>;
    FType: string;
    [JsonNameAttribute('updated_at')]
    FUpdatedAt: string;
    FVersion: Integer;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Unique Agent identifier.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 timestamp at which the Agent was archived, when applicable.
    /// </summary>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// RFC 3339 timestamp at which the Agent was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Human-readable Agent description.
    /// </summary>
    property Description: string read FDescription write FDescription;

    /// <summary>
    /// MCP servers connected to the Agent.
    /// </summary>
    property MCPServers: TArray<TAgentMCPServer> read FMCPServers write FMCPServers;

    /// <summary>
    /// Raw JSON text for the Agent metadata object.
    /// </summary>
    property Metadata: string read FMetadata write FMetadata;

    /// <summary>
    /// Resolved model configuration.
    /// </summary>
    property Model: TAgentModelConfig read FModel write FModel;

    /// <summary>
    /// Resolved multiagent coordinator topology.
    /// </summary>
    property Multiagent: TAgentMultiagent read FMultiagent write FMultiagent;

    /// <summary>
    /// Human-readable Agent name.
    /// </summary>
    property Name: string read FName write FName;

    /// <summary>
    /// Skills available to the Agent.
    /// </summary>
    property Skills: TArray<TAgentSkill> read FSkills write FSkills;

    /// <summary>
    /// Agent system prompt.
    /// </summary>
    property System: string read FSystem write FSystem;

    /// <summary>
    /// Tool configurations available to the Agent.
    /// </summary>
    property Tools: TArray<TAgentTool> read FTools write FTools;

    /// <summary>
    /// Object type. For Agents, this is <c>agent</c>.
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 timestamp at which the Agent was last updated.
    /// </summary>
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;

    /// <summary>
    /// Current Agent version. Starts at 1 and increments when the Agent is modified.
    /// </summary>
    property Version: Integer read FVersion write FVersion;

    destructor Destroy; override;
  end;

  TAgentList = class(TJSONFingerprint)
  private
    FData: TArray<TAgent>;
    [JsonNameAttribute('next_page')]
    FNextPage: string;
  protected
    procedure AfterDeserialize; override;
    procedure ContentUpdate; override;
  public
    /// <summary>
    /// Page of Agents or Agent versions returned by the API.
    /// </summary>
    property Data: TArray<TAgent> read FData write FData;

    /// <summary>
    /// Opaque cursor for the next page. Empty when there are no more results.
    /// </summary>
    property NextPage: string read FNextPage write FNextPage;

    destructor Destroy; override;
  end;

  TAsynAgent = TAsynCallBack<TAgent>;
  TPromiseAgent = TPromiseCallback<TAgent>;
  TAsynAgentList = TAsynCallBack<TAgentList>;
  TPromiseAgentList = TPromiseCallback<TAgentList>;

  TAgentsAbstractSupport = class(TAnthropicAPIRoute)
  protected
    /// <summary>
    /// Creates an Agent.
    /// </summary>
    function Create(const ParamProc: TAgentCreateParamProc): TAgent; overload; virtual; abstract;

    /// <summary>
    /// Lists Agents.
    /// </summary>
    function List: TAgentList; overload; virtual; abstract;

    /// <summary>
    /// Lists Agents using query parameters.
    /// </summary>
    function List(const ParamProc: TAgentListParamProc): TAgentList; overload; virtual; abstract;

    /// <summary>
    /// Retrieves the latest version of an Agent.
    /// </summary>
    function Retrieve(const AgentId: string): TAgent; overload; virtual; abstract;

    /// <summary>
    /// Retrieves an Agent with optional version selection.
    /// </summary>
    function Retrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc): TAgent; overload; virtual; abstract;

    /// <summary>
    /// Updates an Agent.
    /// </summary>
    function Update(const AgentId: string; const ParamProc: TAgentUpdateParamProc): TAgent; overload; virtual; abstract;

    /// <summary>
    /// Archives an Agent.
    /// </summary>
    function Archive(const AgentId: string): TAgent; overload; virtual; abstract;

    /// <summary>
    /// Lists Agent versions.
    /// </summary>
    function Versions(const AgentId: string): TAgentList; overload; virtual; abstract;

    /// <summary>
    /// Lists Agent versions using query parameters.
    /// </summary>
    function Versions(const AgentId: string; const ParamProc: TAgentListParamProc): TAgentList; overload; virtual; abstract;
  end;

  TAgentsAsynchronousSupport = class(TAgentsAbstractSupport)
  protected
    procedure AsynCreate(const ParamProc: TAgentCreateParamProc; const CallBacks: TFunc<TAsynAgent>); overload;
    procedure AsynList(const CallBacks: TFunc<TAsynAgentList>); overload;
    procedure AsynList(const ParamProc: TAgentListParamProc; const CallBacks: TFunc<TAsynAgentList>); overload;
    procedure AsynRetrieve(const AgentId: string; const CallBacks: TFunc<TAsynAgent>); overload;
    procedure AsynRetrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc; const CallBacks: TFunc<TAsynAgent>); overload;
    procedure AsynUpdate(const AgentId: string; const ParamProc: TAgentUpdateParamProc; const CallBacks: TFunc<TAsynAgent>); overload;
    procedure AsynArchive(const AgentId: string; const CallBacks: TFunc<TAsynAgent>); overload;
    procedure AsynVersions(const AgentId: string; const CallBacks: TFunc<TAsynAgentList>); overload;
    procedure AsynVersions(const AgentId: string; const ParamProc: TAgentListParamProc; const CallBacks: TFunc<TAsynAgentList>); overload;
  end;

  TAgentsRoute = class(TAgentsAsynchronousSupport)
  public
    /// <summary>
    /// Creates an Agent with the supplied request parameters.
    /// </summary>
    function Create(const ParamProc: TAgentCreateParamProc): TAgent; overload; override;

    /// <summary>
    /// Lists Agents.
    /// </summary>
    function List: TAgentList; overload; override;

    /// <summary>
    /// Lists Agents with query parameters.
    /// </summary>
    function List(const ParamProc: TAgentListParamProc): TAgentList; overload; override;

    /// <summary>
    /// Retrieves the latest version of an Agent.
    /// </summary>
    function Retrieve(const AgentId: string): TAgent; overload; override;

    /// <summary>
    /// Retrieves an Agent with optional version selection.
    /// </summary>
    function Retrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc): TAgent; overload; override;

    /// <summary>
    /// Updates an Agent and returns the updated Agent.
    /// </summary>
    function Update(const AgentId: string; const ParamProc: TAgentUpdateParamProc): TAgent; overload; override;

    /// <summary>
    /// Archives an Agent and returns the archived Agent representation.
    /// </summary>
    function Archive(const AgentId: string): TAgent; overload; override;

    /// <summary>
    /// Lists versions for an Agent.
    /// </summary>
    function Versions(const AgentId: string): TAgentList; overload; override;

    /// <summary>
    /// Lists versions for an Agent with query parameters.
    /// </summary>
    function Versions(const AgentId: string; const ParamProc: TAgentListParamProc): TAgentList; overload; override;

    /// <summary>
    /// Asynchronously creates a managed agent.
    /// </summary>
    /// <param name="ParamProc">
    /// Callback used to configure the agent creation parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the created agent.
    /// </returns>
    function AsyncAwaitCreate(const ParamProc: TAgentCreateParamProc; const Callbacks: TFunc<TPromiseAgent> = nil): TPromise<TAgent>; overload;

    /// <summary>
    /// Asynchronously lists managed agents.
    /// </summary>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the agent list.
    /// </returns>
    function AsyncAwaitList(const Callbacks: TFunc<TPromiseAgentList> = nil): TPromise<TAgentList>; overload;

    /// <summary>
    /// Asynchronously lists managed agents using query parameters.
    /// </summary>
    /// <param name="ParamProc">
    /// Callback used to configure the list parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the agent list.
    /// </returns>
    function AsyncAwaitList(const ParamProc: TAgentListParamProc; const Callbacks: TFunc<TPromiseAgentList> = nil): TPromise<TAgentList>; overload;

    /// <summary>
    /// Asynchronously retrieves a managed agent.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the retrieved agent.
    /// </returns>
    function AsyncAwaitRetrieve(const AgentId: string; const Callbacks: TFunc<TPromiseAgent> = nil): TPromise<TAgent>; overload;

    /// <summary>
    /// Asynchronously retrieves a managed agent using query parameters.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent to retrieve.
    /// </param>
    /// <param name="ParamProc">
    /// Callback used to configure the retrieve parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the retrieved agent.
    /// </returns>
    function AsyncAwaitRetrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc; const Callbacks: TFunc<TPromiseAgent> = nil): TPromise<TAgent>; overload;

    /// <summary>
    /// Asynchronously updates a managed agent.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent to update.
    /// </param>
    /// <param name="ParamProc">
    /// Callback used to configure the update parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the updated agent.
    /// </returns>
    function AsyncAwaitUpdate(const AgentId: string; const ParamProc: TAgentUpdateParamProc; const Callbacks: TFunc<TPromiseAgent> = nil): TPromise<TAgent>; overload;

    /// <summary>
    /// Asynchronously archives a managed agent.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent to archive.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the archived agent.
    /// </returns>
    function AsyncAwaitArchive(const AgentId: string; const Callbacks: TFunc<TPromiseAgent> = nil): TPromise<TAgent>; overload;

    /// <summary>
    /// Asynchronously lists versions for a managed agent.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the agent version list.
    /// </returns>
    function AsyncAwaitVersions(const AgentId: string; const Callbacks: TFunc<TPromiseAgentList> = nil): TPromise<TAgentList>; overload;

    /// <summary>
    /// Asynchronously lists versions for a managed agent using query parameters.
    /// </summary>
    /// <param name="AgentId">
    /// The identifier of the agent.
    /// </param>
    /// <param name="ParamProc">
    /// Callback used to configure the list parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional promise callback configuration.
    /// </param>
    /// <returns>
    /// A promise that resolves to the agent version list.
    /// </returns>
    function AsyncAwaitVersions(const AgentId: string; const ParamProc: TAgentListParamProc; const Callbacks: TFunc<TPromiseAgentList> = nil): TPromise<TAgentList>; overload;
  end;

implementation

uses
  Anthropic.API.JsonSafeReader;

type
  TAgentResponseHydrator = class
  private
    class procedure HydrateTool(const Tool: TAgentTool;
      const Root: TJsonReader; const Path: string); static;
  public
    class procedure HydrateAgent(const Agent: TAgent); static;
    class procedure HydrateAgentList(const List: TAgentList); static;
  end;

{ TAgentResponseHydrator }

class procedure TAgentResponseHydrator.HydrateTool(const Tool: TAgentTool;
  const Root: TJsonReader; const Path: string);
begin
  if (Tool = nil) or (not Root.IsValid) or Path.IsEmpty then
    Exit;

  Tool.FInputSchema := Root.ExtractSubJson(Path + '.input_schema', Tool.FInputSchema);
end;

class procedure TAgentResponseHydrator.HydrateAgent(const Agent: TAgent);
begin
  if (Agent = nil) or Agent.JSONResponse.Trim.IsEmpty then
    Exit;

  var Root := TJsonReader.Parse(Agent.JSONResponse);
  if not Root.IsValid then
    Exit;

  Agent.FMetadata := Root.ExtractSubJson('metadata', Agent.FMetadata);

  for var I := 0 to High(Agent.FTools) do
    HydrateTool(Agent.FTools[I], Root, Format('tools[%d]', [I]));
end;

class procedure TAgentResponseHydrator.HydrateAgentList(const List: TAgentList);
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

{ TAgentModelConfigParams }

class function TAgentModelConfigParams.New: TAgentModelConfigParams;
begin
  Result := TAgentModelConfigParams.Create;
end;

function TAgentModelConfigParams.Id(const Value: string): TAgentModelConfigParams;
begin
  Result := TAgentModelConfigParams(Add('id', Value));
end;

function TAgentModelConfigParams.Speed(const Value: string): TAgentModelConfigParams;
begin
  Result := TAgentModelConfigParams(Add('speed', Value));
end;

{ TAgentMCPServerParams }

class function TAgentMCPServerParams.New: TAgentMCPServerParams;
begin
  Result := TAgentMCPServerParams.Create.&Type();
end;

function TAgentMCPServerParams.&Type(const Value: string): TAgentMCPServerParams;
begin
  Result := TAgentMCPServerParams(Add('type', Value));
end;

function TAgentMCPServerParams.Name(const Value: string): TAgentMCPServerParams;
begin
  Result := TAgentMCPServerParams(Add('name', Value));
end;

function TAgentMCPServerParams.Url(const Value: string): TAgentMCPServerParams;
begin
  Result := TAgentMCPServerParams(Add('url', Value));
end;

{ TAgentRosterEntryParams }

class function TAgentRosterEntryParams.New(const Value: string): TAgentRosterEntryParams;
begin
  Result := TAgentRosterEntryParams.Create.&Type(Value);
end;


function TAgentRosterEntryParams.&Type(const Value: string): TAgentRosterEntryParams;
begin
  Result := TAgentRosterEntryParams(Add('type', Value));
end;

{ TAgentReferenceParams }

class function TAgentReferenceParams.New: TAgentReferenceParams;
begin
  Result := TAgentReferenceParams.Create.&Type();
end;

function TAgentReferenceParams.&Type(const Value: string): TAgentReferenceParams;
begin
  Result := TAgentReferenceParams(Add('type', Value));
end;

function TAgentReferenceParams.Id(const Value: string): TAgentReferenceParams;
begin
  Result := TAgentReferenceParams(Add('id', Value));
end;

function TAgentReferenceParams.Version(const Value: Integer): TAgentReferenceParams;
begin
  Result := TAgentReferenceParams(Add('version', Value));
end;

{ TAgentSelfReferenceParams }

class function TAgentSelfReferenceParams.New: TAgentSelfReferenceParams;
begin
  Result := TAgentSelfReferenceParams.Create.&Type();
end;

function TAgentSelfReferenceParams.&Type(const Value: string): TAgentSelfReferenceParams;
begin
  Result := TAgentSelfReferenceParams(Add('type', Value));
end;

{ TAgentMultiagentParams }

class function TAgentMultiagentParams.New: TAgentMultiagentParams;
begin
  Result := TAgentMultiagentParams.Create.&Type();
end;

function TAgentMultiagentParams.&Type(const Value: string): TAgentMultiagentParams;
begin
  Result := TAgentMultiagentParams(Add('type', Value));
end;

function TAgentMultiagentParams.Agents(const Value: TArray<string>): TAgentMultiagentParams;
begin
  Result := TAgentMultiagentParams(Add('agents', Value));
end;

function TAgentMultiagentParams.Agents(const Value: TArray<TAgentRosterEntryParams>): TAgentMultiagentParams;
begin
  Result := TAgentMultiagentParams(Add('agents',
    TJSONHelper.ToJsonArray<TAgentRosterEntryParams>(Value)));
end;

{ TAgentAnthropicSkillParams }

class function TAgentAnthropicSkillParams.New: TAgentAnthropicSkillParams;
begin
  Result := TAgentAnthropicSkillParams.Create.&Type();
end;

function TAgentAnthropicSkillParams.&Type(const Value: string): TAgentAnthropicSkillParams;
begin
  Result := TAgentAnthropicSkillParams(Add('type', Value));
end;

{ TAgentCustomSkillParams }

class function TAgentCustomSkillParams.New: TAgentCustomSkillParams;
begin
  Result := TAgentCustomSkillParams.Create.&Type();
end;

function TAgentCustomSkillParams.&Type(const Value: string): TAgentCustomSkillParams;
begin
  Result := TAgentCustomSkillParams(Add('type', Value));
end;

{ TAgentSkillParams }

class function TAgentSkillParams.New(const Value: string): TAgentSkillParams;
begin
  Result := TAgentSkillParams.Create.&Type(Value);
end;

function TAgentSkillParams.&Type(const Value: string): TAgentSkillParams;
begin
  Result := TAgentSkillParams(Add('type', Value));
end;

function TAgentSkillParams.SkillId(const Value: string): TAgentSkillParams;
begin
  Result := TAgentSkillParams(Add('skill_id', Value));
end;

function TAgentSkillParams.Version(const Value: string): TAgentSkillParams;
begin
  Result := TAgentSkillParams(Add('version', Value));
end;

{ TAgentPermissionPolicyParams }

class function TAgentPermissionPolicyParams.New(const Value: string): TAgentPermissionPolicyParams;
begin
  Result := TAgentPermissionPolicyParams.Create.&Type(Value);
end;

function TAgentPermissionPolicyParams.&Type(const Value: string): TAgentPermissionPolicyParams;
begin
  Result := TAgentPermissionPolicyParams(Add('type', Value));
end;

{ TAgentAlwaysAllowPolicyParams }

class function TAgentAlwaysAllowPolicyParams.New: TAgentAlwaysAllowPolicyParams;
begin
  Result := TAgentAlwaysAllowPolicyParams.Create.&Type();
end;

function TAgentAlwaysAllowPolicyParams.&Type(const Value: string): TAgentAlwaysAllowPolicyParams;
begin
  Result := TAgentAlwaysAllowPolicyParams(Add('type', Value));
end;

{ TAgentAlwaysAskPolicyParams }

class function TAgentAlwaysAskPolicyParams.New: TAgentAlwaysAskPolicyParams;
begin
  Result := TAgentAlwaysAskPolicyParams.Create.&Type();
end;

function TAgentAlwaysAskPolicyParams.&Type(const Value: string): TAgentAlwaysAskPolicyParams;
begin
  Result := TAgentAlwaysAskPolicyParams(Add('type', Value));
end;

{ TAgentToolConfigParams }

class function TAgentToolConfigParams.New: TAgentToolConfigParams;
begin
  Result := TAgentToolConfigParams.Create;
end;

function TAgentToolConfigParams.Enabled(const Value: Boolean): TAgentToolConfigParams;
begin
  Result := TAgentToolConfigParams(Add('enabled', Value));
end;

function TAgentToolConfigParams.Name(const Value: string): TAgentToolConfigParams;
begin
  Result := TAgentToolConfigParams(Add('name', Value));
end;

function TAgentToolConfigParams.PermissionPolicy(
  const Value: TAgentPermissionPolicyParams): TAgentToolConfigParams;
begin
  Result := TAgentToolConfigParams(Add('permission_policy', Value.Detach));
end;

{ TAgentToolsetDefaultConfigParams }

class function TAgentToolsetDefaultConfigParams.New: TAgentToolsetDefaultConfigParams;
begin
  Result := TAgentToolsetDefaultConfigParams.Create;
end;

function TAgentToolsetDefaultConfigParams.Enabled(
  const Value: Boolean): TAgentToolsetDefaultConfigParams;
begin
  Result := TAgentToolsetDefaultConfigParams(Add('enabled', Value));
end;

function TAgentToolsetDefaultConfigParams.PermissionPolicy(
  const Value: TAgentPermissionPolicyParams): TAgentToolsetDefaultConfigParams;
begin
  Result := TAgentToolsetDefaultConfigParams(Add('permission_policy', Value.Detach));
end;

{ TAgentToolParams }

class function TAgentToolParams.New(const Value: string): TAgentToolParams;
begin
  Result := TAgentToolParams.Create.&Type(Value);
end;

function TAgentToolParams.&Type(const Value: string): TAgentToolParams;
begin
  Result := TAgentToolParams(Add('type', Value));
end;

{ TAgentBuiltInToolsetParams }

class function TAgentBuiltInToolsetParams.New: TAgentBuiltInToolsetParams;
begin
  Result := TAgentBuiltInToolsetParams.Create.&Type();
end;

function TAgentBuiltInToolsetParams.&Type(const Value: string): TAgentBuiltInToolsetParams;
begin
  Result := TAgentBuiltInToolsetParams(Add('type', Value));
end;

function TAgentBuiltInToolsetParams.Configs(
  const Value: TArray<TAgentToolConfigParams>): TAgentBuiltInToolsetParams;
begin
  Result := TAgentBuiltInToolsetParams(Add('configs',
    TJSONHelper.ToJsonArray<TAgentToolConfigParams>(Value)));
end;

function TAgentBuiltInToolsetParams.DefaultConfig(
  const Value: TAgentToolsetDefaultConfigParams): TAgentBuiltInToolsetParams;
begin
  Result := TAgentBuiltInToolsetParams(Add('default_config', Value.Detach));
end;

{ TAgentMCPToolsetParams }

class function TAgentMCPToolsetParams.New: TAgentMCPToolsetParams;
begin
  Result := TAgentMCPToolsetParams.Create.&Type();
end;

function TAgentMCPToolsetParams.&Type(const Value: string): TAgentMCPToolsetParams;
begin
  Result := TAgentMCPToolsetParams(Add('type', Value));
end;

function TAgentMCPToolsetParams.MCPServerName(const Value: string): TAgentMCPToolsetParams;
begin
  Result := TAgentMCPToolsetParams(Add('mcp_server_name', Value));
end;

function TAgentMCPToolsetParams.Configs(
  const Value: TArray<TAgentToolConfigParams>): TAgentMCPToolsetParams;
begin
  Result := TAgentMCPToolsetParams(Add('configs',
    TJSONHelper.ToJsonArray<TAgentToolConfigParams>(Value)));
end;

function TAgentMCPToolsetParams.DefaultConfig(
  const Value: TAgentToolsetDefaultConfigParams): TAgentMCPToolsetParams;
begin
  Result := TAgentMCPToolsetParams(Add('default_config', Value.Detach));
end;

{ TAgentCustomToolParams }

class function TAgentCustomToolParams.New: TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams.Create.&Type();
end;

function TAgentCustomToolParams.&Type(const Value: string): TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams(Add('type', Value));
end;

function TAgentCustomToolParams.Name(const Value: string): TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams(Add('name', Value));
end;

function TAgentCustomToolParams.Description(const Value: string): TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams(Add('description', Value));
end;

function TAgentCustomToolParams.InputSchema(
  const Value: TSchemaParams): TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams(Add('input_schema', Value.Detach));
end;

function TAgentCustomToolParams.InputSchema(
  const Value: TJSONObject): TAgentCustomToolParams;
begin
  Result := TAgentCustomToolParams(Add('input_schema', TJSONValue(Value.Clone)));
end;

function TAgentCustomToolParams.InputSchema(
  const Value: string): TAgentCustomToolParams;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TAgentCustomToolParams(Add('input_schema', JSONObject)));

  raise EArgumentException.Create('Invalid JSON object for input_schema');
end;

{ TAgentCreateParams }

class function TAgentCreateParams.New: TAgentCreateParams;
begin
  Result := TAgentCreateParams.Create;
end;

function TAgentCreateParams.Model(const Value: string): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('model', Value));
end;

function TAgentCreateParams.Model(const Value: TAgentModelConfigParams): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('model', Value.Detach));
end;

function TAgentCreateParams.Name(const Value: string): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('name', Value));
end;

function TAgentCreateParams.Description(const Value: string): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('description', Value));
end;

function TAgentCreateParams.MCPServers(
  const Value: TArray<TAgentMCPServerParams>): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('mcp_servers',
    TJSONHelper.ToJsonArray<TAgentMCPServerParams>(Value)));
end;

function TAgentCreateParams.Metadata(const Value: TJSONObject): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TAgentCreateParams.Metadata(const Key, Value: string): TAgentCreateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TAgentCreateParams.Multiagent(const Value: TAgentMultiagentParams): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('multiagent', Value.Detach));
end;

function TAgentCreateParams.Skills(
  const Value: TArray<TAgentSkillParams>): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('skills',
    TJSONHelper.ToJsonArray<TAgentSkillParams>(Value)));
end;

function TAgentCreateParams.System(const Value: string): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('system', Value));
end;

function TAgentCreateParams.Tools(
  const Value: TArray<TAgentToolParams>): TAgentCreateParams;
begin
  Result := TAgentCreateParams(Add('tools',
    TJSONHelper.ToJsonArray<TAgentToolParams>(Value)));
end;

{ TAgentUpdateParams }

class function TAgentUpdateParams.New: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams.Create;
end;

function TAgentUpdateParams.Version(const Value: Integer): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('version', Value));
end;

function TAgentUpdateParams.Model(const Value: string): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('model', Value));
end;

function TAgentUpdateParams.Model(const Value: TAgentModelConfigParams): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('model', Value.Detach));
end;

function TAgentUpdateParams.Name(const Value: string): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('name', Value));
end;

function TAgentUpdateParams.Description(const Value: string): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('description', Value));
end;

function TAgentUpdateParams.ClearDescription: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('description', TJSONNull.Create));
end;

function TAgentUpdateParams.MCPServers(
  const Value: TArray<TAgentMCPServerParams>): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('mcp_servers',
    TJSONHelper.ToJsonArray<TAgentMCPServerParams>(Value)));
end;

function TAgentUpdateParams.ClearMCPServers: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('mcp_servers', TJSONArray.Create));
end;

function TAgentUpdateParams.Metadata(const Value: TJSONObject): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('metadata', TJSONValue(Value.Clone)));
end;

function TAgentUpdateParams.Metadata(const Key, Value: string): TAgentUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, Value);
  Result := Self;
end;

function TAgentUpdateParams.DeleteMetadata(const Key: string): TAgentUpdateParams;
begin
  GetOrCreateObject('metadata').AddPair(Key, TJSONNull.Create);
  Result := Self;
end;

function TAgentUpdateParams.Multiagent(const Value: TAgentMultiagentParams): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('multiagent', Value.Detach));
end;

function TAgentUpdateParams.Skills(
  const Value: TArray<TAgentSkillParams>): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('skills',
    TJSONHelper.ToJsonArray<TAgentSkillParams>(Value)));
end;

function TAgentUpdateParams.ClearSkills: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('skills', TJSONArray.Create));
end;

function TAgentUpdateParams.System(const Value: string): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('system', Value));
end;

function TAgentUpdateParams.ClearSystem: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('system', TJSONNull.Create));
end;

function TAgentUpdateParams.Tools(
  const Value: TArray<TAgentToolParams>): TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('tools',
    TJSONHelper.ToJsonArray<TAgentToolParams>(Value)));
end;

function TAgentUpdateParams.ClearTools: TAgentUpdateParams;
begin
  Result := TAgentUpdateParams(Add('tools', TJSONArray.Create));
end;

{ TAgentListParams }

class function TAgentListParams.New: TAgentListParams;
begin
  Result := TAgentListParams.Create;
end;

function TAgentListParams.CreatedAtGte(const Value: string): TAgentListParams;
begin
  Result := TAgentListParams(Add('created_at[gte]', Value));
end;

function TAgentListParams.CreatedAtLte(const Value: string): TAgentListParams;
begin
  Result := TAgentListParams(Add('created_at[lte]', Value));
end;

function TAgentListParams.IncludeArchived(const Value: Boolean): TAgentListParams;
begin
  Result := TAgentListParams(Add('include_archived', Value));
end;

function TAgentListParams.Limit(const Value: Integer): TAgentListParams;
begin
  Result := TAgentListParams(Add('limit', Value));
end;

function TAgentListParams.Page(const Value: string): TAgentListParams;
begin
  Result := TAgentListParams(Add('page', Value));
end;

{ TAgentRetrieveParams }

class function TAgentRetrieveParams.New: TAgentRetrieveParams;
begin
  Result := TAgentRetrieveParams.Create;
end;

function TAgentRetrieveParams.Version(const Value: Integer): TAgentRetrieveParams;
begin
  Result := TAgentRetrieveParams(Add('version', Value));
end;

{ TAgent }

procedure TAgent.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TAgent.ContentUpdate;
begin
  inherited;
  TAgentResponseHydrator.HydrateAgent(Self);
end;

destructor TAgent.Destroy;
begin
  for var Item in FMCPServers do
    Item.Free;
  FModel.Free;
  FMultiagent.Free;
  for var Item in FSkills do
    Item.Free;
  for var Item in FTools do
    Item.Free;
  inherited;
end;

{ TAgentMultiagent }

destructor TAgentMultiagent.Destroy;
begin
  for var Item in FAgents do
    Item.Free;
  inherited;
end;

{ TAgentToolConfig }

destructor TAgentToolConfig.Destroy;
begin
  FPermissionPolicy.Free;
  inherited;
end;

{ TAgentToolsetDefaultConfig }

destructor TAgentToolsetDefaultConfig.Destroy;
begin
  FPermissionPolicy.Free;
  inherited;
end;

{ TAgentTool }

function TAgentTool.IsBuiltInToolset: Boolean;
begin
  Result := SameText(FType, 'agent_toolset_20260401');
end;

function TAgentTool.IsCustomTool: Boolean;
begin
  Result := SameText(FType, 'custom');
end;

function TAgentTool.IsMCPToolset: Boolean;
begin
  Result := SameText(FType, 'mcp_toolset');
end;

destructor TAgentTool.Destroy;
begin
  for var Item in FConfigs do
    Item.Free;
  FDefaultConfig.Free;
  inherited;
end;

{ TAgentList }

procedure TAgentList.AfterDeserialize;
begin
  inherited;
  ContentUpdate;
end;

procedure TAgentList.ContentUpdate;
begin
  inherited;
  TAgentResponseHydrator.HydrateAgentList(Self);
end;

destructor TAgentList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TAgentsRoute }

function TAgentsRoute.Create(const ParamProc: TAgentCreateParamProc): TAgent;
begin
  Result := API.Post<TAgent, TAgentCreateParams>('agents', ParamProc, True);
end;

function TAgentsRoute.List: TAgentList;
begin
  Result := API.Get<TAgentList>('agents');
end;

function TAgentsRoute.List(const ParamProc: TAgentListParamProc): TAgentList;
begin
  Result := API.Get<TAgentList, TAgentListParams>('agents', ParamProc);
end;

function TAgentsRoute.Retrieve(const AgentId: string): TAgent;
begin
  Result := API.Get<TAgent>('agents/' + AgentId);
end;

function TAgentsRoute.Retrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc): TAgent;
begin
  Result := API.Get<TAgent, TAgentRetrieveParams>('agents/' + AgentId, ParamProc);
end;

function TAgentsRoute.Update(const AgentId: string; const ParamProc: TAgentUpdateParamProc): TAgent;
begin
  Result := API.Post<TAgent, TAgentUpdateParams>('agents/' + AgentId, ParamProc, True);
end;

function TAgentsRoute.Archive(const AgentId: string): TAgent;
begin
  Result := API.Post<TAgent>('agents/' + AgentId + '/archive');
end;

function TAgentsRoute.Versions(const AgentId: string): TAgentList;
begin
  Result := API.Get<TAgentList>('agents/' + AgentId + '/versions');
end;

function TAgentsRoute.Versions(const AgentId: string; const ParamProc: TAgentListParamProc): TAgentList;
begin
  Result := API.Get<TAgentList, TAgentListParams>('agents/' + AgentId + '/versions', ParamProc);
end;

function TAgentsRoute.AsyncAwaitCreate(const ParamProc: TAgentCreateParamProc; const Callbacks: TFunc<TPromiseAgent>): TPromise<TAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgent>(
    procedure(const CallbackParams: TFunc<TAsynAgent>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitList(const Callbacks: TFunc<TPromiseAgentList>): TPromise<TAgentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgentList>(
    procedure(const CallbackParams: TFunc<TAsynAgentList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitList(const ParamProc: TAgentListParamProc; const Callbacks: TFunc<TPromiseAgentList>): TPromise<TAgentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgentList>(
    procedure(const CallbackParams: TFunc<TAsynAgentList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitRetrieve(const AgentId: string; const Callbacks: TFunc<TPromiseAgent>): TPromise<TAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgent>(
    procedure(const CallbackParams: TFunc<TAsynAgent>)
    begin
      Self.AsynRetrieve(AgentId, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitRetrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc; const Callbacks: TFunc<TPromiseAgent>): TPromise<TAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgent>(
    procedure(const CallbackParams: TFunc<TAsynAgent>)
    begin
      Self.AsynRetrieve(AgentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitUpdate(const AgentId: string; const ParamProc: TAgentUpdateParamProc; const Callbacks: TFunc<TPromiseAgent>): TPromise<TAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgent>(
    procedure(const CallbackParams: TFunc<TAsynAgent>)
    begin
      Self.AsynUpdate(AgentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitArchive(const AgentId: string; const Callbacks: TFunc<TPromiseAgent>): TPromise<TAgent>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgent>(
    procedure(const CallbackParams: TFunc<TAsynAgent>)
    begin
      Self.AsynArchive(AgentId, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitVersions(const AgentId: string; const Callbacks: TFunc<TPromiseAgentList>): TPromise<TAgentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgentList>(
    procedure(const CallbackParams: TFunc<TAsynAgentList>)
    begin
      Self.AsynVersions(AgentId, CallbackParams);
    end,
    Callbacks);
end;

function TAgentsRoute.AsyncAwaitVersions(const AgentId: string; const ParamProc: TAgentListParamProc; const Callbacks: TFunc<TPromiseAgentList>): TPromise<TAgentList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TAgentList>(
    procedure(const CallbackParams: TFunc<TAsynAgentList>)
    begin
      Self.AsynVersions(AgentId, ParamProc, CallbackParams);
    end,
    Callbacks);
end;

{ TAgentsAsynchronousSupport }

procedure TAgentsAsynchronousSupport.AsynCreate(const ParamProc: TAgentCreateParamProc; const CallBacks: TFunc<TAsynAgent>);
begin
  with TAsynCallBackExec<TAsynAgent, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynList(const CallBacks: TFunc<TAsynAgentList>);
begin
  with TAsynCallBackExec<TAsynAgentList, TAgentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgentList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynList(const ParamProc: TAgentListParamProc; const CallBacks: TFunc<TAsynAgentList>);
begin
  with TAsynCallBackExec<TAsynAgentList, TAgentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgentList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynRetrieve(const AgentId: string; const CallBacks: TFunc<TAsynAgent>);
begin
  with TAsynCallBackExec<TAsynAgent, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Retrieve(AgentId);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynRetrieve(const AgentId: string; const ParamProc: TAgentRetrieveParamProc; const CallBacks: TFunc<TAsynAgent>);
begin
  with TAsynCallBackExec<TAsynAgent, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Retrieve(AgentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynUpdate(const AgentId: string; const ParamProc: TAgentUpdateParamProc; const CallBacks: TFunc<TAsynAgent>);
begin
  with TAsynCallBackExec<TAsynAgent, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Update(AgentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynArchive(const AgentId: string; const CallBacks: TFunc<TAsynAgent>);
begin
  with TAsynCallBackExec<TAsynAgent, TAgent>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgent
      begin
        Result := Self.Archive(AgentId);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynVersions(const AgentId: string; const CallBacks: TFunc<TAsynAgentList>);
begin
  with TAsynCallBackExec<TAsynAgentList, TAgentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgentList
      begin
        Result := Self.Versions(AgentId);
      end);
  finally
    Free;
  end;
end;

procedure TAgentsAsynchronousSupport.AsynVersions(const AgentId: string; const ParamProc: TAgentListParamProc; const CallBacks: TFunc<TAsynAgentList>);
begin
  with TAsynCallBackExec<TAsynAgentList, TAgentList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TAgentList
      begin
        Result := Self.Versions(AgentId, ParamProc);
      end);
  finally
    Free;
  end;
end;

end.
