# Agents [beta]

An **Agent** is a reusable, versioned agent definition that bundles a system prompt, default model, tools, MCP servers, and Skills into a single addressable resource. Sessions reference an agent by id (and optionally pin a version), so updating the agent does not require rewriting client code.

- [Overview](#overview)
- [Minimal create](#minimal-create)
- [Create with model configuration](#create-with-model-configuration)
- [Create with Skills](#create-with-skills)
- [Create with built-in toolset](#create-with-built-in-toolset)
- [Create with MCP servers and MCP toolset](#create-with-mcp-servers-and-mcp-toolset)
- [Create with a custom tool](#create-with-a-custom-tool)
- [Create with multi-agent topology (coordinator)](#create-with-multi-agent-topology-coordinator)
- [Retrieve (latest or pinned)](#retrieve-latest-or-pinned)
- [List with filters and pagination](#list-with-filters-and-pagination)
- [Update (with optimistic concurrency control)](#update-with-optimistic-concurrency-control)
- [Update — clearing fields](#update--clearing-fields)
- [Archive](#archive)
- [Versions](#versions)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

The wrapper exposes the Agents API through `Client.Agents`, returning typed `TAgent` / `TAgentList` instances. Every operation has both a blocking variant and a promise-based variant (`AsyncAwait*`).

Agents are **versioned**: every update produces a new version, and `Client.Agents.Versions(AgentId)` enumerates the version history. Sessions can either follow the latest version or pin a specific one.

Two construction styles are supported and interchangeable:

- **Direct fluent** — `TAgentXxxParams.New.Field(...)`.
- **Via the `Generation` helper** (`Anthropic.Helpers`) — `Generation.Agent.CreateXxx.Field(...)` for individual params, and `Generation.AgentXxxParts.AddYyy(...)` for typed array builders. The latter implicitly converts to a `TArray<T>` when passed to `.Skills(...)`, `.Tools(...)`, `.MCPServers(...)`, etc.

The examples below use the `Generation` style for readability.

>[!IMPORTANT]
> The `managed-agents-2026-04-01` beta header is set automatically by the wrapper when any `Client.Agents.*` route is called.

<br>

## Minimal create

The minimum required fields to register a usable Agent are a `Model` and a `Name`. A `System` prompt is strongly recommended.

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Async.Promise;
// Client: IAnthropic;

var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    Params
      .Model('claude-opus-4-7')
      .Name('Release notes drafter')
      .Description('Drafts release notes from a list of merged pull requests.')
      .System(
        'You are a senior technical writer. ' +
        'Produce concise, factual release notes grouped by area.');
  end;

// Asynchronous (promise-based)
var Promise := Client.Agents.AsyncAwaitCreate(Payload);
Promise
  .&Then(
    procedure (Value: TAgent)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);

// Synchronous
//  var Value := Client.Agents.Create(Payload);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

After creation, `Value.Id` is the agent identifier (use it for `Retrieve` / `Update` / `Archive` / `Versions` and for binding sessions). `Value.Version` starts at `1`.

<br>

## Create with model configuration

Use the object form of `Model` to set the inference `Speed` mode (`standard` or `fast`) on top of the model id.

```pascal
var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model(
           Agent.CreateModelConfig
             .Id('claude-opus-4-7')
             .Speed('fast') )
        .Name('Triage assistant')
        .System('Classify incoming support tickets.');
  end;
```

<br>

## Create with Skills

Up to 20 Skills can be attached. Use `AgentSkillParts.AddAnthropic` for Anthropic-managed Skills (e.g. `xlsx`, `pptx`, `docx`, `pdf`) and `AddCustom` for custom ones registered through the [Skills API](agent-skills-custom.md).

```pascal
var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model('claude-opus-4-7')
        .Name('Spreadsheet author')
        .System('Build Excel reports from structured input.')
        .Skills(
           AgentSkillParts
             .AddAnthropic('xlsx', 'latest')
             .AddCustom('skill_01ABCxyz', '3') );
  end;
```

Omitting the second argument lets the server resolve the most recent published version of the Skill.

<br>

## Create with built-in toolset

The built-in Agent toolset exposes `bash`, `edit`, `read`, `write`, `glob`, `grep`, `web_fetch`, and `web_search`. Configure individual tools with `Configs(...)` and the toolset-wide defaults with `DefaultConfig(...)`. Each tool config can carry an `Enabled` flag and a permission policy (`always_allow` / `always_ask`).

```pascal
var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model('claude-opus-4-7')
        .Name('Repo navigator')
        .System('Help the user explore a cloned repository.')
        .Tools(
           AgentToolParts.AddBuiltInToolset(
             Agent.CreateBuiltInToolset
               .DefaultConfig(
                  Agent.CreateToolsetDefaultConfig
                    .Enabled(True)
                    .PermissionPolicy(Agent.AlwaysAsk))
               .Configs(
                  AgentToolConfigParts
                    .AddConfig('read',  True,  Agent.AlwaysAllow)
                    .AddConfig('write', False))
           ) );
  end;
```

In this example, every built-in tool is enabled and requires confirmation by default, except `read` (auto-approved) and `write` (disabled outright).

<br>

## Create with MCP servers and MCP toolset

To expose tools served by an MCP endpoint, declare the server with `AgentMCPServerParts.AddServer(Name, Url)` and reference its name from an `mcp_toolset` entry inside `AgentToolParts.AddMCPToolset(Name)`.

```pascal
var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model('claude-opus-4-7')
        .Name('Linear triage agent')
        .System('Use Linear MCP tools to triage incoming issues.')
        .MCPServers(
           AgentMCPServerParts
             .AddServer('linear', 'https://mcp.linear.app/sse') )
        .Tools(
           AgentToolParts.AddMCPToolset('linear') );
  end;
```

The MCP server `Name` must match the name passed to `AddMCPToolset`. Tools exposed by that server can be individually overridden through the `TAgentMCPToolsetParams` overload of `AddMCPToolset` (which accepts a full `Configs([...])` block).

<br>

## Create with a custom tool

A custom tool is executed by the **client** (not by the server). The Agent declares the tool's name, description, and input schema; the client receives `tool_use` events for it during session execution and replies with `tool_result`.

The `AddCustomTool(Name, Description, InputSchema)` overload takes the schema as a raw JSON string. For a strongly typed schema, use the `TAgentCustomToolParams` overload combined with `Anthropic.Schema.TSchemaParams`.

```pascal
// uses Anthropic.Schema;

var GetWeatherSchema := TSchemaParams.New
  .&Type('object')
  .Properties( TJSONObject.Create
     .AddPair('location', TJSONObject.Create
         .AddPair('type', 'string')
         .AddPair('description', 'City and state, e.g. San Francisco, CA'))
  )
  .Required(['location']);

var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model('claude-opus-4-7')
        .Name('Weather concierge')
        .System('Answer weather questions; call get_weather when needed.')
        .Tools(
           AgentToolParts.AddCustomTool(
             Agent.CreateCustomTool
               .Name('get_weather')
               .Description('Return the current weather for a given location.')
               .InputSchema(GetWeatherSchema)
           ) );
  end;
```

<br>

## Create with multi-agent topology (coordinator)

A coordinator Agent delegates to sub-agents listed in the multiagent roster. Use `AgentRosterParts.AddAgent(Id, Version)` to reference another agent (with an optional pinned version) and `AddSelf` to include the parent agent itself.

```pascal
var Payload: TAgentCreateParamProc :=
  procedure (Params: TAgentCreateParams)
  begin
    with Generation do
      Params
        .Model('claude-opus-4-7')
        .Name('Release coordinator')
        .System('Coordinate the release notes specialists.')
        .Multiagent(
           Agent.CreateMultiagent
             .Agents(
                AgentRosterParts
                  .AddSelf
                  .AddAgent('agent_01AAA', 2)
                  .AddAgent('agent_01BBB')) );
  end;
```

Pass `Version = 0` (the default) to `AddAgent` to track the latest version of the referenced agent.

<br>

## Retrieve (latest or pinned)

```pascal
// Latest version
var Agent := Client.Agents.Retrieve('agent_01ABCxyz');
try
  Display(TutorialHub, Agent);
finally
  Agent.Free;
end;

// Pinned to a specific version
var Pinned := Client.Agents.Retrieve('agent_01ABCxyz',
  procedure (Params: TAgentRetrieveParams)
  begin
    Params.Version(2);
  end);
try
  Display(TutorialHub, Pinned);
finally
  Pinned.Free;
end;

// Asynchronous variant
var Promise := Client.Agents.AsyncAwaitRetrieve('agent_01ABCxyz');
Promise
  .&Then(
    procedure (Value: TAgent)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);
```

<br>

## List with filters and pagination

`TAgentListParams` supports time-range filtering, archived inclusion, page size, and cursor pagination via `NextPage`.

```pascal
var Page1 := Client.Agents.List(
  procedure (Params: TAgentListParams)
  begin
    Params
      .CreatedAtGte('2026-01-01T00:00:00Z')
      .IncludeArchived(False)
      .Limit(50);
  end);
try
  Display(TutorialHub, Page1);

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.Agents.List(
        procedure (Params: TAgentListParams)
        begin
          Params
            .Limit(50)
            .Page(Page1.NextPage);
        end);
      try
        Display(TutorialHub, Page2);
      finally
        Page2.Free;
      end;
    end;
finally
  Page1.Free;
end;
```

The list endpoint defaults to `Limit(20)` and `IncludeArchived(False)`. `Limit` is capped at 100.

<br>

## Update (with optimistic concurrency control)

`Update` sets the new fields **in addition to** an expected current `Version`. The server rejects the call if the agent has been modified concurrently — this prevents lost-update races.

```pascal
var Updated := Client.Agents.Update('agent_01ABCxyz',
  procedure (Params: TAgentUpdateParams)
  begin
    Params
      .Version(2)                                           // expected current version
      .System('You are now a senior staff engineer reviewer.')
      .Metadata('owner', 'team-platform');                  // upsert a single key
  end);
try
  Display(TutorialHub, Updated);   // Updated.Version = 3
finally
  Updated.Free;
end;
```

>[!IMPORTANT]
> `Version(Value)` here is **not** the new version — it is the version you believe is current. If you omit it, the server rejects the call.

<br>

## Update — clearing fields

For optional fields (description, system prompt, MCP servers, skills, tools), the wrapper exposes explicit `Clear*` helpers that send the right JSON null / empty-array semantics. They are not equivalent to omitting the field.

```pascal
var Cleared := Client.Agents.Update('agent_01ABCxyz',
  procedure (Params: TAgentUpdateParams)
  begin
    Params
      .Version(3)
      .ClearDescription          // sets description to null
      .ClearMCPServers           // sets mcp_servers to []
      .ClearSkills               // sets skills to []
      .DeleteMetadata('owner');  // drops the 'owner' metadata key
  end);
```

Available clearers: `ClearDescription`, `ClearMCPServers`, `ClearSkills`, `ClearSystem`, `ClearTools`, `DeleteMetadata(Key)`.

<br>

## Archive

Archived agents are read-only: they remain visible to historical sessions but cannot be referenced by new sessions and cannot be updated.

```pascal
var Archived := Client.Agents.Archive('agent_01ABCxyz');
try
  // Archived.ArchivedAt is now populated (RFC 3339 timestamp).
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;
```

<br>

## Versions

`Versions` returns a paginated `TAgentList` whose entries are the **distinct historical versions** of one agent, ordered most recent first.

```pascal
var Versions := Client.Agents.Versions('agent_01ABCxyz',
  procedure (Params: TAgentListParams)
  begin
    Params.Limit(20);
  end);
try
  for var V in Versions.Data do
    // V.Version, V.UpdatedAt, V.System, V.Tools, ...
    Display(TutorialHub, V);
finally
  Versions.Free;
end;
```

<br>

## Constraints & notes

- Agents are versioned. For production sessions, **pin a version** (`TSessionAgentParams.Version`) to avoid silent drift when the agent is updated.
- `Update` requires the expected current `Version` (optimistic concurrency control). On `409` errors, refetch the agent, merge your change, and retry.
- The `managed-agents-2026-04-01` beta header is set automatically by the wrapper.
- Custom tools defined on the agent are executed by the **client**, not by the server. Built-in toolset entries and MCP toolset entries are executed server-side.
- Skills attached to an agent are resolved by the server at session start.

<br>

## References

- Source unit: [`Anthropic.Agents.pas`](../source/Anthropic.Agents.pas)
- Helper unit: [`Anthropic.Helpers.pas`](../source/Anthropic.Helpers.pas) (`Generation.Agent`, `AgentSkillParts`, `AgentToolParts`, `AgentMCPServerParts`, `AgentRosterParts`, `AgentToolConfigParts`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Sessions](managed-agents-sessions.md), [Agent Skills](agent-skills.md), [Custom Skills](agent-skills-custom.md)
