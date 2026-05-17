# Environments [beta]

An **Environment** describes the execution container in which a Session runs. It is the server-side template that controls the cloud configuration (networking policy, pre-installed packages) and the human-readable metadata; sessions reference an environment by id at creation time.

- [Overview](#overview)
- [Minimal create](#minimal-create)
- [Create with unrestricted network](#create-with-unrestricted-network)
- [Create with limited network](#create-with-limited-network)
- [Create with pre-installed packages](#create-with-pre-installed-packages)
- [Create with metadata](#create-with-metadata)
- [Retrieve](#retrieve)
- [List with filters and pagination](#list-with-filters-and-pagination)
- [Update — patch fields](#update--patch-fields)
- [Update — clearing fields](#update--clearing-fields)
- [Update — networking and packages](#update--networking-and-packages)
- [Archive](#archive)
- [Delete](#delete)
- [Constraints & notes](#constraints--notes)
- [References](#references)

___

<br>

## Overview

The wrapper exposes the Environments API through `Client.Environments`, returning typed `TEnvironment` / `TEnvironmentList` instances. Every operation has both a blocking variant and a promise-based variant (`AsyncAwait*`).

An environment is **independent** from any single session: it is a reusable template. Multiple sessions can run against the same environment, and the same agent can be paired with different environments.

Two construction styles are supported and interchangeable:

- **Direct fluent** — `TEnvironmentXxxParams.New.Field(...)`.
- **Via the `Generation` helper** (`Anthropic.Helpers`) — `Generation.Environment.CreateXxx.Field(...)`. The helper exposes `CreateParams`, `UpdateParams`, `ListParams`, `CreateCloudConfig`, `CreateUnrestrictedNetwork`, `CreateLimitedNetwork`, and `CreatePackages`.

The examples below use the `Generation` style for readability.

>[!IMPORTANT]
> The `managed-agents-2026-04-01` beta header is set automatically by the wrapper when any `Client.Environments.*` route is called.

<br>

## Minimal create

The only required field is `Name`. Everything else (cloud config, description, metadata) is optional and may be added later via `Update`.

```pascal
// uses Anthropic, Anthropic.Types, Anthropic.Helpers,
//      Anthropic.Environment, Anthropic.Async.Promise;
// Client: IAnthropic;

var Payload: TEnvironmentCreateParamProc :=
  procedure (Params: TEnvironmentCreateParams)
  begin
    Params
      .Name('Default cloud sandbox')
      .Description('Baseline environment for release-notes sessions.');
  end;

// Asynchronous (promise-based)
var Promise := Client.Environments.AsyncAwaitCreate(Payload);
Promise
  .&Then(
    procedure (Value: TEnvironment)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);

// Synchronous
//  var Value := Client.Environments.Create(Payload);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
```

After creation, `Value.Id` is the environment identifier (use it for `Retrieve` / `Update` / `Archive` / `Delete` and when starting a session). `Value.CreatedAt` and `Value.UpdatedAt` are RFC 3339 timestamps.

<br>

## Create with unrestricted network

The cloud configuration is attached via `Config(...)`. To allow the container to reach **any** host, build a networking block with `CreateUnrestrictedNetwork`.

```pascal
var Payload: TEnvironmentCreateParamProc :=
  procedure (Params: TEnvironmentCreateParams)
  begin
    with Generation do
      Params
        .Name('Open-internet sandbox')
        .Description('Full outbound network — only for trusted agents.')
        .Config(
           Environment.CreateCloudConfig
             .Networking( Environment.CreateUnrestrictedNetwork ) );
  end;
```

`Value.Config.Networking.IsUnrestricted` returns `True` on the resolved object once the API responds.

<br>

## Create with limited network

A limited policy is the recommended default. It exposes three switches:

- `AllowedHosts([...])` — explicit list of hosts the container may reach.
- `AllowMCPServers(True)` — allow outbound calls to the MCP servers declared on the agent.
- `AllowPackageManagers(True)` — allow access to public registries (PyPI, npm, …).

All three default to absent / `False` when omitted.

```pascal
var Payload: TEnvironmentCreateParamProc :=
  procedure (Params: TEnvironmentCreateParams)
  begin
    with Generation do
      Params
        .Name('Triage sandbox')
        .Description('Restricted egress for the triage coordinator.')
        .Config(
           Environment.CreateCloudConfig
             .Networking(
                Environment.CreateLimitedNetwork
                  .AllowMCPServers(True)
                  .AllowPackageManagers(False)
                  .AllowedHosts([
                    'api.github.com',
                    'raw.githubusercontent.com',
                    'mcp.linear.app']) ) );
  end;
```

On the resolved response, `Value.Config.Networking.IsLimited` is `True` and `AllowedHosts` exposes the persisted list.

<br>

## Create with pre-installed packages

Use `CreatePackages` to declare packages installed when a session boots. The wrapper supports six managers — `Apt`, `Cargo`, `Gem`, `Go`, `Npm`, `Pip` — each accepting a `TArray<string>`. Version pinning follows the manager's native semantics (for example `package==1.0.0` for pip).

```pascal
var Payload: TEnvironmentCreateParamProc :=
  procedure (Params: TEnvironmentCreateParams)
  begin
    with Generation do
      Params
        .Name('Data-science sandbox')
        .Description('Python + system tooling for notebook-style sessions.')
        .Config(
           Environment.CreateCloudConfig
             .Networking(
                Environment.CreateLimitedNetwork
                  .AllowPackageManagers(True))
             .Packages(
                Environment.CreatePackages
                  .Apt(['ripgrep', 'jq'])
                  .Pip(['pandas==2.2.2', 'pyarrow>=15', 'duckdb'])
                  .Npm(['typescript@5.4.5'])) );
  end;
```

>[!NOTE]
> `AllowPackageManagers(True)` is required for the install step itself when the limited policy is in effect; otherwise package fetches are blocked at runtime.

<br>

## Create with metadata

Metadata is a free-form JSON object. The wrapper exposes two ergonomic overloads of `Metadata`:

- `Metadata(Key, Value)` — upsert a single key (call repeatedly to add several).
- `Metadata(JSONObject)` — replace the metadata object in one shot.

```pascal
var Payload: TEnvironmentCreateParamProc :=
  procedure (Params: TEnvironmentCreateParams)
  begin
    Params
      .Name('Team-platform sandbox')
      .Metadata('owner', 'team-platform')
      .Metadata('cost-center', 'CC-4221')
      .Metadata('tier', 'production');
  end;
```

The resolved `TEnvironment.Metadata` returns the raw JSON text of the persisted object, which you can parse with `TJSONObject.ParseJSONValue` for inspection.

<br>

## Retrieve

```pascal
// Synchronous
var Env := Client.Environments.Retrieve('env_01ABCxyz');
try
  Display(TutorialHub, Env);
finally
  Env.Free;
end;

// Asynchronous variant
var Promise := Client.Environments.AsyncAwaitRetrieve('env_01ABCxyz');
Promise
  .&Then(
    procedure (Value: TEnvironment)
    begin
      Display(TutorialHub, Value);
    end)
  .&Catch(
    procedure (E: Exception)
    begin
      Display(TutorialHub, E.Message);
    end);
```

`Env.ArchivedAt` is non-empty when the environment has been archived.

<br>

## List with filters and pagination

`TEnvironmentListParams` supports cursor pagination via `Page(...)` and archived inclusion via `IncludeArchived`. `Limit` is capped at 100 (default 20).

```pascal
var Page1 := Client.Environments.List(
  procedure (Params: TEnvironmentListParams)
  begin
    Params
      .IncludeArchived(False)
      .Limit(50);
  end);
try
  Display(TutorialHub, Page1);

  if Page1.NextPage <> '' then
    begin
      var Page2 := Client.Environments.List(
        procedure (Params: TEnvironmentListParams)
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

// Unfiltered shortcut
//  var All := Client.Environments.List;
```

<br>

## Update — patch fields

`Update` is a sparse PATCH: only the fields you set are sent. Anything you omit is left untouched on the server.

```pascal
var Updated := Client.Environments.Update('env_01ABCxyz',
  procedure (Params: TEnvironmentUpdateParams)
  begin
    Params
      .Name('Triage sandbox (v2)')
      .Description('Adjusted allowed-hosts list after the Linear migration.')
      .Metadata('owner', 'team-platform');   // upsert a single key
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

<br>

## Update — clearing fields

For optional fields, the wrapper exposes explicit clearers that send the right JSON null / empty-collection semantics. They are **not** equivalent to omitting the field.

```pascal
var Cleared := Client.Environments.Update('env_01ABCxyz',
  procedure (Params: TEnvironmentUpdateParams)
  begin
    Params
      .ClearDescription            // sets description to null
      .DeleteMetadata('cost-center');  // drops a single metadata key
  end);
```

Available clearers on `TEnvironmentUpdateParams`:

| Method                                    | Effect                                            |
|-------------------------------------------|---------------------------------------------------|
| `ClearDescription`                        | Sends `description: null`.                        |
| `DeleteMetadata(Key)`                     | Drops one metadata key (sends `null` for it).     |
| `DeleteMetadataWithEmptyString(Key)`      | Drops one metadata key by sending an empty string.|

Inside a `Networking` block (limited policy), `ClearAllowedHosts` sends an empty `allowed_hosts` array.

<br>

## Update — networking and packages

The `Config(...)` setter on `TEnvironmentUpdateParams` patches the cloud configuration. Omitted nested fields preserve the existing server-side values, so you can update only the networking policy without re-sending the package list (and vice versa).

```pascal
// Switch the network policy to "limited" and broaden the allowed-hosts list,
// without touching the package configuration.
var Updated := Client.Environments.Update('env_01ABCxyz',
  procedure (Params: TEnvironmentUpdateParams)
  begin
    with Generation do
      Params
        .Config(
           Environment.CreateCloudConfig
             .Networking(
                Environment.CreateLimitedNetwork
                  .AllowMCPServers(True)
                  .AllowPackageManagers(True)
                  .AllowedHosts([
                    'api.github.com',
                    'raw.githubusercontent.com',
                    'pypi.org',
                    'files.pythonhosted.org',
                    'registry.npmjs.org'])) );
  end);
try
  Display(TutorialHub, Updated);
finally
  Updated.Free;
end;
```

```pascal
// Replace the pip package list — leaves networking untouched.
var Updated := Client.Environments.Update('env_01ABCxyz',
  procedure (Params: TEnvironmentUpdateParams)
  begin
    with Generation do
      Params
        .Config(
           Environment.CreateCloudConfig
             .Packages(
                Environment.CreatePackages
                  .Pip(['pandas==2.2.2', 'polars==0.20.31', 'duckdb'])) );
  end);
```

`TEnvironmentPackagesParams` also exposes `ClearApt`, `ClearCargo`, `ClearGem`, `ClearGo`, `ClearNpm`, `ClearPip` to send an empty array for a given manager (useful for removing every previously installed package of that type without touching the others).

<br>

## Archive

Archived environments are read-only: they remain visible to historical sessions but cannot be referenced by new sessions and cannot be updated. Prefer `Archive` over `Delete` whenever the environment has been used by past sessions.

```pascal
var Archived := Client.Environments.Archive('env_01ABCxyz');
try
  // Archived.ArchivedAt is now populated (RFC 3339 timestamp).
  Display(TutorialHub, Archived);
finally
  Archived.Free;
end;
```

<br>

## Delete

`Delete` is a hard delete and only succeeds when no active session references the environment. The response carries the deleted id and a `type` field equal to `environment_deleted`.

```pascal
var Deleted := Client.Environments.Delete('env_01ABCxyz');
try
  // Deleted.Id, Deleted.&Type
  Display(TutorialHub, Deleted);
finally
  Deleted.Free;
end;
```

<br>

## Constraints & notes

- An environment in use by an active session cannot be deleted — archive it instead.
- `Update` is a sparse PATCH: omit fields you want to keep, use the explicit `Clear*` / `Delete*` helpers when you actually want to remove a value.
- Networking policies are mutually exclusive: a single `Networking(...)` call carries either an unrestricted **or** a limited policy, never both.
- For a limited policy, `AllowMCPServers` and `AllowPackageManagers` default to `False` and `AllowedHosts` defaults to empty; outbound traffic to anything outside that scope is blocked at runtime.
- Long-running session resources (mounted repositories, memory stores) are attached at **session** creation, not at environment creation — see the [Sessions](managed-agents-sessions.md) guide.
- The `managed-agents-2026-04-01` beta header is set automatically by the wrapper.

<br>

## References

- Source unit: [`Anthropic.Environment.pas`](../source/Anthropic.Environment.pas)
- Helper unit: [`Anthropic.Helpers.pas`](../source/Anthropic.Helpers.pas) (`Generation.Environment` — `CreateParams`, `UpdateParams`, `ListParams`, `CreateCloudConfig`, `CreateUnrestrictedNetwork`, `CreateLimitedNetwork`, `CreatePackages`)
- Companion guides: [Managed Agents overview](managed-agents.md), [Agents](managed-agents-agents.md), [Sessions](managed-agents-sessions.md)
