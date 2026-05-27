unit Demo.Anthropic.Agent.Provisioning;

interface

uses
  System.SysUtils, System.Generics.Collections,
  Anthropic, Anthropic.Agents, Anthropic.Environment,
  Demo.Anthropic.Agent.Cards,
  Demo.Anthropic.Agent.Registry;

type
  TResolvedAgent = TAgentCloudResource;

  IAgentProvisioner = interface
    ['{8B5D9C42-7E31-4F0A-9C16-2D8E5A7B3C04}']
    /// <summary>
    /// Resolves the card definition to live server ids, creating the
    /// Environment and Agent(s). Blocking - call from a worker thread.
    /// Cached by card id, version and definition hash.
    /// </summary>
    function Resolve(const Def: TAgentCardDefinition): TResolvedAgent;
    procedure RecordSession(const Def: TAgentCardDefinition;
      const Resolved: TResolvedAgent; const SessionId, Status: string);
    function CanReuseSession(const Def: TAgentCardDefinition;
      const SessionId: string): Boolean;
    procedure UpdateSessionStatus(const SessionId, Status: string);
  end;

  TAgentProvisioner = class(TInterfacedObject, IAgentProvisioner)
  private
    FClient: IAnthropic;
    FCache: TDictionary<string, TResolvedAgent>;
    FRegistry: IAgentCloudRegistry;

    function BuildToolConfig(const Cfg: TBuiltinToolConfig): TAgentToolConfigParams;
    function HasEnabledReadTool(const Tools: TAgentToolsDef): Boolean;
    function ToolsWithReadEnabled(const Tools: TAgentToolsDef): TAgentToolsDef;
    function BuildToolParams(const Tools: TAgentToolsDef;
      const RequireReadTool: Boolean): TArray<TAgentToolParams>;

    function CacheKey(const Def: TAgentCardDefinition): string;
    function TryResolveFromRegistry(const Def: TAgentCardDefinition;
      out Resolved: TResolvedAgent): Boolean;
    function CreateEnvironment(const Def: TAgentCardDefinition;
      const RegistryEntryId: string): string;
    function CreateBasicAgent(const Def: TAgentCardDefinition;
      const RegistryEntryId, AName, AModel, ASystem, Role, Ref: string;
      const Tools: TAgentToolsDef; const ForceReadTool: Boolean):
      TAgentCloudSubAgentId;
    function CreateCoordinator(const Coord: TCoordinatorDef;
      const RefMap: TDictionary<string, string>;
      const Def: TAgentCardDefinition;
      const RegistryEntryId: string;
      const ForceReadTool: Boolean): TAgentCloudSubAgentId;
  public
    constructor Create(const AClient: IAnthropic;
      const ARegistry: IAgentCloudRegistry = nil);
    destructor Destroy; override;
    function Resolve(const Def: TAgentCardDefinition): TResolvedAgent;
    procedure RecordSession(const Def: TAgentCardDefinition;
      const Resolved: TResolvedAgent; const SessionId, Status: string);
    function CanReuseSession(const Def: TAgentCardDefinition;
      const SessionId: string): Boolean;
    procedure UpdateSessionStatus(const SessionId, Status: string);
  end;

implementation

{$REGION 'Dev note'}
(*

  Provisioning: turns a parsed agent-card definition
  (Demo.Anthropic.Agent.Cards) into live Managed Agents server ids.

  Resolve is blocking and must be called off the UI thread. Sub-agents are
  created first, their ids are injected into the coordinator roster, then the
  coordinator is created.

  The demo no longer provisions advanced extras such as persistent memory,
  package-installed environments, client-side tools or skills. The remaining
  cards need only a basic environment, built-in tool policies, optional
  sub-agents and the local-project read-tool guard.

*)
{$ENDREGION}

{ TAgentProvisioner }

constructor TAgentProvisioner.Create(const AClient: IAnthropic;
  const ARegistry: IAgentCloudRegistry);
begin
  inherited Create;
  FClient := AClient;
  FRegistry := ARegistry;
  FCache := TDictionary<string, TResolvedAgent>.Create;
end;

destructor TAgentProvisioner.Destroy;
begin
  FCache.Free;
  inherited;
end;

function TAgentProvisioner.CacheKey(const Def: TAgentCardDefinition): string;
begin
  Result := Def.CardId + '|' + Def.Version + '|' + Def.DefinitionHash;
end;

function TAgentProvisioner.BuildToolConfig(
  const Cfg: TBuiltinToolConfig): TAgentToolConfigParams;
begin
  Result := TAgentToolConfigParams.New
    .Name(Cfg.Name)
    .Enabled(Cfg.Enabled);

  if SameText(Cfg.Policy, 'always_ask') then
    Result.PermissionPolicy(TAgentAlwaysAskPolicyParams.New)
  else
  if SameText(Cfg.Policy, 'always_allow') then
    Result.PermissionPolicy(TAgentAlwaysAllowPolicyParams.New);
end;

function TAgentProvisioner.HasEnabledReadTool(
  const Tools: TAgentToolsDef): Boolean;
begin
  Result := False;
  if not Tools.Builtin.Defined then
    Exit;

  for var Cfg in Tools.Builtin.Configs do
    if SameText(Cfg.Name, 'read') then
      Exit(Cfg.Enabled);

  Result := Tools.Builtin.DefaultEnabled;
end;

function TAgentProvisioner.ToolsWithReadEnabled(
  const Tools: TAgentToolsDef): TAgentToolsDef;
begin
  Result := Tools;
  if HasEnabledReadTool(Result) then
    Exit;

  Result.Builtin.Defined := True;
  if Result.Builtin.DefaultPolicy.Trim.IsEmpty then
    Result.Builtin.DefaultPolicy := 'always_ask';

  var Configs: TArray<TBuiltinToolConfig> := [];
  for var ExistingCfg in Result.Builtin.Configs do
    begin
      var UpdatedCfg := ExistingCfg;
      if SameText(UpdatedCfg.Name, 'read') then
        begin
          UpdatedCfg.Enabled := True;
          UpdatedCfg.Policy := 'always_allow';
        end;
      Configs := Configs + [UpdatedCfg];
    end;

  for var FinalCfg in Configs do
    if SameText(FinalCfg.Name, 'read') then
      begin
        Result.Builtin.Configs := Configs;
        Exit;
      end;

  var ReadCfg := Default(TBuiltinToolConfig);
  ReadCfg.Name := 'read';
  ReadCfg.Enabled := True;
  ReadCfg.Policy := 'always_allow';
  Result.Builtin.Configs := Configs + [ReadCfg];
end;

function TAgentProvisioner.BuildToolParams(const Tools: TAgentToolsDef;
  const RequireReadTool: Boolean): TArray<TAgentToolParams>;
begin
  Result := [];

  var EffectiveTools := Tools;
  if RequireReadTool then
    EffectiveTools := ToolsWithReadEnabled(EffectiveTools);

  if EffectiveTools.Builtin.Defined then
    begin
      var Toolset := TAgentBuiltInToolsetParams.New;

      var DefaultCfg := TAgentToolsetDefaultConfigParams.New
        .Enabled(EffectiveTools.Builtin.DefaultEnabled);

      if SameText(EffectiveTools.Builtin.DefaultPolicy, 'always_ask') then
        DefaultCfg.PermissionPolicy(TAgentAlwaysAskPolicyParams.New)
      else
      if SameText(EffectiveTools.Builtin.DefaultPolicy, 'always_allow') then
        DefaultCfg.PermissionPolicy(TAgentAlwaysAllowPolicyParams.New);
      Toolset.DefaultConfig(DefaultCfg);

      var Configs: TArray<TAgentToolConfigParams> := [];
      for var Cfg in EffectiveTools.Builtin.Configs do
        Configs := Configs + [BuildToolConfig(Cfg)];
      if Length(Configs) > 0 then
        Toolset.Configs(Configs);

      var ToolsetEntry: TAgentToolParams := Toolset;
      Result := Result + [ToolsetEntry];
    end;
end;

function TAgentProvisioner.CreateEnvironment(const Def: TAgentCardDefinition;
  const RegistryEntryId: string): string;
begin
  var Created := FClient.Environments.Create(
    procedure (Params: TEnvironmentCreateParams)
    begin
      if Def.Environment.Name.Trim.IsEmpty then
        Params.Name('pythia-agent-environment')
      else
        Params.Name(Def.Environment.Name);

      if not Def.Environment.Description.Trim.IsEmpty then
        Params.Description(Def.Environment.Description);

      var Metadata := TAgentCloudMetadata.Build(
        Def, RegistryEntryId, 'environment', 'environment', 'environment');
      try
        Params.Metadata(Metadata);
      finally
        Metadata.Free;
      end;
    end);
  try
    Result := Created.Id;
  finally
    Created.Free;
  end;
end;

function TAgentProvisioner.CreateBasicAgent(const Def: TAgentCardDefinition;
  const RegistryEntryId, AName, AModel, ASystem, Role, Ref: string;
  const Tools: TAgentToolsDef; const ForceReadTool: Boolean):
  TAgentCloudSubAgentId;
begin
  Result := Default(TAgentCloudSubAgentId);

  var Created := FClient.Agents.Create(
    procedure (Params: TAgentCreateParams)
    begin
      Params.Model(AModel).Name(AName);
      if not ASystem.Trim.IsEmpty then
        Params.System(ASystem);

      var ToolParams := BuildToolParams(Tools, ForceReadTool);
      if Length(ToolParams) > 0 then
        Params.Tools(ToolParams);

      var Metadata := TAgentCloudMetadata.Build(
        Def, RegistryEntryId, 'agent', Role, Ref);
      try
        Params.Metadata(Metadata);
      finally
        Metadata.Free;
      end;
    end);
  try
    Result.AgentId := Created.Id;
    Result.CloudVersion := Created.Version;
    Result.Ref := Ref;
    Result.Status := 'active';
  finally
    Created.Free;
  end;
end;

function TAgentProvisioner.CreateCoordinator(const Coord: TCoordinatorDef;
  const RefMap: TDictionary<string, string>;
  const Def: TAgentCardDefinition; const RegistryEntryId: string;
  const ForceReadTool: Boolean): TAgentCloudSubAgentId;
begin
  Result := Default(TAgentCloudSubAgentId);

  var Created := FClient.Agents.Create(
    procedure (Params: TAgentCreateParams)
    begin
      Params.Model(Coord.Model).Name(Coord.Name);
      if not Coord.System.Trim.IsEmpty then
        Params.System(Coord.System);

      {--- The coordinator is the agent bound to the session. When the session
           mounts "file" resources it must expose a usable read tool, even if
           the card left the coordinator tool-less because it normally
           delegates. }
      var ToolParams := BuildToolParams(Coord.Tools, ForceReadTool);
      if Length(ToolParams) > 0 then
        Params.Tools(ToolParams);

      var Roster: TArray<TAgentRosterEntryParams> := [];
      for var Entry in Coord.Roster do
        if SameText(Entry, 'self') then
          begin
            var SelfRef: TAgentRosterEntryParams := TAgentSelfReferenceParams.New;
            Roster := Roster + [SelfRef];
          end
        else
          begin
            var SubId: string;
            if RefMap.TryGetValue(Entry, SubId) then
              begin
                var AgentRef: TAgentRosterEntryParams :=
                  TAgentReferenceParams.New.Id(SubId);
                Roster := Roster + [AgentRef];
              end;
          end;

      if Length(Roster) > 0 then
        Params.Multiagent(TAgentMultiagentParams.New.Agents(Roster));

      var Metadata := TAgentCloudMetadata.Build(
        Def, RegistryEntryId, 'agent', 'coordinator', 'coordinator');
      try
        Params.Metadata(Metadata);
      finally
        Metadata.Free;
      end;
    end);
  try
    Result.AgentId := Created.Id;
    Result.CloudVersion := Created.Version;
    Result.Ref := 'coordinator';
    Result.Status := 'active';
  finally
    Created.Free;
  end;
end;

function TAgentProvisioner.TryResolveFromRegistry(
  const Def: TAgentCardDefinition; out Resolved: TResolvedAgent): Boolean;
begin
  Result := False;
  Resolved := Default(TResolvedAgent);

  if FRegistry = nil then
    Exit;

  if not FRegistry.TryFindActive(Def, Resolved) then
    Exit;

  try
    var Env := FClient.Environments.Retrieve(Resolved.EnvironmentId);
    try
      if not Env.ArchivedAt.Trim.IsEmpty then
        begin
          FRegistry.MarkEntryStatus(Resolved.RegistryEntryId, 'missing');
          Exit;
        end;
    finally
      Env.Free;
    end;

    var Agent := FClient.Agents.Retrieve(Resolved.AgentId);
    try
      if not Agent.ArchivedAt.Trim.IsEmpty then
        begin
          FRegistry.MarkEntryStatus(Resolved.RegistryEntryId, 'missing');
          Exit;
        end;
    finally
      Agent.Free;
    end;

    for var Sub in Resolved.SubAgents do
      begin
        var SubAgent := FClient.Agents.Retrieve(Sub.AgentId);
        try
          if not SubAgent.ArchivedAt.Trim.IsEmpty then
            begin
              FRegistry.MarkEntryStatus(Resolved.RegistryEntryId, 'missing');
              Exit;
            end;
        finally
          SubAgent.Free;
        end;
      end;

    Result := True;
    FRegistry.TouchActive(Def);
  except
    on E: Exception do
      begin
        if not Resolved.RegistryEntryId.Trim.IsEmpty then
          FRegistry.MarkEntryStatus(Resolved.RegistryEntryId, 'missing');
        Resolved := Default(TResolvedAgent);
        Result := False;
      end;
  end;
end;

function TAgentProvisioner.Resolve(
  const Def: TAgentCardDefinition): TResolvedAgent;
begin
  var Key := CacheKey(Def);
  if FCache.TryGetValue(Key, Result) then
    Exit;

  Result := Default(TResolvedAgent);

  if TryResolveFromRegistry(Def, Result) then
    begin
      FCache.AddOrSetValue(Key, Result);
      Exit;
    end;

  if FRegistry <> nil then
    FRegistry.RetireMismatched(Def);

  Result.RegistryEntryId := TAgentCloudIds.NewClsid;
  Result.EnvironmentId := CreateEnvironment(Def, Result.RegistryEntryId);
  Result.EnvironmentStatus := 'active';

  case Def.Kind of
    ackSingle:
      begin
        var Created := CreateBasicAgent(
          Def, Result.RegistryEntryId,
          Def.Agent.Name, Def.Agent.Model, Def.Agent.System,
          'agent', 'self',
          Def.Agent.Tools, Def.Session.Folder.Defined);
        Result.AgentId := Created.AgentId;
        Result.AgentVersion := Created.CloudVersion;
        Result.AgentStatus := 'active';
      end;

    ackMultiagent:
      begin
        var RefMap := TDictionary<string, string>.Create;
        try
          for var Sub in Def.SubAgents do
            begin
              var CreatedSub := CreateBasicAgent(
                Def, Result.RegistryEntryId,
                Sub.Name, Sub.Model, Sub.System,
                'subagent', Sub.Ref,
                Sub.Tools, False);
              Result.SubAgents := Result.SubAgents + [CreatedSub];
              RefMap.AddOrSetValue(Sub.Ref, CreatedSub.AgentId);
            end;

          var CreatedCoord := CreateCoordinator(
            Def.Coordinator, RefMap, Def, Result.RegistryEntryId,
            Def.Session.Folder.Defined);
          Result.AgentId := CreatedCoord.AgentId;
          Result.AgentVersion := CreatedCoord.CloudVersion;
          Result.AgentStatus := 'active';
        finally
          RefMap.Free;
        end;
      end;
  end;

  Result.Valid := True;
  if FRegistry <> nil then
    FRegistry.SaveActive(Def, Result);
  FCache.AddOrSetValue(Key, Result);
end;

procedure TAgentProvisioner.RecordSession(const Def: TAgentCardDefinition;
  const Resolved: TResolvedAgent; const SessionId, Status: string);
begin
  if FRegistry <> nil then
    FRegistry.RecordSession(Def, Resolved, SessionId, Status);
end;

function TAgentProvisioner.CanReuseSession(const Def: TAgentCardDefinition;
  const SessionId: string): Boolean;
begin
  if SessionId.Trim.IsEmpty then
    Exit(False);

  if FRegistry = nil then
    Exit(True);

  Result := FRegistry.HasSession(Def, SessionId);
end;

procedure TAgentProvisioner.UpdateSessionStatus(const SessionId,
  Status: string);
begin
  if FRegistry <> nil then
    FRegistry.MarkSessionStatus(SessionId, Status);
end;

end.
