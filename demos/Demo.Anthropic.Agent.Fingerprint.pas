unit Demo.Anthropic.Agent.Fingerprint;

interface

uses
  Demo.Anthropic.Agent.Cards;

type
  TAgentDefinitionFingerprint = record
  private
    class function CanonicalJson(const Def: TAgentCardDefinition): string; static;
  public
    class function ComputeHash(const Def: TAgentCardDefinition): string; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Agent definition fingerprinting for the pythia-anthropic VCL demo.

  The registry reuses cloud Managed Agents only when the selected card has the
  same identity, version and effective definition. This unit produces the
  effective-definition part of that key: a deterministic SHA-256 hash over a
  stable JSON projection of TAgentCardDefinition.

  The hash deliberately includes the fields that affect cloud provisioning:
  card id, version, kind, environment, agent/coordinator/sub-agent prompts,
  model names, roster and built-in tool policy. Session runtime details such
  as the current session id, cloud ids or registry timestamps are not part of
  the fingerprint.

  Field order matters because the canonical JSON is hashed as text. Builders
  in this unit must therefore keep writing properties in a stable order, and
  new provisioning-relevant fields should be added explicitly instead of
  relying on generic RTTI or object enumeration.

*)
{$ENDREGION}

uses
  System.SysUtils, System.Hash,
  WVPythia.JSON.SafeWriter;

type
  TAgentDefinitionCanonicalJson = record
  private
    class function BoolText(const Value: Boolean): string; static;
    class function KindText(const Kind: TAgentCardKind): string; static;
    class function ToolsJson(const Tools: TAgentToolsDef): string; static;
    class function AgentJson(const Name, Model, SystemPrompt: string;
      const Tools: TAgentToolsDef): string; static;
    class function StringArrayJson(const Values: TArray<string>): string; static;
  public
    class function Build(const Def: TAgentCardDefinition): string; static;
  end;

{ TAgentDefinitionCanonicalJson }

class function TAgentDefinitionCanonicalJson.BoolText(
  const Value: Boolean): string;
begin
  if Value then
    Result := 'true'
  else
    Result := 'false';
end;

class function TAgentDefinitionCanonicalJson.KindText(
  const Kind: TAgentCardKind): string;
begin
  case Kind of
    ackSingle:
      Result := 'single';

    ackMultiagent:
      Result := 'multiagent';
  else
    Result := 'unknown';
  end;
end;

class function TAgentDefinitionCanonicalJson.ToolsJson(
  const Tools: TAgentToolsDef): string;
begin
  var W := TJsonWriter.NewObject;
  W.SetString('builtin_defined', BoolText(Tools.Builtin.Defined));
  W.SetString('builtin_default_enabled',
    BoolText(Tools.Builtin.DefaultEnabled));
  W.SetString('builtin_default_policy', Tools.Builtin.DefaultPolicy);

  var Configs := TJsonWriter.NewArray;
  for var Cfg in Tools.Builtin.Configs do
    begin
      var Item := TJsonWriter.NewObject;
      Item.SetString('name', Cfg.Name);
      Item.SetString('enabled', BoolText(Cfg.Enabled));
      Item.SetString('policy', Cfg.Policy);
      Configs.AppendObjectJson('', Item.ToJson);
    end;
  W.SetArrayJson('builtin_configs', Configs.ToJson);

  Result := W.ToJson;
end;

class function TAgentDefinitionCanonicalJson.AgentJson(
  const Name, Model, SystemPrompt: string;
  const Tools: TAgentToolsDef): string;
begin
  var W := TJsonWriter.NewObject;
  W.SetString('name', Name);
  W.SetString('model', Model);
  W.SetString('system', SystemPrompt);
  W.SetObjectJson('tools', ToolsJson(Tools));
  Result := W.ToJson;
end;

class function TAgentDefinitionCanonicalJson.StringArrayJson(
  const Values: TArray<string>): string;
begin
  var W := TJsonWriter.NewArray;
  for var Value in Values do
    W.AppendString('', Value);

  Result := W.ToJson;
end;

class function TAgentDefinitionCanonicalJson.Build(
  const Def: TAgentCardDefinition): string;
begin
  var Root := TJsonWriter.NewObject;
  Root.SetString('card_id', Def.CardId);
  Root.SetString('version', Def.Version);
  Root.SetString('kind', KindText(Def.Kind));

  var Env := TJsonWriter.NewObject;
  Env.SetString('name', Def.Environment.Name);
  Env.SetString('description', Def.Environment.Description);
  Root.SetObjectJson('environment', Env.ToJson);

  case Def.Kind of
    ackSingle:
      Root.SetObjectJson('agent', AgentJson(
        Def.Agent.Name,
        Def.Agent.Model,
        Def.Agent.System,
        Def.Agent.Tools));

    ackMultiagent:
      begin
        var Coord := TJsonWriter.Parse(AgentJson(
          Def.Coordinator.Name,
          Def.Coordinator.Model,
          Def.Coordinator.System,
          Def.Coordinator.Tools));
        Coord.SetArrayJson('roster', StringArrayJson(Def.Coordinator.Roster));
        Root.SetObjectJson('coordinator', Coord.ToJson);

        var Subs := TJsonWriter.NewArray;
        for var Sub in Def.SubAgents do
          begin
            var Item := TJsonWriter.Parse(AgentJson(
              Sub.Name,
              Sub.Model,
              Sub.System,
              Sub.Tools));
            Item.SetString('ref', Sub.Ref);
            Subs.AppendObjectJson('', Item.ToJson);
          end;
        Root.SetArrayJson('subagents', Subs.ToJson);
      end;
  end;

  Result := Root.ToJson;
end;

{ TAgentDefinitionFingerprint }

class function TAgentDefinitionFingerprint.CanonicalJson(
  const Def: TAgentCardDefinition): string;
begin
  Result := TAgentDefinitionCanonicalJson.Build(Def);
end;

class function TAgentDefinitionFingerprint.ComputeHash(
  const Def: TAgentCardDefinition): string;
begin
  Result := 'sha256:' +
    THashSHA2.GetHashString(CanonicalJson(Def)).ToLowerInvariant;
end;

end.
