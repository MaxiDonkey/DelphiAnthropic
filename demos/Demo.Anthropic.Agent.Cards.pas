unit Demo.Anthropic.Agent.Cards;

interface

uses
  System.SysUtils, System.JSON,
  WVPythia.JSON.SafeReader;

type
  TAgentCardKind = (ackUnknown, ackSingle, ackMultiagent);

  TEnvironmentDef = record
    Name: string;
    Description: string;
  end;

  TBuiltinToolConfig = record
    {--- read | write | bash | glob | grep | web_search | web_fetch | edit }
    Name: string;
    Enabled: Boolean;
    {--- 'always_allow' | 'always_ask' }
    Policy: string;
  end;

  TBuiltinToolsetDef = record
    Defined: Boolean;
    DefaultEnabled: Boolean;
    DefaultPolicy: string;
    Configs: TArray<TBuiltinToolConfig>;
  end;

  TAgentToolsDef = record
    Builtin: TBuiltinToolsetDef;
  end;

  TAgentDef = record
    Name: string;
    Model: string;
    System: string;
    Tools: TAgentToolsDef;
  end;

  TSubAgentDef = record
    {--- local reference used by the coordinator roster }
    Ref: string;
    Name: string;
    Model: string;
    System: string;
    Tools: TAgentToolsDef;
  end;

  TCoordinatorDef = record
    Name: string;
    Model: string;
    System: string;
    Tools: TAgentToolsDef;
    {--- 'self' or sub-agent Ref names }
    Roster: TArray<string>;
  end;

  TFolderResourceDef = record
    {--- True when the selected project must be mounted for the session. }
    Defined: Boolean;
    {--- selected project folder path, resolved from the input bubble state. }
    Path: string;
  end;

  TSessionDef = record
    Title: string;
    Folder: TFolderResourceDef;
  end;

  TAgentCardDefinition = record
    Valid: Boolean;
    CardId: string;
    Version: string;
    DefinitionHash: string;
    Kind: TAgentCardKind;
    Environment: TEnvironmentDef;
    {--- populated when Kind = ackSingle }
    Agent: TAgentDef;
    {--- populated when Kind = ackMultiagent }
    SubAgents: TArray<TSubAgentDef>;
    {--- populated when Kind = ackMultiagent }
    Coordinator: TCoordinatorDef;
    Session: TSessionDef;
  end;

  TAgentCardReader = record
  private
    class function ParseKind(const Value: string): TAgentCardKind; static;
    class function ParseEnvironment(const Reader: TJsonReader): TEnvironmentDef; static;
    class function ParseTools(const Reader: TJsonReader): TAgentToolsDef; static;
    class function ParseSession(const Reader: TJsonReader): TSessionDef; static;
    class function FindCardContent(const Cards: TJsonReader; const CardId: string): string; static;
    class function FindCardMarkdownPath(const Cards: TJsonReader; const CardId: string): string; static;
    class function FindCardVersion(const Cards: TJsonReader; const CardId: string): string; static;
    class function TryReadContent(const ContentJson, CardId,
      EnvelopeVersion: string;
      out Def: TAgentCardDefinition): Boolean; static;
  public
    /// <summary>
    /// Locates the card identified by <c>CardId</c> inside the agent-cards JSON
    /// document and parses its inline <c>content</c> payload or external
    /// <c>md_path</c> Markdown definition into <c>Def</c>.
    /// Returns False when the card is missing or its definition is invalid.
    /// </summary>
    class function TryRead(const CardsJson, CardId: string;
      out Def: TAgentCardDefinition): Boolean; overload; static;

    /// <summary>
    /// Same as the two-argument overload, with <c>CardsFolder</c> used as the
    /// base directory for card entries whose definition is externalized in
    /// <c>md_path</c>.
    /// </summary>
    class function TryRead(const CardsJson, CardId, CardsFolder: string;
      out Def: TAgentCardDefinition): Boolean; overload; static;

    /// <summary>
    /// Reads the top-level <c>name</c> field of the card identified by
    /// <c>CardId</c> — the human-readable label shown in the cards menu and
    /// echoed in the input bubble chip. Returns False when the card is
    /// missing or carries no name. Does not parse the inner content payload.
    /// </summary>
    class function TryGetCardLabel(const CardsJson, CardId: string;
      out Name: string): Boolean; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Agent-card definition parsing for the pythia-anthropic VCL demo.

  The demo now ships five deliberately small cards:
  - one single web-research agent;
  - two local-project multi-agent samples;
  - two Markdown-backed code patch / sandbox-edit agents.

  This unit stays intentionally narrow. It parses the environment, agent
  topology, built-in tool policy and session title. The local project folder is
  no longer read from the card: it is resolved from the project selected in the
  Pythia input bubble. The removed advanced sample used extra resources such as
  repository mounts, persistent memory, outcome rubrics, client-side tools and
  package provisioning; those shapes are no longer part of this demo reader.

  Card envelope:

    { "id","name","commentaire","badge",
      "content": "<escaped JSON payload below>" }

  Markdown-backed card envelope:

    { "id","name","commentaire","badge",
      "md_path": "..\\safe-code-patch-agent.md" }

  "content" payload:
    {
      "kind": "single" | "multiagent",
      "environment": { "name","description" },
      "agent"?:       { "name","model","system","tools" },
      "subagents"?:   [ { "ref","name","model","system","tools" } ],
      "coordinator"?: { "name","model","system","tools","roster":["self",...] },
      "session":      { "title" }
    }

  tools:   { "builtin"? }
  builtin: { "default":{"enabled","policy"}, "configs":[{"name","enabled","policy"}] }

*)
{$ENDREGION}

uses
  System.IOUtils,
  Demo.Anthropic.Agent.Markdown,
  Demo.Anthropic.Agent.Fingerprint;

{ TAgentCardReader }

class function TAgentCardReader.ParseKind(const Value: string): TAgentCardKind;
begin
  if SameText(Value, 'single') then
    Exit(ackSingle);

  if SameText(Value, 'multiagent') then
    Exit(ackMultiagent);

  Result := ackUnknown;
end;

class function TAgentCardReader.FindCardContent(const Cards: TJsonReader;
  const CardId: string): string;
begin
  {--- The card "content" is a JSON value stored as an escaped string. }
  Result := '';

  var Total := Cards.Count('cards');
  for var I := 0 to Total - 1 do
    if SameText(Cards.AsString(Format('cards[%d].id', [I])), CardId) then
      begin
        Result := Cards.ExtractSubJson(Format('cards[%d].content', [I]));
        Exit;
      end;
end;

class function TAgentCardReader.FindCardMarkdownPath(const Cards: TJsonReader;
  const CardId: string): string;
begin
  Result := '';

  var Total := Cards.Count('cards');
  for var I := 0 to Total - 1 do
    if SameText(Cards.AsString(Format('cards[%d].id', [I])), CardId) then
      begin
        Result := Cards.AsString(Format('cards[%d].md_path', [I]));
        Exit;
      end;
end;

class function TAgentCardReader.FindCardVersion(const Cards: TJsonReader;
  const CardId: string): string;
begin
  Result := '';

  var Total := Cards.Count('cards');
  for var I := 0 to Total - 1 do
    if SameText(Cards.AsString(Format('cards[%d].id', [I])), CardId) then
      begin
        Result := Cards.AsString(Format('cards[%d].version', [I]));
        Exit;
      end;
end;

class function TAgentCardReader.ParseEnvironment(
  const Reader: TJsonReader): TEnvironmentDef;
begin
  Result := Default(TEnvironmentDef);
  if not Reader.IsValid then
    Exit;

  Result.Name := Reader.AsString('name');
  Result.Description := Reader.AsString('description');
end;

class function TAgentCardReader.ParseTools(const Reader: TJsonReader): TAgentToolsDef;
begin
  Result := Default(TAgentToolsDef);
  if not Reader.IsValid then
    Exit;

  if Reader.IsObjectNode('builtin') then
    begin
      Result.Builtin.Defined := True;
      Result.Builtin.DefaultEnabled := Reader.AsBoolean('builtin.default.enabled', False);
      Result.Builtin.DefaultPolicy := Reader.AsString('builtin.default.policy');

      var ConfigCount := Reader.Count('builtin.configs');
      for var I := 0 to ConfigCount - 1 do
        begin
          var Cfg := Default(TBuiltinToolConfig);
          Cfg.Name := Reader.AsString(Format('builtin.configs[%d].name', [I]));
          Cfg.Enabled := Reader.AsBoolean(Format('builtin.configs[%d].enabled', [I]), False);
          Cfg.Policy := Reader.AsString(Format('builtin.configs[%d].policy', [I]));
          Result.Builtin.Configs := Result.Builtin.Configs + [Cfg];
        end;
    end;
end;

class function TAgentCardReader.ParseSession(const Reader: TJsonReader): TSessionDef;
begin
  Result := Default(TSessionDef);
  if not Reader.IsValid then
    Exit;

  Result.Title := Reader.AsString('title');
end;

class function TAgentCardReader.TryReadContent(const ContentJson, CardId,
  EnvelopeVersion: string; out Def: TAgentCardDefinition): Boolean;
begin
  Def := Default(TAgentCardDefinition);
  Result := False;

  if ContentJson.Trim.IsEmpty or CardId.Trim.IsEmpty then
    Exit;

  var Content := TJsonReader.Parse(ContentJson);
  if not Content.IsValid then
    Exit;

  Def.CardId := CardId;
  Def.Version := EnvelopeVersion.Trim;
  var ContentVersion := Content.AsString('version').Trim;
  if Def.Version.IsEmpty then
    Def.Version := ContentVersion
  else
  if (not ContentVersion.IsEmpty) and
     (not SameText(Def.Version, ContentVersion)) then
    Exit;
  if Def.Version.IsEmpty then
    Def.Version := '0.0.0-dev';

  Def.Kind := ParseKind(Content.AsString('kind'));
  if Def.Kind = ackUnknown then
    Exit;

  if Content.IsObjectNode('environment') then
    Def.Environment := ParseEnvironment(
      TJsonReader.Parse(Content.ExtractSubJson('environment')));

  case Def.Kind of
    ackSingle:
      if Content.IsObjectNode('agent') then
        begin
          var A := TJsonReader.Parse(Content.ExtractSubJson('agent'));
          Def.Agent.Name := A.AsString('name');
          Def.Agent.Model := A.AsString('model');
          Def.Agent.System := A.AsString('system');
          Def.Agent.Tools := ParseTools(TJsonReader.Parse(A.ExtractSubJson('tools')));
        end;

    ackMultiagent:
      begin
        var SubCount := Content.Count('subagents');
        for var I := 0 to SubCount - 1 do
          begin
            var S := TJsonReader.Parse(
              Content.ExtractSubJson(Format('subagents[%d]', [I])));

            var Sub := Default(TSubAgentDef);
            Sub.Ref := S.AsString('ref');
            Sub.Name := S.AsString('name');
            Sub.Model := S.AsString('model');
            Sub.System := S.AsString('system');
            Sub.Tools := ParseTools(TJsonReader.Parse(S.ExtractSubJson('tools')));
            Def.SubAgents := Def.SubAgents + [Sub];
          end;

        if Content.IsObjectNode('coordinator') then
          begin
            var Co := TJsonReader.Parse(Content.ExtractSubJson('coordinator'));
            Def.Coordinator.Name := Co.AsString('name');
            Def.Coordinator.Model := Co.AsString('model');
            Def.Coordinator.System := Co.AsString('system');
            Def.Coordinator.Tools :=
              ParseTools(TJsonReader.Parse(Co.ExtractSubJson('tools')));
            Def.Coordinator.Roster := Co.ArrayStrings('roster');
          end;
      end;
  end;

  if Content.IsObjectNode('session') then
    Def.Session := ParseSession(TJsonReader.Parse(Content.ExtractSubJson('session')));

  Def.Valid := True;
  Def.DefinitionHash := TAgentDefinitionFingerprint.ComputeHash(Def);
  Result := True;
end;

class function TAgentCardReader.TryRead(const CardsJson, CardId: string;
  out Def: TAgentCardDefinition): Boolean;
begin
  Result := TryRead(CardsJson, CardId, '', Def);
end;

class function TAgentCardReader.TryRead(const CardsJson, CardId,
  CardsFolder: string; out Def: TAgentCardDefinition): Boolean;
begin
  Def := Default(TAgentCardDefinition);
  Result := False;

  if CardsJson.Trim.IsEmpty or CardId.Trim.IsEmpty then
    Exit;

  var Cards := TJsonReader.Parse(CardsJson);
  if not Cards.IsValid then
    Exit;

  var EnvelopeVersion := FindCardVersion(Cards, CardId);

  var ContentJson := FindCardContent(Cards, CardId);
  if not ContentJson.Trim.IsEmpty then
    Exit(TryReadContent(ContentJson, CardId, EnvelopeVersion, Def));

  var MarkdownPath := FindCardMarkdownPath(Cards, CardId).Trim;
  if MarkdownPath.IsEmpty then
    Exit;

  if not TPath.IsPathRooted(MarkdownPath) and not CardsFolder.Trim.IsEmpty then
    MarkdownPath := TPath.GetFullPath(TPath.Combine(CardsFolder, MarkdownPath));

  Result := TAgentMarkdownReader.TryReadFile(
    MarkdownPath, CardId, EnvelopeVersion, Def);
  if Result then
    Def.DefinitionHash := TAgentDefinitionFingerprint.ComputeHash(Def);
end;

class function TAgentCardReader.TryGetCardLabel(
  const CardsJson, CardId: string; out Name: string): Boolean;
begin
  {--- Walks the cards array, finds the entry whose top-level "id" matches
       CardId, and returns its top-level "name" without touching the inner
       "content" payload. Kept narrow on purpose: callers that need the
       full agent definition use TryRead instead. }
  Result := False;
  Name := '';

  if CardsJson.Trim.IsEmpty or CardId.Trim.IsEmpty then
    Exit;

  var Cards := TJsonReader.Parse(CardsJson);
  if not Cards.IsValid then
    Exit;

  var Total := Cards.Count('cards');
  for var I := 0 to Total - 1 do
    if SameText(Cards.AsString(Format('cards[%d].id', [I])), CardId) then
      begin
        Name := Cards.AsString(Format('cards[%d].name', [I]));
        Result := not Name.Trim.IsEmpty;
        Exit;
      end;
end;

end.
