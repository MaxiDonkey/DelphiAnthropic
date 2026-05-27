unit Demo.Anthropic.Agent.Registry;

interface

uses
  System.SysUtils, System.JSON,
  WVPythia.JSON.SafeReader, WVPythia.JSON.SafeWriter,
  Demo.Anthropic.Agent.Cards;

type
  TAgentCloudSubAgentId = record
    Ref: string;
    AgentId: string;
    CloudVersion: Integer;
    Status: string;
  end;

  TAgentCloudResource = record
    Valid: Boolean;
    RegistryEntryId: string;
    EnvironmentId: string;
    EnvironmentStatus: string;
    AgentId: string;
    AgentVersion: Integer;
    AgentStatus: string;
    SubAgents: TArray<TAgentCloudSubAgentId>;
  end;

  TAgentCloudSessionRef = record
    EntryId: string;
    SessionId: string;
    Status: string;
  end;

  TAgentCloudEnvironmentRef = record
    EntryId: string;
    EnvironmentId: string;
  end;

  TAgentCloudAgentRef = record
    EntryId: string;
    AgentId: string;
    Ref: string;
    Role: string;
  end;

  TAgentCloudCleanupPolicy = record
    SessionTtlHours: Integer;
    EnvironmentTtlDays: Integer;
    RetiredAgentTtlDays: Integer;
    DeleteSessions: Boolean;
    DeleteEnvironments: Boolean;
    ArchiveAgents: Boolean;
    class function DemoDefaults: TAgentCloudCleanupPolicy; static;
  end;

  IAgentCloudRegistry = interface
    ['{2A6B7B34-29E1-4D7E-A572-8130489F9E70}']
    function FileName: string;
    function TryFindActive(const Def: TAgentCardDefinition;
      out Resource: TAgentCloudResource): Boolean;
    procedure RetireMismatched(const Def: TAgentCardDefinition);
    procedure SaveActive(const Def: TAgentCardDefinition;
      const Resource: TAgentCloudResource);
    procedure TouchActive(const Def: TAgentCardDefinition);
    procedure MarkEntryStatus(const EntryId, Status: string);
    procedure RecordSession(const Def: TAgentCardDefinition;
      const Resource: TAgentCloudResource; const SessionId,
      Status: string);
    function HasSession(const Def: TAgentCardDefinition;
      const SessionId: string): Boolean;
    procedure MarkSessionStatus(const SessionId, Status: string);
    function ExpiredSessions(const CutoffUtc: TDateTime):
      TArray<TAgentCloudSessionRef>;
    function RetiredEnvironments(const CutoffUtc: TDateTime):
      TArray<TAgentCloudEnvironmentRef>;
    function RetiredAgents(const CutoffUtc: TDateTime):
      TArray<TAgentCloudAgentRef>;
    procedure MarkEnvironmentStatus(const EntryId, Status: string);
    procedure MarkAgentStatus(const AgentId, Status: string);
    procedure MarkCleanupStarted;
    procedure MarkCleanupCompleted;
    procedure MarkCleanupError(const ErrorText: string);
  end;

  TAgentCloudIds = record
  public
    class function NewClsid: string; static;
  end;

  TAgentCloudMetadata = record
  public
    class function Build(const Def: TAgentCardDefinition;
      const RegistryEntryId, ResourceKind, ResourceRole,
      ResourceRef: string): TJSONObject; static;
  end;

  TAgentCloudRegistry = class(TInterfacedObject, IAgentCloudRegistry)
  private
    FFileName: string;
    FJson: string;
    FLock: TObject;

    class function UtcNowText: string; static;
    class function NewRegistryJson: string; static;
    class function EntryPath(const EntryIndex: Integer): string; static;
    class function SessionPath(const EntryIndex,
      SessionIndex: Integer): string; static;
    class function SubAgentPath(const EntryIndex,
      SubIndex: Integer): string; static;
    class function IsOlderThan(const Value: string;
      const CutoffUtc: TDateTime): Boolean; static;
    class function SessionJson(const Resource: TAgentCloudResource;
      const SessionId, Status: string): string; static;
    class function SubAgentJson(const Sub: TAgentCloudSubAgentId): string; static;
    class function FormatJsonForFile(const Value: string): string; static;

    function Reader: TJsonReader;
    function Writer: TJsonWriter;
    function FindEntryIndexById(const R: TJsonReader;
      const EntryId: string): Integer;
    function FindActiveEntryIndex(const R: TJsonReader;
      const Def: TAgentCardDefinition): Integer;
    function HasKeptSessions(const R: TJsonReader;
      const EntryIndex: Integer): Boolean;
    procedure Load;
    procedure Commit(var W: TJsonWriter);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;

    function FileName: string;
    function TryFindActive(const Def: TAgentCardDefinition;
      out Resource: TAgentCloudResource): Boolean;
    procedure RetireMismatched(const Def: TAgentCardDefinition);
    procedure SaveActive(const Def: TAgentCardDefinition;
      const Resource: TAgentCloudResource);
    procedure TouchActive(const Def: TAgentCardDefinition);
    procedure MarkEntryStatus(const EntryId, Status: string);
    procedure RecordSession(const Def: TAgentCardDefinition;
      const Resource: TAgentCloudResource; const SessionId,
      Status: string);
    function HasSession(const Def: TAgentCardDefinition;
      const SessionId: string): Boolean;
    procedure MarkSessionStatus(const SessionId, Status: string);
    function ExpiredSessions(const CutoffUtc: TDateTime):
      TArray<TAgentCloudSessionRef>;
    function RetiredEnvironments(const CutoffUtc: TDateTime):
      TArray<TAgentCloudEnvironmentRef>;
    function RetiredAgents(const CutoffUtc: TDateTime):
      TArray<TAgentCloudAgentRef>;
    procedure MarkEnvironmentStatus(const EntryId, Status: string);
    procedure MarkAgentStatus(const AgentId, Status: string);
    procedure MarkCleanupStarted;
    procedure MarkCleanupCompleted;
    procedure MarkCleanupError(const ErrorText: string);
  end;

implementation

{$REGION 'Dev note'}
(*

  Local Managed Agents registry for the pythia-anthropic VCL demo.

  Anthropic Managed Agents, environments and sessions live in the cloud, but
  this demo needs a local memory of what it created so later app sessions can
  reuse matching cloud resources instead of provisioning a fresh environment
  and agents every time.

  An active registry entry is keyed by the card id, card version and
  definition hash. If any of those values changes, the old entry is retired
  and a new cloud resource set is provisioned. The registry entry id itself is
  a local CLSID-style id without braces; Anthropic resource ids are stored
  separately.

  The registry also keeps session ids and cleanup status. Cleanup code asks
  this unit for expired terminal sessions, retired environments and retired
  agents; cloud cleanup then reports back through Mark*Status methods. This
  keeps cleanup scoped to resources created by this demo, not to the whole
  Anthropic account.

  JSON access goes through WVPythia.JSON.SafeReader/SafeWriter. System.JSON is
  still present only because the Anthropic SDK metadata API expects a
  TJSONObject instance.

*)
{$ENDREGION}

uses
  System.Classes, System.IOUtils, System.DateUtils;

const
  REGISTRY_SCHEMA_VERSION = 1;
  PROVIDER_NAME = 'anthropic';
  DEMO_NAME = 'VCL_Anthropic';

type
  TAgentCloudSessionStatus = record
  public
    class function IsTerminal(const Status: string): Boolean; static;
    class function IsCleanable(const Status: string): Boolean; static;
  end;

{ TAgentCloudSessionStatus }

class function TAgentCloudSessionStatus.IsTerminal(
  const Status: string): Boolean;
begin
  Result :=
    SameText(Status, 'deleted') or
    SameText(Status, 'archived') or
    SameText(Status, 'missing');
end;

class function TAgentCloudSessionStatus.IsCleanable(
  const Status: string): Boolean;
begin
  Result :=
    SameText(Status, 'completed') or
    SameText(Status, 'failed') or
    SameText(Status, 'interrupted');
end;

{ TAgentCloudCleanupPolicy }

class function TAgentCloudCleanupPolicy.DemoDefaults:
  TAgentCloudCleanupPolicy;
begin
  Result := Default(TAgentCloudCleanupPolicy);
  Result.SessionTtlHours := 24;
  Result.EnvironmentTtlDays := 7;
  Result.RetiredAgentTtlDays := 30;
  Result.DeleteSessions := True;
  Result.DeleteEnvironments := True;
  Result.ArchiveAgents := True;
end;

{ TAgentCloudIds }

class function TAgentCloudIds.NewClsid: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := Guid.ToString;
  if Result.StartsWith('{') and Result.EndsWith('}') then
    Result := Result.Substring(1, Result.Length - 2);
end;

{ TAgentCloudMetadata }

class function TAgentCloudMetadata.Build(const Def: TAgentCardDefinition;
  const RegistryEntryId, ResourceKind, ResourceRole,
  ResourceRef: string): TJSONObject;
begin
  var W := TJsonWriter.NewObject;
  W.SetString('pythia_demo', DEMO_NAME);
  W.SetString('pythia_provider', PROVIDER_NAME);
  W.SetString('pythia_registry_entry_id', RegistryEntryId);
  W.SetString('pythia_card_id', Def.CardId);
  W.SetString('pythia_card_version', Def.Version);
  W.SetString('pythia_definition_hash', Def.DefinitionHash);
  W.SetString('pythia_resource_kind', ResourceKind);
  W.SetString('pythia_resource_role', ResourceRole);
  W.SetString('pythia_resource_ref', ResourceRef);

  if W.JSONObject = nil then
    Exit(TJSONObject.Create);

  Result := TJSONObject(W.JSONObject.Clone);
end;

{ TAgentCloudRegistry }

constructor TAgentCloudRegistry.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  FLock := TObject.Create;
  Load;
end;

destructor TAgentCloudRegistry.Destroy;
begin
  FLock.Free;
  inherited;
end;

class function TAgentCloudRegistry.UtcNowText: string;
begin
  Result := DateToISO8601(TTimeZone.Local.ToUniversalTime(Now), True);
end;

class function TAgentCloudRegistry.NewRegistryJson: string;
begin
  var W := TJsonWriter.NewObject;
  W.SetInteger('schema_version', REGISTRY_SCHEMA_VERSION);
  W.SetString('provider', PROVIDER_NAME);
  W.SetString('demo', DEMO_NAME);
  W.SetString('created_at', UtcNowText);
  W.EnsureArray('entries');

  Result := W.ToJson;
end;

class function TAgentCloudRegistry.EntryPath(
  const EntryIndex: Integer): string;
begin
  Result := Format('entries[%d]', [EntryIndex]);
end;

class function TAgentCloudRegistry.SessionPath(const EntryIndex,
  SessionIndex: Integer): string;
begin
  Result := Format('entries[%d].sessions[%d]', [EntryIndex, SessionIndex]);
end;

class function TAgentCloudRegistry.SubAgentPath(const EntryIndex,
  SubIndex: Integer): string;
begin
  Result := Format('entries[%d].subagents[%d]', [EntryIndex, SubIndex]);
end;

class function TAgentCloudRegistry.IsOlderThan(const Value: string;
  const CutoffUtc: TDateTime): Boolean;
begin
  Result := False;
  if Value.Trim.IsEmpty then
    Exit;

  try
    Result := ISO8601ToDate(Value, True) < CutoffUtc;
  except
    Result := False;
  end;
end;

class function TAgentCloudRegistry.SessionJson(
  const Resource: TAgentCloudResource; const SessionId,
  Status: string): string;
begin
  var W := TJsonWriter.NewObject;
  W.SetString('id', SessionId);
  W.SetString('agent_id', Resource.AgentId);
  W.SetString('environment_id', Resource.EnvironmentId);
  W.SetString('status', Status);
  W.SetString('created_at', UtcNowText);
  W.SetString('last_used_at', UtcNowText);

  Result := W.ToJson;
end;

class function TAgentCloudRegistry.SubAgentJson(
  const Sub: TAgentCloudSubAgentId): string;
begin
  var W := TJsonWriter.NewObject;
  W.SetString('ref', Sub.Ref);
  W.SetString('agent_id', Sub.AgentId);
  W.SetInteger('agent_version', Sub.CloudVersion);
  W.SetString('status', 'active');

  Result := W.ToJson;
end;

class function TAgentCloudRegistry.FormatJsonForFile(
  const Value: string): string;
begin
  Result := Value.Trim;
  if Result.IsEmpty then
    Exit;

  var R := TJsonReader.Parse(Result);
  if R.IsValid then
    Result := R.Format().Trim + sLineBreak;
end;

function TAgentCloudRegistry.Reader: TJsonReader;
begin
  Result := TJsonReader.Parse(FJson);
end;

function TAgentCloudRegistry.Writer: TJsonWriter;
begin
  Result := TJsonWriter.Parse(FJson);
  if not Result.IsValid then
    Result := TJsonWriter.Parse(NewRegistryJson);
end;

procedure TAgentCloudRegistry.Load;
begin
  FJson := '';
  if FileExists(FFileName) then
    try
      FJson := TFile.ReadAllText(FFileName, TEncoding.UTF8);
    except
      FJson := '';
    end;

  var WRoot := TJsonWriter.Parse(FJson);
  if (not WRoot.IsValid) or (WRoot.JSONObject = nil) then
    FJson := NewRegistryJson
  else
    begin
      var R := TJsonReader.Parse(FJson);
      if not R.IsArrayNode('entries') then
        begin
          var W := TJsonWriter.Parse(FJson);
          W.Remove('entries');
          W.EnsureArray('entries');
          FJson := W.ToJson;
        end;
    end;
end;

procedure TAgentCloudRegistry.Commit(var W: TJsonWriter);
begin
  W.SetString('updated_at', UtcNowText);
  FJson := FormatJsonForFile(W.ToJson);

  var Folder := TPath.GetDirectoryName(FFileName);
  if not Folder.Trim.IsEmpty then
    TDirectory.CreateDirectory(Folder);

  var TempName := FFileName + '.tmp';
  TFile.WriteAllText(TempName, FJson, TEncoding.UTF8);
  TFile.Copy(TempName, FFileName, True);
  TFile.Delete(TempName);
end;

function TAgentCloudRegistry.FileName: string;
begin
  Result := FFileName;
end;

function TAgentCloudRegistry.FindEntryIndexById(const R: TJsonReader;
  const EntryId: string): Integer;
begin
  Result := -1;
  for var I := 0 to R.Count('entries') - 1 do
    if SameText(R.AsString(Format('entries[%d].id', [I])), EntryId) then
      Exit(I);
end;

function TAgentCloudRegistry.FindActiveEntryIndex(const R: TJsonReader;
  const Def: TAgentCardDefinition): Integer;
begin
  Result := -1;
  for var I := 0 to R.Count('entries') - 1 do
    if SameText(R.AsString(Format('entries[%d].status', [I])), 'active') and
       SameText(R.AsString(Format('entries[%d].card_id', [I])), Def.CardId) and
       SameText(R.AsString(Format('entries[%d].card_version', [I])),
         Def.Version) and
       SameText(R.AsString(Format('entries[%d].definition_hash', [I])),
         Def.DefinitionHash) then
      Exit(I);
end;

function TAgentCloudRegistry.HasKeptSessions(const R: TJsonReader;
  const EntryIndex: Integer): Boolean;
begin
  Result := False;
  for var I := 0 to R.Count(Format('entries[%d].sessions', [EntryIndex])) - 1 do
    if not TAgentCloudSessionStatus.IsTerminal(
      R.AsString(Format('entries[%d].sessions[%d].status',
        [EntryIndex, I]))) then
      Exit(True);
end;

function TAgentCloudRegistry.TryFindActive(const Def: TAgentCardDefinition;
  out Resource: TAgentCloudResource): Boolean;
begin
  TMonitor.Enter(FLock);
  try
    Resource := Default(TAgentCloudResource);
    var R := Reader;
    var EntryIndex := FindActiveEntryIndex(R, Def);
    Result := EntryIndex >= 0;
    if not Result then
      Exit;

    var Base := EntryPath(EntryIndex);
    Resource.Valid := True;
    Resource.RegistryEntryId := R.AsString(Base + '.id');
    Resource.EnvironmentId := R.AsString(Base + '.environment_id');
    Resource.EnvironmentStatus := R.AsString(Base + '.environment_status');
    Resource.AgentId := R.AsString(Base + '.agent_id');
    Resource.AgentVersion := R.AsInteger(Base + '.agent_version');
    Resource.AgentStatus := R.AsString(Base + '.agent_status');

    for var I := 0 to R.Count(Base + '.subagents') - 1 do
      begin
        var SubPath := SubAgentPath(EntryIndex, I);
        var Sub := Default(TAgentCloudSubAgentId);
        Sub.Ref := R.AsString(SubPath + '.ref');
        Sub.AgentId := R.AsString(SubPath + '.agent_id');
        Sub.CloudVersion := R.AsInteger(SubPath + '.agent_version');
        Sub.Status := R.AsString(SubPath + '.status');
        Resource.SubAgents := Resource.SubAgents + [Sub];
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.RetireMismatched(
  const Def: TAgentCardDefinition);
begin
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var W := Writer;
    for var I := 0 to R.Count('entries') - 1 do
      if SameText(R.AsString(Format('entries[%d].status', [I])), 'active') and
         SameText(R.AsString(Format('entries[%d].card_id', [I])),
           Def.CardId) and
         (not SameText(R.AsString(Format('entries[%d].card_version', [I])),
            Def.Version) or
          not SameText(R.AsString(Format('entries[%d].definition_hash', [I])),
            Def.DefinitionHash)) then
        begin
          W.SetString(EntryPath(I) + '.status', 'retired');
          W.SetString(EntryPath(I) + '.retired_at', UtcNowText);
        end;
    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.SaveActive(const Def: TAgentCardDefinition;
  const Resource: TAgentCloudResource);
begin
  if Resource.RegistryEntryId.Trim.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var W := Writer;
    var EntryIndex := FindEntryIndexById(R, Resource.RegistryEntryId);
    if EntryIndex < 0 then
      begin
        EntryIndex := R.Count('entries');
        W.EnsureArray('entries');
        W.AppendObjectJson('entries', '{}');
      end;

    var Base := EntryPath(EntryIndex);
    W.SetString(Base + '.id', Resource.RegistryEntryId);
    W.SetString(Base + '.created_at',
      R.AsString(Base + '.created_at', UtcNowText));
    W.SetString(Base + '.card_id', Def.CardId);
    W.SetString(Base + '.card_version', Def.Version);
    W.SetString(Base + '.definition_hash', Def.DefinitionHash);
    W.SetString(Base + '.status', 'active');
    W.SetString(Base + '.last_used_at', UtcNowText);
    W.SetString(Base + '.environment_id', Resource.EnvironmentId);
    W.SetString(Base + '.environment_status', 'active');
    W.SetString(Base + '.agent_id', Resource.AgentId);
    W.SetInteger(Base + '.agent_version', Resource.AgentVersion);
    W.SetString(Base + '.agent_status', 'active');

    W.Remove(Base + '.subagents');
    W.EnsureArray(Base + '.subagents');
    for var Sub in Resource.SubAgents do
      W.AppendObjectJson(Base + '.subagents', SubAgentJson(Sub));

    if not R.IsArrayNode(Base + '.sessions') then
      begin
        W.Remove(Base + '.sessions');
        W.EnsureArray(Base + '.sessions');
      end;

    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.TouchActive(const Def: TAgentCardDefinition);
begin
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var EntryIndex := FindActiveEntryIndex(R, Def);
    if EntryIndex >= 0 then
      begin
        var W := Writer;
        W.SetString(EntryPath(EntryIndex) + '.last_used_at', UtcNowText);
        Commit(W);
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkEntryStatus(const EntryId, Status: string);
begin
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var EntryIndex := FindEntryIndexById(R, EntryId);
    if EntryIndex >= 0 then
      begin
        var W := Writer;
        W.SetString(EntryPath(EntryIndex) + '.status', Status);
        W.SetString(EntryPath(EntryIndex) + '.' + Status + '_at', UtcNowText);
        Commit(W);
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.RecordSession(const Def: TAgentCardDefinition;
  const Resource: TAgentCloudResource; const SessionId, Status: string);
begin
  if SessionId.Trim.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var EntryIndex := FindActiveEntryIndex(R, Def);
    if EntryIndex < 0 then
      EntryIndex := FindEntryIndexById(R, Resource.RegistryEntryId);
    if EntryIndex < 0 then
      Exit;

    var W := Writer;
    var Base := EntryPath(EntryIndex);
    if not R.IsArrayNode(Base + '.sessions') then
      begin
        W.Remove(Base + '.sessions');
        W.EnsureArray(Base + '.sessions');
      end;

    for var I := 0 to R.Count(Base + '.sessions') - 1 do
      if SameText(R.AsString(SessionPath(EntryIndex, I) + '.id'),
        SessionId) then
        begin
          W.SetString(SessionPath(EntryIndex, I) + '.status', Status);
          W.SetString(SessionPath(EntryIndex, I) + '.last_used_at', UtcNowText);
          Commit(W);
          Exit;
        end;

    W.AppendObjectJson(Base + '.sessions',
      SessionJson(Resource, SessionId, Status));
    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TAgentCloudRegistry.HasSession(const Def: TAgentCardDefinition;
  const SessionId: string): Boolean;
begin
  Result := False;
  if SessionId.Trim.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var EntryIndex := FindActiveEntryIndex(R, Def);
    if EntryIndex < 0 then
      Exit;

    for var I := 0 to R.Count(EntryPath(EntryIndex) + '.sessions') - 1 do
      if SameText(R.AsString(SessionPath(EntryIndex, I) + '.id'),
           SessionId) and
         (not TAgentCloudSessionStatus.IsTerminal(
           R.AsString(SessionPath(EntryIndex, I) + '.status'))) then
        Exit(True);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkSessionStatus(const SessionId,
  Status: string);
begin
  if SessionId.Trim.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var W := Writer;
    for var EntryIndex := 0 to R.Count('entries') - 1 do
      for var SessionIndex := 0 to R.Count(EntryPath(EntryIndex) + '.sessions') - 1 do
        if SameText(R.AsString(SessionPath(EntryIndex, SessionIndex) + '.id'),
          SessionId) then
          begin
            W.SetString(SessionPath(EntryIndex, SessionIndex) + '.status',
              Status);
            W.SetString(SessionPath(EntryIndex, SessionIndex) + '.last_used_at',
              UtcNowText);
            Commit(W);
            Exit;
          end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TAgentCloudRegistry.ExpiredSessions(
  const CutoffUtc: TDateTime): TArray<TAgentCloudSessionRef>;
begin
  Result := [];
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    for var EntryIndex := 0 to R.Count('entries') - 1 do
      for var SessionIndex := 0 to R.Count(EntryPath(EntryIndex) + '.sessions') - 1 do
        begin
          var Path := SessionPath(EntryIndex, SessionIndex);
          var Status := R.AsString(Path + '.status');
          if TAgentCloudSessionStatus.IsCleanable(Status) and
             IsOlderThan(R.AsString(Path + '.last_used_at'), CutoffUtc) then
            begin
              var Ref := Default(TAgentCloudSessionRef);
              Ref.EntryId := R.AsString(EntryPath(EntryIndex) + '.id');
              Ref.SessionId := R.AsString(Path + '.id');
              Ref.Status := Status;
              Result := Result + [Ref];
            end;
        end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TAgentCloudRegistry.RetiredEnvironments(
  const CutoffUtc: TDateTime): TArray<TAgentCloudEnvironmentRef>;
begin
  Result := [];
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    for var EntryIndex := 0 to R.Count('entries') - 1 do
      if SameText(R.AsString(EntryPath(EntryIndex) + '.status'), 'retired') and
         SameText(R.AsString(EntryPath(EntryIndex) + '.environment_status'),
           'active') and
         (not HasKeptSessions(R, EntryIndex)) and
         IsOlderThan(R.AsString(EntryPath(EntryIndex) + '.last_used_at'),
           CutoffUtc) then
        begin
          var Ref := Default(TAgentCloudEnvironmentRef);
          Ref.EntryId := R.AsString(EntryPath(EntryIndex) + '.id');
          Ref.EnvironmentId :=
            R.AsString(EntryPath(EntryIndex) + '.environment_id');
          Result := Result + [Ref];
        end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

function TAgentCloudRegistry.RetiredAgents(
  const CutoffUtc: TDateTime): TArray<TAgentCloudAgentRef>;
begin
  Result := [];
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    for var EntryIndex := 0 to R.Count('entries') - 1 do
      if SameText(R.AsString(EntryPath(EntryIndex) + '.status'), 'retired') and
         (not HasKeptSessions(R, EntryIndex)) and
         IsOlderThan(R.AsString(EntryPath(EntryIndex) + '.last_used_at'),
           CutoffUtc) then
        begin
          if SameText(R.AsString(EntryPath(EntryIndex) + '.agent_status'),
            'active') then
            begin
              var Coord := Default(TAgentCloudAgentRef);
              Coord.EntryId := R.AsString(EntryPath(EntryIndex) + '.id');
              Coord.AgentId := R.AsString(EntryPath(EntryIndex) + '.agent_id');
              Coord.Ref := 'coordinator';
              Coord.Role := 'coordinator';
              Result := Result + [Coord];
            end;

          for var SubIndex := 0 to R.Count(EntryPath(EntryIndex) + '.subagents') - 1 do
            if SameText(R.AsString(SubAgentPath(EntryIndex, SubIndex) +
              '.status'), 'active') then
              begin
                var Sub := Default(TAgentCloudAgentRef);
                Sub.EntryId := R.AsString(EntryPath(EntryIndex) + '.id');
                Sub.AgentId := R.AsString(SubAgentPath(EntryIndex, SubIndex) +
                  '.agent_id');
                Sub.Ref := R.AsString(SubAgentPath(EntryIndex, SubIndex) +
                  '.ref');
                Sub.Role := 'subagent';
                Result := Result + [Sub];
              end;
        end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkEnvironmentStatus(const EntryId,
  Status: string);
begin
  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var EntryIndex := FindEntryIndexById(R, EntryId);
    if EntryIndex >= 0 then
      begin
        var W := Writer;
        W.SetString(EntryPath(EntryIndex) + '.environment_status', Status);
        W.SetString(EntryPath(EntryIndex) + '.environment_' + Status + '_at',
          UtcNowText);
        Commit(W);
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkAgentStatus(const AgentId, Status: string);
begin
  if AgentId.Trim.IsEmpty then
    Exit;

  TMonitor.Enter(FLock);
  try
    var R := Reader;
    var W := Writer;
    for var EntryIndex := 0 to R.Count('entries') - 1 do
      begin
        if SameText(R.AsString(EntryPath(EntryIndex) + '.agent_id'),
          AgentId) then
          begin
            W.SetString(EntryPath(EntryIndex) + '.agent_status', Status);
            W.SetString(EntryPath(EntryIndex) + '.agent_' + Status + '_at',
              UtcNowText);
            Commit(W);
            Exit;
          end;

        for var SubIndex := 0 to R.Count(EntryPath(EntryIndex) + '.subagents') - 1 do
          if SameText(R.AsString(SubAgentPath(EntryIndex, SubIndex) +
            '.agent_id'), AgentId) then
            begin
              W.SetString(SubAgentPath(EntryIndex, SubIndex) + '.status',
                Status);
              W.SetString(SubAgentPath(EntryIndex, SubIndex) + '.' + Status +
                '_at', UtcNowText);
              Commit(W);
              Exit;
            end;
      end;
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkCleanupStarted;
begin
  TMonitor.Enter(FLock);
  try
    var W := Writer;
    W.EnsureObject('cleanup');
    W.SetString('cleanup.last_started_at', UtcNowText);
    W.SetString('cleanup.last_error', '');
    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkCleanupCompleted;
begin
  TMonitor.Enter(FLock);
  try
    var W := Writer;
    W.EnsureObject('cleanup');
    W.SetString('cleanup.last_completed_at', UtcNowText);
    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TAgentCloudRegistry.MarkCleanupError(const ErrorText: string);
begin
  TMonitor.Enter(FLock);
  try
    var W := Writer;
    W.EnsureObject('cleanup');
    W.SetString('cleanup.last_error', ErrorText);
    Commit(W);
  finally
    TMonitor.Exit(FLock);
  end;
end;

end.
