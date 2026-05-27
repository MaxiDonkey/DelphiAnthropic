unit Demo.Anthropic.Session.Flow;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  WVPythia.Chat.Interfaces, WVPythia.Chat.ManagedFlow, WVPythia.Vendors.Services,
  Anthropic,
  Demo.Anthropic.Context, Demo.Anthropic.Agent.Provisioning;

type
  IAgentSessionFlow = interface
    ['{4E4293C7-B002-4618-BCC3-18C3FFB387BC}']
    /// <summary>
    /// Runs one Managed Agents turn for the current Pythia chat and finalizes
    /// it through AOnFinalize. Called on the UI thread.
    /// </summary>
    procedure Run(const AState: TStateBuffer;
      const AOnFinalize: TManagedItemFinalizeProc);
  end;

  TAgentSessionFlow = class(TInterfacedObject, IAgentSessionFlow)
  private
    FBrowser: IPythiaBrowser;
    FContext: IContext;
    FClient: IAnthropic;
    FProvisioner: IAgentProvisioner;
    FCurrentTurn: TObject;
  public
    constructor Create(const ABrowser: IPythiaBrowser; const AContext: IContext;
      const AClient: IAnthropic; const AProvisioner: IAgentProvisioner);
    destructor Destroy; override;
    procedure Run(const AState: TStateBuffer;
      const AOnFinalize: TManagedItemFinalizeProc);
  end;

implementation

{$REGION 'Dev note'}
(*

  Managed Agents turn orchestration for the pythia-anthropic VCL demo.

  This is the agent-side counterpart of the Messages streaming chat in
  Demo.Anthropic.Services. When the user submits a prompt with an agent
  card selected, TAnthropicServices routes the turn here instead of the
  Messages API path.

  One Pythia chat maps to one Managed Agents Session. TAgentTurn handles a
  single turn:

    1. read the agent card definition (Demo.Anthropic.Agent.Cards),
    2. provision Environment + Agent(s) (blocking,
       Demo.Anthropic.Agent.Provisioning),
    3. open a new Session or reuse the one recorded by a prior turn
       (IContext.LastAgentSessionId),
    4. send the user message,
    5. stream the session events (Demo.Anthropic.Session.Transport),
    6. map each event to the Pythia display blocks and browser UI,
    7. drive the interactive loops (tool confirmation, interrupt),
    8. finalize through the shared Demo.Anthropic.Finalize machinery.

  Threading
  ---------
  Execute runs on the UI thread. Provisioning / session setup runs on a
  worker thread (blocking SDK calls). The transport invokes HandleEvent /
  HandleDone on its SDK-backed stream worker; UI and browser mutations are
  marshalled to the main thread via TThread.Queue. The tool-confirmation
  prompt goes through FBrowser.WebDecisionDlg: the call blocks the stream
  worker thread while the dialog itself marshals UI work to the WebView and
  resumes the worker when the operator's decision arrives. TEmitGuard
  guarantees the turn finalizes exactly once.

*)
{$ENDREGION}

uses
  System.UITypes, System.Generics.Collections, System.IOUtils,
  WVPythia.Chat.DecisionDlg,
  WVPythia.TextFile.Helper, WVPythia.JSON.SafeReader,
  WVPythia.JSON.SafeWriter, WVPythia.Chat.Consts,
  Anthropic.Sessions,
  Demo.Anthropic.Agent.Cards, Demo.Anthropic.Agent.Folder,
  Demo.Anthropic.Agent.Fingerprint, Demo.Anthropic.Agent.Registry,
  Demo.Anthropic.Agent.LocalApply, Demo.Anthropic.Agent.TurnDisplay,
  Demo.Anthropic.Session.Transport, Demo.Anthropic.Session.Events,
  Demo.Anthropic.Finalize, Demo.Anthropic.Strs;

type
  {--- One Managed Agents turn. Owned by TAgentSessionFlow, which keeps it
       alive until the next turn (Pythia serializes turns). }
  TAgentTurn = class
  strict private
    function TryKey(const Key: string; out ToolEv: TSessionEvent): Boolean;
  private
    FBrowser: IPythiaBrowser;
    FContext: IContext;
    FClient: IAnthropic;
    FProvisioner: IAgentProvisioner;

    FState: TStateBuffer;
    FDisplay: IPythiaTurnDisplay;
    FEmitGuard: IEmitGuard;
    FWorker: TThread;
    FTransport: TSessionStreamTransport;

    FCardId: string;
    FCardVersion: string;
    FDefinitionHash: string;
    FPriorSessionId: string;
    FSessionId: string;
    FResolved: TResolvedAgent;
    FCancelled: Boolean;
    FStreamError: string;
    FFinishRequested: Boolean;
    FDeniedToolRequestCount: Integer;
    FEventTrace: TArray<string>;
    FEventLogPath: string;
    FSelectedProjectFolder: string;
    FOfferLocalApply: Boolean;
    FPendingToolEvents: TDictionary<string, TSessionEvent>;

    procedure Queue(const P: TThreadProcedure);
    procedure Finalize(const AsError: Boolean; const ErrMsg: string);

    function UploadFolderFiles(const LocalFolder: string): TArray<TFolderUploadedFile>;
    function BuiltinToolEnabled(const Tools: TAgentToolsDef;
      const ToolName: string): Boolean;
    function ToolsNeedProjectFolder(const Tools: TAgentToolsDef): Boolean;
    function AgentNeedsProjectFolder(const Def: TAgentCardDefinition): Boolean;
    function ToolsCanModifySandbox(const Tools: TAgentToolsDef): Boolean;
    function AgentCanModifySandbox(const Def: TAgentCardDefinition): Boolean;
    procedure BindSelectedProjectFolder(var Def: TAgentCardDefinition);
    procedure TryOfferLocalApply;

    procedure ApplyUiModel(var Def: TAgentCardDefinition);
    function BuildTurnPrompt(const Def: TAgentCardDefinition): string;
    function CreateSession(const Def: TAgentCardDefinition): string;
    procedure RecordTrace(const ModelId: string);
    procedure RecordEventTrace(const EventData: string);
    procedure SendEventsSafe(const Events: array of TSessionEventParams);
    procedure SendTurn(const Def: TAgentCardDefinition);

    procedure WorkerBody;
    procedure HandleEvent(const EventData: string);
    procedure HandleDone(const ErrorMsg: string);
    function BuildConfirmation(const Ev: TSessionEvent): TSessionEventParams;
    procedure HandleConfirmationRequest(const Ev: TSessionEvent);
    procedure RememberToolUse(const Ev: TSessionEvent);
    function TryResolveActionTool(const Ev: TSessionEvent;
      out ToolEv: TSessionEvent): Boolean;
    function ToolUseKey(const Ev: TSessionEvent): string;
    function ToolTitle(const Ev: TSessionEvent): string;
    function ToolOutputText(const Ev: TSessionEvent): string;
    function ThreadTitle(const Ev: TSessionEvent): string;
    function IsEmptyMessagePlaceholder(const Text: string): Boolean;
  public
    constructor Create(const ABrowser: IPythiaBrowser; const AContext: IContext;
      const AClient: IAnthropic; const AProvisioner: IAgentProvisioner;
      const AState: TStateBuffer; const AOnFinalize: TManagedItemFinalizeProc);
    destructor Destroy; override;
    procedure Execute;
  end;

  TAgentTurnWorker = class(TThread)
  private
    FOwner: TAgentTurn;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TAgentTurn);
  end;

{ TAgentTurnWorker }

constructor TAgentTurnWorker.Create(AOwner: TAgentTurn);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
end;

procedure TAgentTurnWorker.Execute;
begin
  FOwner.WorkerBody;
end;

{ TAgentTurn }

constructor TAgentTurn.Create(const ABrowser: IPythiaBrowser;
  const AContext: IContext; const AClient: IAnthropic;
  const AProvisioner: IAgentProvisioner; const AState: TStateBuffer;
  const AOnFinalize: TManagedItemFinalizeProc);
begin
  inherited Create;
  FBrowser := ABrowser;
  FContext := AContext;
  FClient := AClient;
  FProvisioner := AProvisioner;
  FState := AState;
  FDisplay := TAnthropicAgentTurnDisplay.Create(FBrowser);
  FEmitGuard := TEmitGuard.Create(AOnFinalize);
  FPendingToolEvents := TDictionary<string, TSessionEvent>.Create;
end;

destructor TAgentTurn.Destroy;
begin
  if Assigned(FWorker) then
    begin
      FWorker.Terminate;
      FWorker.WaitFor;
      FWorker.Free;
    end;

  FTransport.Free;
  FPendingToolEvents.Free;
  inherited;
end;

procedure TAgentTurn.Queue(const P: TThreadProcedure);
begin
  TThread.Queue(nil, P);
end;

function TAgentTurn.UploadFolderFiles(
  const LocalFolder: string): TArray<TFolderUploadedFile>;
begin
  {--- A managed agent runs in a cloud sandbox: a local folder cannot be
       mounted directly. Every reviewable file is uploaded to the Files API,
       then mounted as a session "file" resource by CreateSession.
  }
  Result := TFolderUploader.Upload(FClient, LocalFolder,
    procedure (const Current, Total: Integer; const RelativePath: string)
    begin
      FDisplay.Status(
        Format('Uploading project file (%d/%d)', [Current, Total]),
        RelativePath);
    end);
end;

function TAgentTurn.BuiltinToolEnabled(const Tools: TAgentToolsDef;
  const ToolName: string): Boolean;
begin
  Result := False;
  if not Tools.Builtin.Defined then
    Exit;

  Result := Tools.Builtin.DefaultEnabled;
  for var Cfg in Tools.Builtin.Configs do
    if SameText(Cfg.Name, ToolName) then
      Exit(Cfg.Enabled);
end;

function TAgentTurn.ToolsNeedProjectFolder(
  const Tools: TAgentToolsDef): Boolean;
begin
  Result :=
    BuiltinToolEnabled(Tools, 'read') or
    BuiltinToolEnabled(Tools, 'glob') or
    BuiltinToolEnabled(Tools, 'grep') or
    BuiltinToolEnabled(Tools, 'edit') or
    BuiltinToolEnabled(Tools, 'write');
end;

function TAgentTurn.AgentNeedsProjectFolder(
  const Def: TAgentCardDefinition): Boolean;
begin
  case Def.Kind of
    ackSingle:
      Exit(ToolsNeedProjectFolder(Def.Agent.Tools));

    ackMultiagent:
      begin
        if ToolsNeedProjectFolder(Def.Coordinator.Tools) then
          Exit(True);

        for var Sub in Def.SubAgents do
          if ToolsNeedProjectFolder(Sub.Tools) then
            Exit(True);
      end;
  end;

  Result := False;
end;

function TAgentTurn.ToolsCanModifySandbox(
  const Tools: TAgentToolsDef): Boolean;
begin
  Result :=
    BuiltinToolEnabled(Tools, 'edit') or
    BuiltinToolEnabled(Tools, 'write');
end;

function TAgentTurn.AgentCanModifySandbox(
  const Def: TAgentCardDefinition): Boolean;
begin
  case Def.Kind of
    ackSingle:
      Exit(ToolsCanModifySandbox(Def.Agent.Tools));

    ackMultiagent:
      begin
        if ToolsCanModifySandbox(Def.Coordinator.Tools) then
          Exit(True);

        for var Sub in Def.SubAgents do
          if ToolsCanModifySandbox(Sub.Tools) then
            Exit(True);
      end;
  end;

  Result := False;
end;

procedure TAgentTurn.BindSelectedProjectFolder(var Def: TAgentCardDefinition);
begin
  Def.Session.Folder.Defined := False;
  Def.Session.Folder.Path := '';
  FSelectedProjectFolder := '';
  FOfferLocalApply := False;

  if not AgentNeedsProjectFolder(Def) then
    Exit;

  var ProjectFolder := FState.Project.FullPath.Trim;
  if ProjectFolder.IsEmpty then
    raise Exception.Create(
      'This managed agent needs a selected project folder. Select a ' +
      'project with the Project button before starting the agent.');

  var FolderError := '';
  if not TFolderUploader.TryValidate(ProjectFolder, FolderError) then
    raise Exception.Create(FolderError);

  Def.Session.Folder.Defined := True;
  Def.Session.Folder.Path := ProjectFolder;
  FSelectedProjectFolder := ProjectFolder;
  FOfferLocalApply := AgentCanModifySandbox(Def);

  FDisplay.Status('Selected project folder', ProjectFolder);
end;

procedure TAgentTurn.Finalize(const AsError: Boolean; const ErrMsg: string);
begin
  {--- All finalization runs on the main thread and is deduplicated by the
       emit guard, so concurrent completion paths (sekDone, HandleDone,
       cancellation, an exception) collapse to a single emit.
  }
  TThread.Queue(nil,
    procedure
    begin
      {--- Persist the COMPLETE event stream as the turn's response trace, so
           the turn is perfectly traceable and reloadable with every step —
           not just the final text. Built once, at the single finalization
           point, after every event has been collected. The display bridge
           provides the cloned Pythia blocks that travel alongside in
           TFinalizeData. }
      FState.JsonResponse := string.Join(sLineBreak, FEventTrace);

      if AsError then
        begin
          FState.Error := True;
          if not ErrMsg.IsEmpty then
            FState.ErrorMessage := ErrMsg;
        end;

      if FState.Error then
        begin
          var Err := Exception.Create(FState.ErrorMessage);
          try
            FEmitGuard.TryEmit(TFinalizeData.FromException(
              Err, FState, FDisplay));
          finally
            Err.Free;
          end;
        end
      else
        FEmitGuard.TryEmit(TFinalizeData.FromState(FState, FDisplay));
    end);
end;

procedure TAgentTurn.ApplyUiModel(var Def: TAgentCardDefinition);
begin
  {--- A managed-agent card declares a model per agent, but the operator keeps
       control: every agent runs on the model selected in the Pythia UI for
       text generation. The card's own model stays as the fallback when the UI
       carries no selection.
  }
  if Length(FState.Models.Items) <= TEXT_GENERATION_INDEX then
    Exit;

  var UiModel := FState.Models.Items[TEXT_GENERATION_INDEX].Model.Trim;
  if UiModel.IsEmpty then
    Exit;

  Def.Agent.Model := UiModel;
  Def.Coordinator.Model := UiModel;
  for var I := 0 to High(Def.SubAgents) do
    Def.SubAgents[I].Model := UiModel;

  {--- The provisioner keys resolved agents by card identity, version and hash;
       fold the UI model into the card id before hashing so switching model
       re-provisions fresh agents instead of reusing previous ones. }
  Def.CardId := Def.CardId + ':model=' + UiModel;
end;

function TAgentTurn.BuildTurnPrompt(const Def: TAgentCardDefinition): string;
begin
  {--- Local-project agent: tell the agent where the uploaded project tree
       actually lives. Read-only cards keep the original lightweight review
       guardrail; sandbox-edit cards must not receive a contradictory
       "do not write" instruction because their own card controls edit policy. }
  if Def.Session.Folder.Defined then
    begin
      var ProjectIntro :=
        'The local project has been uploaded into this session sandbox under ' +
        'the /mnt/session/uploads directory (project root normally ' +
        TFolderUploader.SandboxRoot + '; if that exact path is absent, locate ' +
        'it by globbing under /mnt/session/uploads).' + sLineBreak;

      if AgentCanModifySandbox(Def) then
        Result :=
          ProjectIntro +
          'Follow the selected agent definition exactly. If an edit is needed, ' +
          'modify only the sandbox copy under /mnt/session/uploads, never the ' +
          'user''s local disk directly. If you propose local application, return ' +
          'the local apply manifest and unified diff between the required ' +
          'PYTHIA_* markers.' + sLineBreak + sLineBreak +
          'User request:' + sLineBreak +
          FState.Text
      else
        Result :=
          ProjectIntro +
          'Review only this project, using read, glob and grep; do not use bash.' +
          sLineBreak +
          'Deliver the review as your final text answer: do not write, create ' +
          'or save any file. Keep it lightweight - inspect at most 12 files.' +
          sLineBreak + sLineBreak +
          'User request:' + sLineBreak +
          FState.Text;
      Exit;
    end;

  Result := FState.Text;
end;

procedure TAgentTurn.TryOfferLocalApply;
begin
  if not FOfferLocalApply then
    Exit;

  if FSelectedProjectFolder.Trim.IsEmpty then
    Exit;

  var Plan: TLocalApplyPlan;
  var ExtractError := '';
  if not TLocalApply.TryExtract(FState.TextBuffer, Plan, ExtractError) then
    begin
      if not ExtractError.Trim.IsEmpty then
        FDisplay.ErrorStatus('Local patch proposal ignored', ExtractError);
      Exit;
    end;

  FDisplay.Status('Local patch detected');

  var DialogRequest := TWebDecisionDlgRequest.Markdown(
    'Apply sandbox patch locally?',
    TLocalApply.PreviewMarkdown(Plan),
    [
      TWebDecisionDlgButton.Create(
        'apply',
        'Apply locally',
        wdrDefault),
      TWebDecisionDlgButton.Create(
        'skip',
        'Skip',
        wdrCancel)
    ]);
  DialogRequest.FooterText :=
    'The cloud sandbox has already been processed. This step applies the returned diff to the selected local folder.';

  var Decision := FBrowser.WebDecisionDlg(DialogRequest);
  if not (Decision.Success and SameText(Decision.ChoiceId, 'apply')) then
    begin
      FDisplay.Status(
        'Local patch skipped',
        'The sandbox change was not applied to the selected local folder.');
      Exit;
    end;

  var Detail := '';
  if TLocalApply.TryApply(Plan, FSelectedProjectFolder, Detail) then
    FDisplay.Status('Local patch applied', Detail)
  else
    FDisplay.ErrorStatus('Local patch failed', Detail);
end;

function TAgentTurn.CreateSession(const Def: TAgentCardDefinition): string;
begin
  {--- Upload the selected local project (blocking) before the session is created, so
       its file ids are already available when the resources are assembled. }
  var FolderFiles: TArray<TFolderUploadedFile> := [];
  if Def.Session.Folder.Defined then
    FolderFiles := UploadFolderFiles(Def.Session.Folder.Path);

  var Sess := FClient.Sessions.Create(
    procedure (Params: TSessionCreateParams)
    begin
      Params.Agent(FResolved.AgentId);
      Params.EnvironmentId(FResolved.EnvironmentId);
      if not Def.Session.Title.Trim.IsEmpty then
        Params.Title(Def.Session.Title);

      var Metadata := TAgentCloudMetadata.Build(
        Def, FResolved.RegistryEntryId, 'session', 'session', 'session');
      try
        Params.Metadata(Metadata);
      finally
        Metadata.Free;
      end;

      var Resources: TArray<TSessionResourceParams> := [];

      {--- Local-project agent: every uploaded file is mounted as its own
           "file" resource, rebuilding the project tree under MountRoot. }
      for var FolderFile in FolderFiles do
        begin
          var FileRes := TSessionFileResourceParams.New
            .FileId(FolderFile.FileId)
            .MountPath(FolderFile.MountPath);
          Resources := Resources + [FileRes];
        end;

      if Length(Resources) > 0 then
        Params.Resources(Resources);
    end);
  try
    Result := Sess.Id;
  finally
    Sess.Free;
  end;
end;

procedure TAgentTurn.RecordTrace(const ModelId: string);
begin
  {--- The session id is persisted into the turn's prompt JSON so the next
       turn can reuse the same session (IContext.LastAgentSessionId).
  }
  var SessId := FSessionId;
  var AgentId := FResolved.AgentId;
  var EnvId := FResolved.EnvironmentId;
  var CardId := FCardId;
  var CardVersion := FCardVersion;
  var DefinitionHash := FDefinitionHash;
  var UserMsg := FState.Text;

  Queue(
    procedure
    begin
      FState.Model := ModelId;

      var Trace := TJsonWriter.NewObject;
      Trace.SetString('managed_agent.session_id', SessId);
      Trace.SetString('managed_agent.agent_id', AgentId);
      Trace.SetString('managed_agent.environment_id', EnvId);
      Trace.SetString('managed_agent.card_id', CardId);
      Trace.SetString('managed_agent.card_version', CardVersion);
      Trace.SetString('managed_agent.definition_hash', DefinitionHash);
      Trace.SetString('user_message', UserMsg);
      FState.JsonRequest := Trace.ToJson;
    end);
end;

procedure TAgentTurn.RecordEventTrace(const EventData: string);
begin
  var Item := EventData.Trim;
  if Item.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(Item);
  if Reader.IsValid then
    Item := Reader.ToJson.Trim;

  if Item.IsEmpty then
    Exit;

  FEventTrace := FEventTrace + [Item];

  {--- Append one raw event per line (JSONL) for offline diagnosis. }
  if not FEventLogPath.IsEmpty then
    try
      TFile.AppendAllText(FEventLogPath, Item + sLineBreak, TEncoding.UTF8);
    except
    end;
end;

procedure TAgentTurn.SendEventsSafe(const Events: array of TSessionEventParams);
var
  Arr: TArray<TSessionEventParams>;
begin
  SetLength(Arr, Length(Events));
  for var I := 0 to High(Events) do
    Arr[I] := Events[I];

  try
    var Response := FClient.Sessions.Events.Send(FSessionId,
      procedure (Params: TSessionSendEventsParams)
      begin
        Params.Events(Arr);
      end);
    Response.Free;
  except
    on E: Exception do
      begin
        var ErrorText := E.Message.Trim;
        if ErrorText.IsEmpty then
          ErrorText := E.ClassName;
        FStreamError := 'Failed to send a session event: ' + ErrorText;

        if Assigned(FTransport) then
          FTransport.Abort;

        FDisplay.BrowserError(FStreamError);
      end;
  end;
end;

procedure TAgentTurn.SendTurn(const Def: TAgentCardDefinition);
begin
  var MsgEvent: TSessionEventParams :=
    TSessionUserMessageEventParams.New.Text(BuildTurnPrompt(Def));

  SendEventsSafe([MsgEvent]);
end;

procedure TAgentTurn.WorkerBody;
begin
  try
    var CardsFile := FBrowser.GetAgentCardsFileName;
    if not FileExists(CardsFile) then
      raise Exception.Create('Agent cards file not found: ' + CardsFile);

    {--- Diagnostic: mirror every raw session event to a log file next to the
         agent-cards file, truncated per turn. It captures the full stream
         even when the turn does not finalize cleanly. }
    FEventLogPath := TPath.Combine(
      TPath.GetDirectoryName(CardsFile), 'agent-session-events.log');
    try
      TFile.WriteAllText(FEventLogPath, '', TEncoding.UTF8);
    except
      FEventLogPath := '';
    end;

    var Def: TAgentCardDefinition;
    if not TAgentCardReader.TryRead(
         TFileIOHelper.LoadFromFile(CardsFile), FCardId,
         TPath.GetDirectoryName(CardsFile), Def) then
      raise Exception.Create('Agent card not found or invalid: ' + FCardId);

    ApplyUiModel(Def);
    Def.DefinitionHash := TAgentDefinitionFingerprint.ComputeHash(Def);
    FCardVersion := Def.Version;
    FDefinitionHash := Def.DefinitionHash;

    {--- Local-project agents use the project selected in the input bubble,
         not any folder value left in the card JSON. Fail before provisioning
         server-side resources when the selected project is missing/invalid. }
    BindSelectedProjectFolder(Def);
    if FOfferLocalApply then
      FPriorSessionId := '';

    {--- Blocking: Environment + Agent(s). }
    FResolved := FProvisioner.Resolve(Def);
    FDisplay.Status('Managed agent provisioned');

    if not FProvisioner.CanReuseSession(Def, FPriorSessionId) then
      FPriorSessionId := '';

    var IsFirstTurn := FPriorSessionId.Trim.IsEmpty;
    if IsFirstTurn then
      FSessionId := CreateSession(Def)
    else
      FSessionId := FPriorSessionId;

    FProvisioner.RecordSession(Def, FResolved, FSessionId, 'running');

    FDisplay.Status('Managed agent session ready', FSessionId);

    var ModelId := Def.Agent.Model;
    if Def.Kind = ackMultiagent then
      ModelId := Def.Coordinator.Model;
    RecordTrace(ModelId);

    FDisplay.Status('Sending agent request');

    SendTurn(Def);
    if not FStreamError.Trim.IsEmpty then
      raise Exception.Create(FStreamError);

    {--- Stream the session events until the turn completes. }
    FDisplay.Status('Streaming managed agent events');

    FTransport := TSessionStreamTransport.Create(FClient);

    FTransport.Start(FSessionId,
      procedure (const EventData: string)
      begin
        HandleEvent(EventData);
      end,
      procedure (ErrorMsg: string)
      begin
        HandleDone(ErrorMsg);
      end);
  except
    on E: Exception do
      begin
        var StartupError := E.Message;
        FProvisioner.UpdateSessionStatus(FSessionId, 'failed');
        FDisplay.ErrorStatus('Managed agent startup error', StartupError);
        Finalize(True, StartupError);
      end;
  end;
end;

function TAgentTurn.ToolUseKey(const Ev: TSessionEvent): string;
begin
  Result := Ev.ToolUseId.Trim;
  if Result.IsEmpty then
    Result := Ev.CustomToolUseId.Trim;

  if Result.IsEmpty then
    Result := Ev.MCPToolUseId.Trim;

  if Result.IsEmpty then
    Result := Ev.EventId.Trim;
end;

function TAgentTurn.ToolTitle(const Ev: TSessionEvent): string;
begin
  var ToolName := Ev.ToolName.Trim;
  if SameText(ToolName, 'glob') or SameText(ToolName, 'grep') then
    Result := 'Project search'
  else
  if SameText(ToolName, 'read') then
    Result := 'Reading identified file'
  else
  if SameText(ToolName, 'edit') then
    Result := 'Sandbox edit request'
  else
    Result := ToolName.Replace('_', ' ');

  if Result.IsEmpty then
    Result := 'Tool call';

  if not Ev.MCPServerName.Trim.IsEmpty then
    Result := Ev.MCPServerName.Trim + ': ' + Result;

  {--- Append the salient input argument (search query, url, command, path)
       so the persisted tool block is informative, not just the bare name. }
  if Ev.InputJson.Trim.IsEmpty then
    Exit;

  var Reader := TJsonReader.Parse(Ev.InputJson);
  if not Reader.IsValid then
    Exit;

  var Arg := Reader.AsString('query');

  if Arg.Trim.IsEmpty then
    Arg := Reader.AsString('url');

  if Arg.Trim.IsEmpty then
    Arg := Reader.AsString('command');

  if Arg.Trim.IsEmpty then
    Arg := Reader.AsString('path');

  if Arg.Trim.IsEmpty then
    Arg := Reader.AsString('pattern');

  if Arg.Trim.IsEmpty then
    Arg := Reader.AsString('file');

  if not Arg.Trim.IsEmpty then
    Result := Result + ' - ' + Arg.Trim;
end;

function TAgentTurn.ToolOutputText(const Ev: TSessionEvent): string;
begin
  Result := Ev.Text.Trim;
  if Result.IsEmpty then
    Result := Ev.ResultJson.Trim;

  if Result.IsEmpty then
    Result := Ev.ContentJson.Trim;

  if Result.IsEmpty then
    Result := Ev.ErrorJson.Trim;
end;

procedure TAgentTurn.RememberToolUse(const Ev: TSessionEvent);
begin
  if not Ev.EventId.Trim.IsEmpty then
    FPendingToolEvents.AddOrSetValue(Ev.EventId.Trim, Ev);

  var Key := ToolUseKey(Ev);
  if not Key.Trim.IsEmpty then
    FPendingToolEvents.AddOrSetValue(Key.Trim, Ev);
end;

function TAgentTurn.TryKey(const Key: string;
  out ToolEv: TSessionEvent): Boolean;
begin
  Result := False;
  if Key.Trim.IsEmpty then
    Exit;

  Result := FPendingToolEvents.TryGetValue(Key.Trim, ToolEv);
  if Result then
    begin
      FPendingToolEvents.Remove(Key.Trim);

      var ToolKey := ToolUseKey(ToolEv);
      if not ToolKey.Trim.IsEmpty then
        FPendingToolEvents.Remove(ToolKey.Trim);

      if not ToolEv.EventId.Trim.IsEmpty then
        FPendingToolEvents.Remove(ToolEv.EventId.Trim);
    end;
end;

function TAgentTurn.TryResolveActionTool(const Ev: TSessionEvent;
  out ToolEv: TSessionEvent): Boolean;
begin
  ToolEv := Default(TSessionEvent);

  for var ActionId in Ev.ActionEventIds do
    if TryKey(ActionId, ToolEv) then
      Exit(True);

  if TryKey(Ev.ToolUseId, ToolEv) then
    Exit(True);

  Result := TryKey(Ev.EventId, ToolEv);
end;

function TAgentTurn.ThreadTitle(const Ev: TSessionEvent): string;
begin
  if (not Ev.FromAgentName.Trim.IsEmpty) and
     (not Ev.ToAgentName.Trim.IsEmpty) then
    Exit(Format('Sub-agent: %s -> %s',
      [Ev.FromAgentName.Trim, Ev.ToAgentName.Trim]));

  if not Ev.ToAgentName.Trim.IsEmpty then
    Exit('Sub-agent: ' + Ev.ToAgentName.Trim);

  if not Ev.AgentName.Trim.IsEmpty then
    Exit('Sub-agent: ' + Ev.AgentName.Trim);

  if not Ev.EventType.Trim.IsEmpty then
    Exit('Session thread: ' + Ev.EventType.Trim.Replace('_', ' '));

  Result := 'Session thread update';
end;

function TAgentTurn.IsEmptyMessagePlaceholder(const Text: string): Boolean;
begin
  Result := SameText(Text.Trim, '[empty message]');
end;

function TAgentTurn.BuildConfirmation(const Ev: TSessionEvent): TSessionEventParams;
begin
  var Allowed := False;
  var AutoDeny := FFinishRequested;
  var ToolName := ToolTitle(Ev);
  var Details := '';

  if not Ev.AgentName.Trim.IsEmpty then
    Details := Details + 'Agent: ' + Ev.AgentName.Trim + sLineBreak;

  if not Ev.ToolName.Trim.IsEmpty then
    Details := Details + 'Tool: ' + Ev.ToolName.Trim + sLineBreak;

  if not Ev.ToolUseId.Trim.IsEmpty then
    Details := Details + 'Tool use id: ' + Ev.ToolUseId.Trim + sLineBreak;

  if not Ev.SessionThreadId.Trim.IsEmpty then
    Details := Details + 'Session thread id: ' + Ev.SessionThreadId.Trim + sLineBreak;

  if not Ev.InputJson.Trim.IsEmpty then
    Details := Details + sLineBreak + 'Input:' + sLineBreak + Ev.InputJson.Trim + sLineBreak;

  FDisplay.ToolUse(ToolUseKey(Ev), ToolName, False);

  if not AutoDeny then
    begin
      {--- Block the stream thread on the operator's decision: the agent is
           waiting for it anyway. WebDecisionDlg marshals the UI work to the
           WebView and resumes here when the browser response is received. }
      var DialogContent := Format(
        '**The managed agent is requesting permission to use:**' + sLineBreak +
        sLineBreak +
        '### %s' + sLineBreak +
        sLineBreak +
        '```text' + sLineBreak +
        '%s' +
        '```',
        [ToolName, Details]);

      var DialogRequest := TWebDecisionDlgRequest.Markdown(
        S_DEMO_TOOL_CONFIRMATION_TITLE,
        DialogContent,
        [
          TWebDecisionDlgButton.Create(
            'allow',
            S_DEMO_TOOL_ALLOW,
            wdrDefault),
          TWebDecisionDlgButton.Create(
            'deny',
            S_DEMO_TOOL_DENY,
            wdrCancel)
        ]);
      DialogRequest.FooterText := S_DEMO_TOOL_ALLOW_CALL;

      var Decision := FBrowser.WebDecisionDlg(
        DialogRequest);

      Allowed :=
        Decision.Success and
        SameText(Decision.ChoiceId, 'allow');
    end;

  if not Allowed then
    begin
      FFinishRequested := True;
      Inc(FDeniedToolRequestCount);
    end;

  var Confirmation := TSessionUserToolConfirmationEventParams.New
    .ToolUseId(Ev.ToolUseId);

  if Allowed then
    Confirmation.Result('allow')
  else
    begin
      Confirmation.Result('deny');
      Confirmation.DenyMessage(
        Format('The operator requested the session to stop and denied this ' +
          'tool call. This is denied tool request #%d after the stop request. ' +
          'Do not request additional tools for this session. Immediately ' +
          'produce the best possible final answer from the ' +
          'information already gathered. If the available evidence is ' +
          'insufficient, say so explicitly.', [FDeniedToolRequestCount]));
    end;

  if not Ev.SessionThreadId.Trim.IsEmpty then
    Confirmation.SessionThreadId(Ev.SessionThreadId);

  var DecisionText := 'Allowed by operator.';
  if not Allowed then
    begin
      if AutoDeny then
        DecisionText :=
          'Automatically denied: the operator already requested the final report.'
      else
        DecisionText :=
          'Denied by operator; future tool requests will be denied until the agent finishes.';
    end;

  FDisplay.ToolResultStatus(ToolUseKey(Ev), ToolName, DecisionText);

  {--- The caller batches the actual Send: every confirmation of one
       requires_action must travel in a single request (see HandleConfirmationRequest). }
  Result := Confirmation;
end;

procedure TAgentTurn.HandleConfirmationRequest(const Ev: TSessionEvent);
var
  ToolEv: TSessionEvent;
  Confirmations: TArray<TSessionEventParams>;
begin
  {--- Direct case: the event itself already names the tool. }
  if (not Ev.ToolName.Trim.IsEmpty) and (not Ev.ToolUseId.Trim.IsEmpty) then
    begin
      SendEventsSafe([BuildConfirmation(Ev)]);
      Exit;
    end;

  {--- A requires_action event batches EVERY tool use of the step in
       stop_reason.event_ids, and the session expects ONE response covering
       the WHOLE batch. Confirmations sent one request at a time — or a
       partial set — are rejected by the server as a "tool use mismatch".

       So: wait until the tool_use event of every listed id has arrived (the
       session re-emits requires_action, so a later pass sees the full set),
       then confirm them all in a single Send. }
  if Length(Ev.ActionEventIds) = 0 then
    Exit;

  for var ActionId in Ev.ActionEventIds do
    if not FPendingToolEvents.ContainsKey(ActionId.Trim) then
      Exit;

  Confirmations := [];
  while TryResolveActionTool(Ev, ToolEv) do
    Confirmations := Confirmations + [BuildConfirmation(ToolEv)];

  if Length(Confirmations) > 0 then
    SendEventsSafe(Confirmations);
end;

procedure TAgentTurn.HandleEvent(const EventData: string);
begin
  {--- Capture every raw event, before any filtering or classification, so the
       turn keeps a complete and replayable trace. Persisted as JsonResponse:
       without it the turn carries only the final text and cannot be rebuilt
       step by step when the chat is reloaded.
  }
  RecordEventTrace(EventData);

  if FCancelled then
    Exit;

  {--- Escape requested by the user: interrupt the agent, stop streaming and
       finalize with whatever was produced so far. }
  if FBrowser.Escape then
    begin
      FCancelled := True;
      SendEventsSafe([TSessionUserInterruptEventParams.New]);

      {--- Record the interruption in the block stream SYNCHRONOUSLY so it is
           part of the display block aggregator before HandleDone finalizes;
           only the browser update is marshalled to the UI thread. }
      FDisplay.AssistantText(
        '_' + S_DEMO_INTERRUPTED_BY_USER + '_',
        S_DEMO_INTERRUPTED_BY_USER);

      {--- Close the stream; HandleDone performs the single finalization. }
      FTransport.Abort;
      Exit;
    end;

  var Ev := TSessionEventParser.Parse(EventData);

  case Ev.Kind of
    {--- For every event kind: the display block and the state buffer are
         updated SYNCHRONOUSLY here, on the stream thread. The display
         aggregator therefore holds every block by the time HandleDone runs
         the single finalization — the persisted snapshot can never be
         partial. Only the browser (UI-thread-only) calls are marshalled
         through the Pythia turn-display bridge. }
    sekAssistantText:
      if (not Ev.Text.IsEmpty) and not IsEmptyMessagePlaceholder(Ev.Text) then
        begin
          FState.AddStreamedText(Ev.Text);
          FDisplay.AssistantDelta(Ev.Text);
        end;

    sekReasoning:
      if not Ev.Text.IsEmpty then
        begin
          FState.AddStreamedThinking(Ev.Text);
          FDisplay.ReasoningDelta(Ev.Text);
        end;

    sekToolUse:
      begin
        RememberToolUse(Ev);

        var Title := ToolTitle(Ev);
        FDisplay.ToolUse(ToolUseKey(Ev), Title);
      end;

    sekToolResult:
      begin
        {--- Merge the result into the tool block opened by sekToolUse so a
             single entry carries the tool identity and its output. }
        var OutputText := ToolOutputText(Ev);
        var ErrorTitle := ToolTitle(Ev);
        var IsToolError := Ev.IsError;
        FDisplay.ToolResult(
          ToolUseKey(Ev), ErrorTitle, OutputText, IsToolError);
      end;

    sekToolConfirmationRequest:
      HandleConfirmationRequest(Ev);

    sekOutcome:
      begin
        var Title := 'Outcome';
        if not Ev.OutcomeResult.Trim.IsEmpty then
          Title := Title + ': ' + Ev.OutcomeResult.Trim;

        var Details := Ev.Explanation.Trim;
        if Details.IsEmpty then
          Details := Ev.Text.Trim;

        if not (Details.IsEmpty and SameText(Title, 'Outcome')) then
          FDisplay.Status(Title, Details);
      end;

    sekThread:
      begin
        var Title := ThreadTitle(Ev);
        var ThreadText := Ev.Text.Trim;
        FDisplay.Status(Title, ThreadText);
      end;

    sekError:
      begin
        var Msg := Ev.Text;
        if Msg.Trim.IsEmpty then
          Msg := Ev.Explanation;
        if Msg.Trim.IsEmpty then
          Msg := Ev.ErrorJson;
        if Msg.Trim.IsEmpty then
          Msg := 'The agent session reported an error.';
        FDisplay.ToolResult('', 'Managed agent error', Msg, True);
        {--- Record the error and close the stream; HandleDone finalizes
             once every event has been processed. }
        FStreamError := Msg;
        FTransport.Abort;
      end;

    sekDone:
      {--- Turn complete: only request the stream to close. Finalization is
           done by HandleDone, after every event has been processed, so the
           persisted block snapshot is never partial. }
      FTransport.Abort;
  end;
end;

procedure TAgentTurn.HandleDone(const ErrorMsg: string);
begin
  {--- The single finalization point. HandleDone runs only after the transport
       has delivered (and the flow has queued) every event, so the cloned
       block snapshot taken in Finalize is always complete.
  }
  if FCancelled then
    FProvisioner.UpdateSessionStatus(FSessionId, 'interrupted')
  else
  if not FStreamError.IsEmpty or not ErrorMsg.IsEmpty then
    FProvisioner.UpdateSessionStatus(FSessionId, 'failed')
  else
    FProvisioner.UpdateSessionStatus(FSessionId, 'completed');

  if not FStreamError.IsEmpty then
    Finalize(True, FStreamError)
  else
  if not ErrorMsg.IsEmpty then
    Finalize(True, ErrorMsg)
  else
    begin
      TryOfferLocalApply;
      Finalize(False, '');
    end;
end;

procedure TAgentTurn.Execute;
const
  AGENT_STARTUP_TEXT = 'Starting managed agent...';
begin
  if Length(FState.Integration.Agents) = 0 then
    begin
      Finalize(True, 'No agent card is selected for this turn.');
      Exit;
    end;

  FCardId := FState.Integration.Agents[0].Id;

  {--- Read here, on the UI thread: LastAgentSessionId walks the persistent
       chat history. Reuse only when the previous managed-agent turn used the
       same selected card; switching cards must provision a fresh session. }
  FPriorSessionId := '';
  if SameText(FContext.LastAgentCardId, FCardId) then
    FPriorSessionId := FContext.LastAgentSessionId;

  FDisplay.AssistantText(AGENT_STARTUP_TEXT, True);
  FDisplay.ToolStatus('Provisioning the managed agent');

  if Assigned(FWorker) then
    raise Exception.Create('Managed agent turn is already running.');

  FWorker := TAgentTurnWorker.Create(Self);
  FWorker.Start;
end;

{ TAgentSessionFlow }

constructor TAgentSessionFlow.Create(const ABrowser: IPythiaBrowser;
  const AContext: IContext; const AClient: IAnthropic;
  const AProvisioner: IAgentProvisioner);
begin
  inherited Create;
  FBrowser := ABrowser;
  FContext := AContext;
  FClient := AClient;
  FProvisioner := AProvisioner;
end;

destructor TAgentSessionFlow.Destroy;
begin
  FCurrentTurn.Free;
  inherited;
end;

procedure TAgentSessionFlow.Run(const AState: TStateBuffer;
  const AOnFinalize: TManagedItemFinalizeProc);
begin
  {--- Pythia serializes turns (the send button is disabled while one runs),
       so the previous turn has fully finalized and its transport thread has
       ended before the next Run begins.
  }
  FreeAndNil(FCurrentTurn);

  var Turn := TAgentTurn.Create(
    FBrowser, FContext, FClient, FProvisioner, AState, AOnFinalize);
  FCurrentTurn := Turn;
  Turn.Execute;
end;

end.
