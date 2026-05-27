unit Demo.Anthropic.Agent.Cleanup;

interface

uses
  Anthropic,
  Demo.Anthropic.Agent.Registry;

type
  TAnthropicAgentCloudCleanup = record
  public
    class procedure StartBackground(const Client: IAnthropic;
      const Registry: IAgentCloudRegistry;
      const Policy: TAgentCloudCleanupPolicy); static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Managed Agents cloud cleanup for the pythia-anthropic VCL demo.

  The Anthropic service owns this unit because cleanup is provider-specific:
  it deals with Managed Agents sessions, environments and agents, all tracked
  locally through Demo.Anthropic.Agent.Registry. The registry is the source of
  truth for what this demo created; this cleaner deliberately does not scan
  the whole Anthropic account.

  StartBackground is fire-and-forget and runs once when the Anthropic services
  are instantiated. The cleanup policy decides TTLs and whether sessions and
  environments are deleted or archived. Agents are archived, not deleted,
  matching the Managed Agents lifecycle.

  Ordering matters:
    1. expired terminal sessions are removed first;
    2. retired environments are cleaned only when their sessions are gone;
    3. retired agents are archived last.

  All cloud calls are best-effort. A 404 means the remote object was already
  removed, so the registry is marked "missing"; other errors are captured in
  the registry cleanup section for diagnosis without blocking app startup.

*)
{$ENDREGION}

uses
  System.SysUtils, System.Classes, System.DateUtils,
  Anthropic.Sessions, Anthropic.Environment, Anthropic.Agents;

type
  TAgentCloudCleanupFailure = record
  private
    class function IsNotFound(const E: Exception): Boolean; static;
  public
    class procedure Session(const Registry: IAgentCloudRegistry;
      const SessionId: string; const E: Exception); static;

    class procedure Environment(const Registry: IAgentCloudRegistry;
      const EntryId: string; const E: Exception); static;

    class procedure Agent(const Registry: IAgentCloudRegistry;
      const AgentId: string; const E: Exception); static;
  end;

{ TAgentCloudCleanupFailure }

class function TAgentCloudCleanupFailure.IsNotFound(
  const E: Exception): Boolean;
begin
  Result := Pos('404', E.Message) > 0;
end;

class procedure TAgentCloudCleanupFailure.Session(
  const Registry: IAgentCloudRegistry; const SessionId: string;
  const E: Exception);
begin
  if IsNotFound(E) then
    Registry.MarkSessionStatus(SessionId, 'missing')
  else
    Registry.MarkCleanupError(E.Message);
end;

class procedure TAgentCloudCleanupFailure.Environment(
  const Registry: IAgentCloudRegistry; const EntryId: string;
  const E: Exception);
begin
  if IsNotFound(E) then
    Registry.MarkEnvironmentStatus(EntryId, 'missing')
  else
    Registry.MarkCleanupError(E.Message);
end;

class procedure TAgentCloudCleanupFailure.Agent(
  const Registry: IAgentCloudRegistry; const AgentId: string;
  const E: Exception);
begin
  if IsNotFound(E) then
    Registry.MarkAgentStatus(AgentId, 'missing')
  else
    Registry.MarkCleanupError(E.Message);
end;

{ TAnthropicAgentCloudCleanup }

class procedure TAnthropicAgentCloudCleanup.StartBackground(
  const Client: IAnthropic; const Registry: IAgentCloudRegistry;
  const Policy: TAgentCloudCleanupPolicy);
begin
  if (Client = nil) or (Registry = nil) then
    Exit;

  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        Registry.MarkCleanupStarted;

        var NowUtc := TTimeZone.Local.ToUniversalTime(Now);
        var SessionCutoff := IncHour(NowUtc, -Policy.SessionTtlHours);
        for var SessionRef in Registry.ExpiredSessions(SessionCutoff) do
          try
            if Policy.DeleteSessions then
              begin
                var Deleted := Client.Sessions.Delete(SessionRef.SessionId);
                try
                  Registry.MarkSessionStatus(SessionRef.SessionId, 'deleted');
                finally
                  Deleted.Free;
                end;
              end
            else
              begin
                var Archived := Client.Sessions.Archive(SessionRef.SessionId);
                try
                  Registry.MarkSessionStatus(SessionRef.SessionId, 'archived');
                finally
                  Archived.Free;
                end;
              end;
          except
            on E: Exception do
              TAgentCloudCleanupFailure.Session(
                Registry, SessionRef.SessionId, E);
          end;

        var EnvCutoff := IncDay(NowUtc, -Policy.EnvironmentTtlDays);
        for var EnvRef in Registry.RetiredEnvironments(EnvCutoff) do
          try
            if Policy.DeleteEnvironments then
              begin
                var Deleted := Client.Environments.Delete(EnvRef.EnvironmentId);
                try
                  Registry.MarkEnvironmentStatus(EnvRef.EntryId, 'deleted');
                finally
                  Deleted.Free;
                end;
              end
            else
              begin
                var Archived := Client.Environments.Archive(EnvRef.EnvironmentId);
                try
                  Registry.MarkEnvironmentStatus(EnvRef.EntryId, 'archived');
                finally
                  Archived.Free;
                end;
              end;
          except
            on E: Exception do
              TAgentCloudCleanupFailure.Environment(
                Registry, EnvRef.EntryId, E);
          end;

        if Policy.ArchiveAgents then
          begin
            var AgentCutoff := IncDay(NowUtc, -Policy.RetiredAgentTtlDays);
            for var AgentRef in Registry.RetiredAgents(AgentCutoff) do
              try
                var Archived := Client.Agents.Archive(AgentRef.AgentId);
                try
                  Registry.MarkAgentStatus(AgentRef.AgentId, 'archived');
                finally
                  Archived.Free;
                end;
              except
                on E: Exception do
                  TAgentCloudCleanupFailure.Agent(
                    Registry, AgentRef.AgentId, E);
              end;
          end;

        Registry.MarkCleanupCompleted;
      except
        on E: Exception do
          Registry.MarkCleanupError(E.Message);
      end;
    end).Start;
end;

end.
