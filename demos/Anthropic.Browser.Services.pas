unit Anthropic.Browser.Services;

interface

uses
  System.SysUtils,
  WVPythia.Chat.Interfaces, WVPythia.Chat.ManagedFlow, WVPythia.TextFile.Helper,
  WVPythia.Strs, WVPythia.Vendors.Services, WVPythia.Chat.Consts,
  Anthropic, Anthropic.Types, Anthropic.Browser.Helpers, Anthropic.Async.Promise,

  Clipbrd;

const
  ABORTED_INDICATOR = 'aborted';

type
  TFinalizeData = record
    Model: string;
    Response: string;
    Reasoning: string;
    JsonRequest: string;
    JsonResponse: string;
    Error: Boolean;
    ErrorMessage: string;

    class function FromState(
      const AState: TStateBuffer): TFinalizeData; overload; static;

    class function FromSuccess(
      const AValue: TEventData;
      const AState: TStateBuffer): TFinalizeData; overload; static;

    class function FromException(
      const E: Exception;
      const AState: TStateBuffer): TFinalizeData; overload; static;

    procedure Emit(const AOnFinalize: TManagedItemFinalizeProc);
  end;

  TAnthropicServices = class(TInterfacedObject, IVendorServices)
  const
    API_KEY_NAME = 'anthropic';
  private
    FClient: IAnthropic;
    FBrowser: IPythiaBrowser;

    function IsEventSuitable(const Event: TChatStream): Boolean;

    function ToolsBuilder(
      const AState: TStateBuffer;
      const Params: TChatParams): TAnthropicServices;

    procedure ThinkingBuilder(
      const AState: TStateBuffer;
      out Effort: string;
      const Params: TChatParams);

    procedure OutputConfigBuilder(
      const AState: TStateBuffer;
      const Effort: string;
      const Params: TChatParams);

    function BuildPayload(
      const AState: TInputPromptState;
      const Model: string;
      State: TStateBuffer): TChatParamProc;

    function BuildSessionCallbacks(State: TStateBuffer): TSessionCallbacksStream;

    function BuildAndCheckPayload(
      const AState: TInputPromptState;
      State: TStateBuffer):TChatParamProc;

  public
    constructor Create(const ABrowser: IPythiaBrowser);

    procedure UpdateApiKey;

    procedure AsyncAwaitStreamChat(
      const AState: TInputPromptState;
      const AOnFinalize: TManagedItemFinalizeProc);
  end;

var
  AnthropicVendor: IVendorServices;

implementation

{ TAnthropicServices }

procedure TAnthropicServices.AsyncAwaitStreamChat(
  const AState: TInputPromptState; const AOnFinalize: TManagedItemFinalizeProc);
begin
  {--- Main entry point for a streamed chat request. }
  var State := TStateBuffer.FromState(AState);

  {--- Select model for "id":"textGeneration" (index 1) }
  State.Model := State.Models.Items[TEXT_GENERATION_INDEX].Model;

  {--- Build and validate the Payload procedure before starting the stream.
       The returned Payload is the fresh instance used by the SDK. }
  var Payload := BuildAndCheckPayload(AState, State);

  {--- Wire streaming callbacks to the browser/UI layer. }
  var SessionCallbacks := BuildSessionCallbacks(State);

  {--- Normalize promise completion into one finalize contract. }
  var Promise := FClient.Chat.AsyncAwaitCreateStream(Payload, SessionCallbacks);

  Promise
    .&Then(
      procedure (Value: TEventData)
      begin
        {--- Finalize success from the aggregated SDK result. }
        TFinalizeData
          .FromSuccess(Value, State)
          .Emit(AOnFinalize);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        if not SameText(E.Message, ABORTED_INDICATOR) then
          begin
            State.Error := True;
            State.ErrorMessage := E.Message;

            TFinalizeData
              .FromException(E, State)
              .Emit(AOnFinalize);
            Exit;
          end;

        {--- Handle cancellation separately. }
        FBrowser.DisplayWarning(S_ABORTED_OPERATION);
        FBrowser.Display(S_ABORTED, False);

        TFinalizeData
          .FromState(State)
          .Emit(AOnFinalize);
      end);
end;

function TAnthropicServices.BuildPayload(
  const AState: TInputPromptState;
  const Model: string;
  State: TStateBuffer): TChatParamProc;
var
  Effort: string;
begin
  {--- Build messages via shared helpers. }
  var Messages := TMessageBuilder.GetMessages(AState);

  {--- Build and snapshot the request payload. }
  Result :=
    procedure (Params: TChatParams)
    begin
      Params
        .Model(Model)
        .Messages(Messages)
        .Stream;

      TRequestSettingsBuilder.Apply(State, Params);
      ToolsBuilder(State, Params);
      ThinkingBuilder(State, Effort, Params);
      OutputConfigBuilder(State, Effort, Params);

      {--- Persist the final request JSON before execution. }
      State.JsonRequest := Params.ToFormat();
    end;
end;

function TAnthropicServices.BuildSessionCallbacks(
  State: TStateBuffer): TSessionCallbacksStream;
begin
  Result :=
    function : TPromiseChatStream
    begin
      Result.Sender := nil;

      Result.OnProgress :=
        procedure (Sender: TObject; Event: TChatStream)
        begin
          {--- Ignore events without usable live deltas. }
          if not IsEventSuitable(Event) then
            Exit;

          var Delta := Event.ContentBlockDelta.Delta;

          {--- Accumulate state and render deltas immediately. }
          State.AddStreamedText(Delta.Text);
          State.AddStreamedThinking(Delta.Thinking);
          State.AddJsonResponse(Event.JSONResponse);

          FBrowser.DisplayStream(Delta.Text, Delta.Thinking, False);
         end;

      Result.OnDoCancel :=
        function : Boolean
        begin
          {--- Poll browser escape state for cancellation. }
          Result := FBrowser.Escape;
        end;

      Result.OnCancellation :=
        function (Sender: TObject): string
        begin
          {--- Reset UI flags after cancellation is acknowledged. }
          FBrowser.Escape := False;
          FBrowser.Locked := False;
          State.AddStreamedText(S_ABORTED);
        end;

      Result.OnError :=
        function (Sender: TObject; Text: string): string
        begin
          {--- Surface stream-level errors reported through the callback layer.
               Lower-level transport failures may bypass this callback. }
          Result := Text;
          FBrowser.ReasoningHide;
          FBrowser.DisplayError(Text);
        end;
    end;
end;

function TAnthropicServices.BuildAndCheckPayload(
      const AState: TInputPromptState;
      State: TStateBuffer): TChatParamProc;
begin
  var Payload := BuildPayload(AState, State.Model, State);

  {--- Validate Payload generation using a disposable instance }
  var JsonPayload := TChatParams.Create;
  try
    {--- BuildPayload produces the Payload procedure ultimately used by the SDK.
         Executing such a procedure is required to validate payload generation,
         but it may consume its captured state or resources.

         Therefore this first Payload instance is disposable and used only for
         validation. A fresh instance is built afterwards and returned to the
         caller. }
    Payload(JsonPayload);

    Result := BuildPayload(AState, State.Model, State);
  finally
    JsonPayload.Free;
  end;
end;

constructor TAnthropicServices.Create(const ABrowser: IPythiaBrowser);
var
  Anthropic_key: string;
begin
  {--- The service is built around two collaborators: the browser-facing UI
       abstraction and the Anthropic SDK client used for remote execution. }
  FBrowser := ABrowser;

  if not FBrowser.ApiKeySecretStore.ReadSecret(API_KEY_NAME, Anthropic_key) then
      FBrowser.TryHandleAsCommand(Format('/api-key new %s', [API_KEY_NAME]));

  FClient := TAnthropicFactory.CreateInstance(Anthropic_key);
end;

function TAnthropicServices.IsEventSuitable(const Event: TChatStream): Boolean;
begin
  {--- Minimal structural validation for streamed events before reading nested
       delta fields. This isolates protocol-shape checks in one place. }
  Result :=
    Assigned(Event) and
    Assigned(Event.ContentBlockDelta) and
    Assigned(Event.ContentBlockDelta.Delta);
end;

procedure TAnthropicServices.OutputConfigBuilder(const AState: TStateBuffer;
  const Effort: string; const Params: TChatParams);
begin
  TThinkingBuilder.TryGetThinkingConfigParam(AState, Effort,
    procedure
    begin
      var AResult := TThinkingBuilder.GetTOutputConfig(AState, Effort);

      if Assigned(AResult) then
        Params
          .OutputConfig(AResult);
    end);
end;

procedure TAnthropicServices.ThinkingBuilder(const AState: TStateBuffer;
  out Effort: string;
  const Params: TChatParams);
begin
  TThinkingBuilder.TryGetOutputConfig(AState, Effort,
    procedure
    begin
      Params
        .Thinking( TThinkingConfigParam
            .New('adaptive')
        )
    end);
end;

function TAnthropicServices.ToolsBuilder(const AState: TStateBuffer;
  const Params: TChatParams): TAnthropicServices;
begin
  TToolsBuilder.TryToBuild(AState,
    procedure
    var
      Tools: TArray<TToolUnion>;
    begin
      {--- Tool registration is intentionally additive so other tool types
           can later be introduced without changing the control flow. }
      Tools := [];
      if AState.WebSearch then
        Tools := Tools + [TWebSearchTool20250305.New.MaxUses(5)];

      Params
        .Tools(Tools)
    end);
  Result := Self;
end;

procedure TAnthropicServices.UpdateApiKey;
var
  Anthropic_key: string;
begin
  if not FBrowser.ApiKeySecretStore.ReadSecret(API_KEY_NAME, Anthropic_key) then
    begin
      FClient.API.Token := '';
      Exit;
    end;

  FClient.API.Token := Anthropic_key;
  FBrowser.DisplaySuccess('Anthropic client is up to date.')
end;

{ TFinalizeData }

class function TFinalizeData.FromState(
  const AState: TStateBuffer): TFinalizeData;
begin
  {--- Rebuilds the final payload from the local stream buffer.
       This path is used when the request stops before a canonical success
       object is available, such as cancellation. }
  Result.Model := AState.Model;
  Result.Response := AState.TextBuffer;
  Result.Reasoning := AState.ThinkingBuffer;
  Result.JsonRequest := AState.JsonRequest;
  Result.JsonResponse := AState.JsonResponse;
  Result.Error := AState.Error;
  Result.ErrorMessage := AState.ErrorMessage;
end;

class function TFinalizeData.FromSuccess(
  const AValue: TEventData;
  const AState: TStateBuffer): TFinalizeData;
begin
  {--- On success, text and reasoning come from the SDK terminal event, while
       request/response JSON traces remain sourced from the local state buffer
       accumulated during the stream. }
  Result.Model := AState.Model;
  Result.Response := AValue.Text;
  Result.Reasoning := AValue.Thought;
  Result.JsonRequest := AState.JsonRequest;
  Result.JsonResponse := AState.JsonResponse;
  Result.Error := AState.Error;
  Result.ErrorMessage := AState.ErrorMessage;
end;

class function TFinalizeData.FromException(
  const E: Exception;
  const AState: TStateBuffer): TFinalizeData;
begin
  {--- Exception finalization intentionally clears reasoning and promotes the
       exception message into the response field so existing rendering and
       storage flows can consume it without a separate error channel. }
  Result.Model := AState.Model;
  Result.Response := '<br>' + E.Message;
  Result.Reasoning := '';
  Result.JsonRequest := AState.JsonRequest;
  Result.JsonResponse := AState.JsonResponse;
  Result.Error := AState.Error;
  Result.ErrorMessage := AState.ErrorMessage;
end;

procedure TFinalizeData.Emit(const AOnFinalize: TManagedItemFinalizeProc);
begin
  {--- Converts the plain record into the managed result object expected by the
       surrounding flow infrastructure, then dispatches it through the caller's
       finalize callback if one was provided. }
  if not Assigned(AOnFinalize) then
    Exit;

  var ResponseFlow := TManagedItemLLMResult.New;
  try
    ResponseFlow
      .UsedModel(Model)
      .Response(Response)
      .Reasoning(Reasoning)
      .PromptJson(JsonRequest)
      .ResponseJson(JsonResponse)
      .Error(Error)
      .ErrorMessage(ErrorMessage);

    AOnFinalize(ResponseFlow);
  finally
    ResponseFlow.Free;
  end;
end;

end.
