unit VCL.WVPythia.Services;

interface

uses
  Vcl.Dialogs,
  System.SysUtils,

  WVPythia.Adapter, WVPythia.Chat.ManagedFlow, WVPythia.ManagedItemService,
  Demo.Anthropic.Services;

type
  TVCLChatManagedItemDialogService = class(TCustomChatManagedItemDialogService)
  protected
    function DoSelectFunctionItem(
      out AItem: TChatManagedItemRef): Boolean; override;

    function DoSelectMCPItem(
      out AItem: TChatManagedItemRef): Boolean; override;

    function DoSelectSkillItem(
      out AItem: TChatManagedItemRef): Boolean; override;

    function DoSelectAgentItem(
      out AItem: TChatManagedItemRef): Boolean; override;

    function DoSelectCustomItem(
      out AItem: TChatManagedItemRef): Boolean; override;

    function DoActivateSystemSettings: Boolean; override;
    function DoActivateModelSelection: Boolean; override;

    function DoActivateInputState(
      const AState: TInputPromptState;
      const AOnFinalize: TManagedItemFinalizeProc): Boolean; override;

    function DoActivateCopyItemEvent(
      const APairId, AKind, AContent: string): Boolean; override;

    function DoActivateCodeCopyItemEvent(
      const ALang, AText: string): Boolean; override;

    function DoActivateNewChatEvent: Boolean; override;

    function DoActivateCardSettingsEvent: Boolean; override;

    function DoActivateAudioInputEvent: Boolean; override;
  end;

  TToolContainer = record
    class function SelectFunctionItem(out AItem: TChatManagedItemRef): Boolean; static;
    class function SelectMCPItem(out AItem: TChatManagedItemRef): Boolean; static;
    class function SelectSkillItem(out AItem: TChatManagedItemRef): Boolean; static;
    class function SelectAgentItem(out AItem: TChatManagedItemRef): Boolean; static;
    class function SelectCustomItem(out AItem: TChatManagedItemRef): Boolean; static;

    class function ActivateSystemPrompt: Boolean; static;
    class function ActivateModelSelection: Boolean; static;
    class function ActivateInputState(
      const AState: TInputPromptState;
      const AOnFinalize: TManagedItemFinalizeProc): Boolean; static;
    class function ActivateCopyItemEvent(
      const APairId, AKind, AContent: string): Boolean; static;
    class function ActivateCodeCopyItemEvent(
      const ALang, AText: string): Boolean; static;
    class function ActivateNewChatEvent: Boolean; static;
    class function ActivateCardSettingsEvent: Boolean; static;
    class function ActivateAudioInputEvent: Boolean; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  VCL adapter hooks for the pythia-anthropic demo.

  Pythia asks a TCustomChatManagedItemDialogService implementation to handle
  UI actions such as selecting managed items, opening settings panes, copying
  content and submitting the current input state. This unit provides the VCL
  demo implementation and keeps those callbacks small.

  The only production path here is ActivateInputState: it forwards the input
  prompt state to the global Anthropic vendor service, which then decides
  whether the turn goes through regular Messages streaming or Managed Agents.
  Most other handlers are demo placeholders that either let Pythia's standard
  selection UI continue or show a simple TODO dialog.

  The implementation uses Main from the implementation section on purpose.
  That creates a cross-interface dependency, but it keeps this sample compact:
  the real vendor logic remains in Demo.Anthropic.Services while this unit
  only bridges VCL UI events to that service.

*)
{$ENDREGION}

uses
  {--- Here we have a cross-interface dependency, which is something to be aware of.
       I took the liberty of doing it here because this is a demo. }
  Main;

{ TVCLChatManagedItemDialogService }

function TVCLChatManagedItemDialogService.DoActivateCodeCopyItemEvent(
  const ALang, AText: string): Boolean;
begin
  Result := TToolContainer.ActivateCodeCopyItemEvent(ALang, AText);
end;

function TVCLChatManagedItemDialogService.DoActivateCopyItemEvent(const APairId,
  AKind, AContent: string): Boolean;
begin
  Result := TToolContainer.ActivateCopyItemEvent(APairId, AKind, AContent);
end;

function TVCLChatManagedItemDialogService.DoActivateInputState(
  const AState: TInputPromptState;
  const AOnFinalize: TManagedItemFinalizeProc): Boolean;
begin
  Result := TToolContainer.ActivateInputState(AState, AOnFinalize);
end;

function TVCLChatManagedItemDialogService.DoActivateModelSelection: Boolean;
begin
  Result := TToolContainer.ActivateModelSelection;
end;

function TVCLChatManagedItemDialogService.DoActivateSystemSettings: Boolean;
begin
  Result := TToolContainer.ActivateSystemPrompt;
end;

function TVCLChatManagedItemDialogService.DoActivateAudioInputEvent: Boolean;
begin
  Result := TToolContainer.ActivateAudioInputEvent;
end;

function TVCLChatManagedItemDialogService.DoActivateCardSettingsEvent: Boolean;
begin
  Result := TToolContainer.ActivateCardSettingsEvent;
end;

function TVCLChatManagedItemDialogService.DoActivateNewChatEvent: Boolean;
begin
  Result := TToolContainer.ActivateNewChatEvent;
end;

function TVCLChatManagedItemDialogService.DoSelectAgentItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := TToolContainer.SelectAgentItem(AItem);
end;

function TVCLChatManagedItemDialogService.DoSelectCustomItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := TToolContainer.SelectCustomItem(AItem);
end;

function TVCLChatManagedItemDialogService.DoSelectFunctionItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := TToolContainer.SelectFunctionItem(AItem);
end;

function TVCLChatManagedItemDialogService.DoSelectMCPItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := TToolContainer.SelectMCPItem(AItem);
end;

function TVCLChatManagedItemDialogService.DoSelectSkillItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := TToolContainer.SelectSkillItem(AItem);
end;

{ TToolContainer }

class function TToolContainer.ActivateCodeCopyItemEvent(const ALang,
  AText: string): Boolean;
begin
  Result := True;

//  ShowMessage(
//    'Code copy intercept ' + sLineBreak +
//    'Lang = ' + ALang + sLineBreak //+
////    'Text = ' + AText
//  );
end;

class function TToolContainer.ActivateCopyItemEvent(const APairId, AKind,
  AContent: string): Boolean;
begin
  Result := True;

//  ShowMessage(
//    'Copy intercept ' + sLineBreak +
//    'PairId = ' + APairId + sLineBreak +
//    'Kind = ' + AKind + sLineBreak +
//    'Content = ' + AContent
//  );
end;

class function TToolContainer.ActivateInputState(
  const AState: TInputPromptState;
  const AOnFinalize: TManagedItemFinalizeProc): Boolean;
begin
  Result := True;
  AnthropicVendor.AsyncAwaitStreamChat(AState, AOnFinalize);
end;

class function TToolContainer.ActivateModelSelection: Boolean;
begin
  Result := True;
  ShowMessage('Todo Model selection');
end;

class function TToolContainer.ActivateSystemPrompt: Boolean;
begin
  Result := True;
  ShowMessage('Todo custom settings');
end;

class function TToolContainer.ActivateAudioInputEvent: Boolean;
begin
  Result := True;
  ShowMessage('Todo audio input');
end;

class function TToolContainer.ActivateCardSettingsEvent: Boolean;
begin
  Result := True;
  ShowMessage('Todo settings card');
end;

class function TToolContainer.ActivateNewChatEvent: Boolean;
begin
  Result := True;
//  ShowMessage('New chat');
end;

class function TToolContainer.SelectAgentItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  {--- Non-intrusive: do not intercept the selection. The standard agent-card
       selector populates State.Integration.Agents, exactly as it does for
       skill and MCP cards; TAnthropicServices routes the turn from there. }
  AItem := Default(TChatManagedItemRef);
  Result := False;
end;

class function TToolContainer.SelectCustomItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Todo custom selection: custom Item');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'provider');
end;

class function TToolContainer.SelectFunctionItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Todo custom selection: function Item');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'GetWeather');
end;

class function TToolContainer.SelectMCPItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Todo custom selection: MCP Item');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'jMCPWeather');
end;

class function TToolContainer.SelectSkillItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Todo custom selection: skill Item');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'custom-skill');
end;

end.


