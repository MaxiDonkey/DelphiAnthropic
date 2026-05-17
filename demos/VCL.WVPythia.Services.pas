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
  ShowMessage('Model selection is not implemented in this demo.');
end;

class function TToolContainer.ActivateSystemPrompt: Boolean;
begin
  Result := True;
  ShowMessage('Custom system settings are not implemented in this demo.');
end;

class function TToolContainer.ActivateAudioInputEvent: Boolean;
begin
  Result := True;
  ShowMessage('Audio input is not implemented in this demo.');
end;

class function TToolContainer.ActivateCardSettingsEvent: Boolean;
begin
  Result := True;
  ShowMessage('Card settings are not implemented in this demo.');
end;

class function TToolContainer.ActivateNewChatEvent: Boolean;
begin
  Result := True;
//  ShowMessage('New chat');
end;

class function TToolContainer.SelectAgentItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Demo placeholder: Agent item selection.');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'web search agent');
end;

class function TToolContainer.SelectCustomItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Demo placeholder: custom item selection.');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'provider');
end;

class function TToolContainer.SelectFunctionItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Demo placeholder: function item selection.');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'GetWeather');
end;

class function TToolContainer.SelectMCPItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Demo placeholder: MCP item selection.');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'jMCPWeather');
end;

class function TToolContainer.SelectSkillItem(
  out AItem: TChatManagedItemRef): Boolean;
begin
  Result := True;
  ShowMessage('Demo placeholder: skill item selection.');
  var Code := Trunc(Random(20000) + 1);
  AItem := TChatManagedItemRef.Create(Code.ToString, 'custom-skill');
end;

end.

