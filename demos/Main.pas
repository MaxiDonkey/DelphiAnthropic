unit Main;

interface

uses
  System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  {--- Pythia-Webview }
  VCL.WVPythia.Chat, WVPythia.Types, WVPythia.Types.EnumWire, WVPythia.Chat.Interfaces,

  {--- Adpter }
  VCL.WVPythia.Services,

  {--- AsyncTools}
  Demo.Anthropic.AsyncUtils,

  {--- Anthropic SDK }
  Anthropic, Demo.Anthropic.Services, Demo.Anthropic.Context, Demo.Anthropic.Strs;

const
  STILL_IN_PROGRESS_ERROR =
    'Requests are still in progress. #10Please wait for them to complete before closing the application.';
  APP_CAPTION =
    'Pythia-Webview2 (%s) - Anthropic vendor Demo - Delphi Anthropic SDK version %s';

type
  TAnthropicDemoPythia = class(TVCLPythia)
  public
    procedure SetLanguage(const Value: string); override;
  end;

  TForm1 = class(TForm)
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    procedure DoOnInitialized;
    procedure UpdateApiKey(KeyName: string);
  public
    Pythia: TVCLPythia;
  end;

var
  Form1: TForm1;

implementation

{$REGION 'Dev note'}
(*

  Main VCL form for the pythia-anthropic demo.

  This unit keeps the host application deliberately small. It creates the
  Pythia WebView control, installs the VCL service adapter, wires the
  Anthropic vendor service once the browser is initialized, and forwards API
  key changes to the provider implementation.

  TAnthropicDemoPythia only customizes language loading so the demo can add
  Anthropic-specific translations on top of the generic Pythia UI language
  folder.

  Shutdown is guarded by HttpMonitoring.IsBusy because the SDK can still have
  active uploads, downloads or streaming requests. When work is in progress the
  form stays open and the error is surfaced through the Pythia browser instead
  of raising a VCL dialog.

*)
{$ENDREGION}

{$R *.dfm}

procedure TAnthropicDemoPythia.SetLanguage(const Value: string);
begin
  inherited SetLanguage(Value);
  TAnthropicDemoTranslations.LoadFromLanguage(GetLanguageFolder, Value);
end;

procedure TForm1.DoOnInitialized;
begin
  AnthropicVendor := TAnthropicServices.Create(
    Pythia,
    TAnthropicContext.CreateInstance(Pythia)
  );

  AlphaBlend := False;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not HttpMonitoring.IsBusy;
  if not CanClose then
    Pythia.DisplayError(STILL_IN_PROGRESS_ERROR);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Caption := Format(APP_CAPTION, [TVCLPythia.Version, Anthropic.Version]);

  Width := 1350;
  Height := 830;

  AlphaBlendValue := 0;
  AlphaBlend := True;

  Pythia := TAnthropicDemoPythia.Create(Panel2);
  Pythia.OnApiKeyChanged := UpdateApiKey;
  Pythia.ServiceAdapter := TVCLChatManagedItemDialogService.Create;
  Pythia.OnInitialized := DoOnInitialized;
  Pythia.Update;
end;

procedure TForm1.UpdateApiKey(KeyName: string);
begin
  if SameText(KeyName, TAnthropicServices.API_KEY_NAME) then
    AnthropicVendor.UpdateApiKey;
end;

end.


