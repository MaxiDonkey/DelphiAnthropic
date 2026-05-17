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
  Anthropic, Demo.Anthropic.Services, Demo.Anthropic.Context;

const
  STILL_IN_PROGRESS_ERROR =
    'Requests are still in progress. #10Please wait for them to complete before closing the application.';
  APP_CAPTION =
    'Pythia-WebView2 (%s) - Anthropic vendor Demo - Delphi Anthropic SDK version %s';

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
  private
    procedure DoOnInitialized;
    procedure UpdateApiKey(KeyName: string);
  public
    Pythia: TVCLPythia;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  (Pythia as IPythiaBrowser).SetSendButtonAvailability(False);
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
  Height := 770;

  AlphaBlendValue := 0;
  AlphaBlend := True;

  Pythia := TVCLPythia.Create(Panel2);
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

