unit Sample.UrlOpen;

interface

procedure OpenUrl(const AUrl: string);

implementation

uses
{$IF Defined(MSWINDOWS)}
  Winapi.Windows, Winapi.ShellAPI
{$ELSEIF Defined(ANDROID)}
  Androidapi.JNI.App, Androidapi.JNI.Net, Androidapi.Helpers
{$ELSEIF Defined(IOS)}
  iOSapi.UIKit, Macapi.Helpers, iOSapi.Foundation, FMX.Helpers.iOS
{$ELSEIF Defined(MACOS)}
  Posix.Stdlib
{$ENDIF}
  ;

procedure OpenUrl(const AUrl: string);
{$IF Defined(ANDROID)}
var
  Intent: JIntent;
{$ENDIF}
begin
{$IF Defined(MSWINDOWS)}
  ShellExecute(0, 'OPEN', PWideChar(AUrl), nil, nil, SW_SHOWNORMAL);

{$ELSEIF Defined(ANDROID)}
  Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
  Intent.setData(StrToJURI(AUrl));
  TAndroidHelper.Activity.startActivity(Intent);

{$ELSEIF Defined(IOS)}
  SharedApplication.OpenURL(StrToNSUrl(AUrl));

{$ELSEIF Defined(MACOS)}
  _system(PAnsiChar('open ' + AnsiString(AUrl)));
{$ENDIF}
end;

end.
