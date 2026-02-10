unit Sample.Key.Managment;

interface

uses
  Winapi.Windows, System.SysUtils, System.Win.Registry, System.UITypes,
  FMX.DialogService;

type
  TEnvironmentManager = record
  public
    class procedure SetUserEnvVar(const Name, Value: string; Expandable: Boolean = False); static;
    class function ReadEnvFromRegistry(const Name: string): string; static;
    class procedure ReadKey(const KeyName: string; var KeyValue: string); static;
    class procedure SetKeyValue; static;
    class function ResolveAnthropicKey: string; static;
  end;

  TInputContent = record
  private
    class procedure SetValue(var ID: string; const Value: string); static;
  public
    class function Text: string; static;
  end;

var
  API_KEY: string;

implementation

{ TEnvironmentManager }

class procedure TEnvironmentManager.ReadKey(const KeyName: string; var KeyValue: string);
begin
  KeyValue := ReadEnvFromRegistry(KeyName);

  if KeyValue.Trim.IsEmpty then
    SetUserEnvVar(KeyName, TInputContent.Text);
end;

class function TEnvironmentManager.ReadEnvFromRegistry(const Name: string): string;

  function ReadFrom(const Root: HKEY; const SubKey, ValueName: string): string;
  var R: TRegistry;
  begin
    Result := '';
    R := TRegistry.Create(KEY_READ);
    try
      R.RootKey := Root;
      if R.OpenKeyReadOnly(SubKey) and R.ValueExists(ValueName) then
        Result := R.ReadString(ValueName);
    finally
      R.Free;
    end;
  end;

begin
  Result := ReadFrom(HKEY_CURRENT_USER, 'Environment', Name);
  if not Result.Trim.IsEmpty then
    Exit;

  Result := ReadFrom(HKEY_LOCAL_MACHINE,
            'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', Name);
end;

class function TEnvironmentManager.ResolveAnthropicKey: string;
begin
  TEnvironmentManager.ReadKey('ANTHROPIC_API_KEY', Result);
end;

class procedure TEnvironmentManager.SetKeyValue;
begin
  var Key := TInputContent.Text;
  if not Key.Trim.IsEmpty then
    SetUserEnvVar('ANTHROPIC_API_KEY', Key);
end;

class procedure TEnvironmentManager.SetUserEnvVar(const Name, Value: string;
  Expandable: Boolean);
var
  EnvKey: HKEY;
  Status: Longint;
  DataSize: DWORD;
begin
  Status := RegCreateKeyEx(HKEY_CURRENT_USER, 'Environment', 0, nil,
                           REG_OPTION_NON_VOLATILE, KEY_SET_VALUE,
                           nil, EnvKey, nil);

  if Status <> ERROR_SUCCESS then
    RaiseLastOSError(Status);

  try
    DataSize := (Length(Value) + 1) * SizeOf(Char);
    Status := RegSetValueEx(EnvKey, PChar(Name), 0, REG_SZ,
                            PByte(PChar(Value)), DataSize);

    if Status <> ERROR_SUCCESS then
      RaiseLastOSError(Status);
  finally
    RegCloseKey(EnvKey);
  end;
end;

{ TInputContent }

class procedure TInputContent.SetValue(var ID: string; const Value: string);
begin
  ID := Value;
end;

class function TInputContent.Text: string;
begin
  var ID_ := EmptyStr;

  TDialogService.InputQuery(
    'Input a text',
    ['ID'],
    [''],
    procedure(const AResult: TModalResult; const AValues: array of string)
    var
      KeyValue: string;
    begin
      if AResult = mrOk then
        begin
          KeyValue := AValues[0];
          if KeyValue <> '' then
            SetValue(ID_, KeyValue);
        end;
    end
  );
  Result := ID_;
end;

initialization
  API_KEY := TEnvironmentManager.ResolveAnthropicKey;
end.

