unit Sample.IniManagment;

interface

uses
  System.SysUtils, System.IniFiles;

type
  TAppIni = record
  strict private
    class function ExeIniPath: string; static;
    class function AppDataIniPath: string; static;
    class function EnsurePathAndMigrate: string; static;
  public
    class function Path: string; static;

    class function ReadString(const Section, Ident, Default: string): string; static;
    class function ReadInteger(const Section, Ident: string; Default: Integer): Integer; static;
    class function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; static;

    class procedure WriteString(const Section, Ident, Value: string); static;
    class procedure WriteInteger(const Section, Ident: string; Value: Integer); static;
    class procedure WriteBool(const Section, Ident: string; Value: Boolean); static;
  end;

implementation

uses
  System.IOUtils;

class function TAppIni.ExeIniPath: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

class function TAppIni.AppDataIniPath: string;
var
  Base, Product, Dir: string;
begin
  Product := ChangeFileExt(ExtractFileName(ParamStr(0)), '');

  Base := System.IOUtils.TPath.GetHomePath;
  Dir  := IncludeTrailingPathDelimiter(Base) + Product;
  TDirectory.CreateDirectory(Dir);

  Result := IncludeTrailingPathDelimiter(Dir) + 'settings.ini';
end;

class function TAppIni.EnsurePathAndMigrate: string;
var
  Src, Dst, Dir: string;
begin
  Src := ExeIniPath;
  Dst := AppDataIniPath;

  Dir := ExtractFilePath(Dst);
  if not TDirectory.Exists(Dir) then
    TDirectory.CreateDirectory(Dir);

  // Migration simple : si pas encore d'ini en AppData, mais il existe à côté de l'exe
  if (not TFile.Exists(Dst)) and TFile.Exists(Src) then
    TFile.Copy(Src, Dst);

  Result := Dst;
end;

class function TAppIni.Path: string;
begin
  Result := EnsurePathAndMigrate;
end;

class function TAppIni.ReadString(const Section, Ident, Default: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Result := Ini.ReadString(Section, Ident, Default);
  finally
    Ini.Free;
  end;
end;

class function TAppIni.ReadInteger(const Section, Ident: string; Default: Integer): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Result := Ini.ReadInteger(Section, Ident, Default);
  finally
    Ini.Free;
  end;
end;

class function TAppIni.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Result := Ini.ReadBool(Section, Ident, Default);
  finally
    Ini.Free;
  end;
end;

class procedure TAppIni.WriteString(const Section, Ident, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Ini.WriteString(Section, Ident, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

class procedure TAppIni.WriteInteger(const Section, Ident: string; Value: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Ini.WriteInteger(Section, Ident, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

class procedure TAppIni.WriteBool(const Section, Ident: string; Value: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(Path);
  try
    Ini.WriteBool(Section, Ident, Value);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

end.

