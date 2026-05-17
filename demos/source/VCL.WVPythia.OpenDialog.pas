unit VCL.WVPythia.OpenDialog;

interface

{$REGION  'Dev notes : Helper.OpenDialog.VCL'}

(*
A. Returns a boolean with True if successful.

  1. Only one file is returned

      var FileName := 'D:\2026-developpement\OpenAI_File_Search\logos\GeminiLogo.png';
      var Ok :=
              TOpenDialogHelper.Create(nil)
                .Filter('Network Graphics (*.png)|*.png')
                .InitialDir(ExtractFileDir(FileName))
                .Execute(FileName);
      if Ok then
        ShowMessage(FileName);

  2. Multiple files can be returned - Multiple selection.

      var FileName := 'D:\2026-developpement\OpenAI_File_Search\logos\GeminiLogo.png';
      var Ok :=
              TOpenDialogHelper.Create(nil)
                .Filter('Network Graphics (*.png)|*.png')
                .InitialDir(ExtractFileDir(FileName))  <--- is placed in the file folder
                .Execute(FileName, True);
      if Ok then
          for var Item in FileName.Split([#10]) do
              ShowMessage(Item);

B. Returns a string and -1 on abort. Don't test if the file exists.

   1. A single file returned or the string with -1

      var FileName := 'D:\2026-developpement\OpenAI_File_Search\logos\GeminiLogo.png';
      var FileName1 := TOpenDialogHelper.Create(nil)
                .Filter('Network Graphics (*.png)|*.png')
                .InitialDir(ExtractFileDir(FileName))
                .Execute;
      if FileExists(FileName1) then
        ShowMessage(FileName1);

   2. Returns multiple files or the string -1

      var FileName := 'D:\2026-developpement\OpenAI_File_Search\logos\GeminiLogo.png';
      var FileName1 := TOpenDialogHelper.Create(nil)
              .Filter('Network Graphics (*.png)|*.png')
              .InitialDir(ExtractFileDir(FileName))
              .Execute(True);

      for var Item in FileName1.Split([#10]) do
        if FileExists(Item) then
           ShowMessage(Item);
*)

{$ENDREGION}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.IOUtils,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs,
  Vcl.Themes;

type
  TOpenDialogHelper = record
  strict private
    FOpenDialog: TOpenDialog;
  public
    class function Use(ADialog: TOpenDialog): TOpenDialogHelper; static;
    function Filter(const S: string): TOpenDialogHelper; inline;
    function FilterIndex(const Index: Integer): TOpenDialogHelper; inline;
    function DefautExt(const S: string): TOpenDialogHelper; inline;
    function InitialDir(const S: string): TOpenDialogHelper; inline;
    function Execute(var FileName: string; Multi: Boolean = False): Boolean; overload; inline;
    function Execute(Multi: Boolean = False): string; overload; inline;

    property Dialog: TOpenDialog read FOpenDialog;
  end;

implementation

{ TOpenDialogHelper }

function TOpenDialogHelper.DefautExt(const S: string): TOpenDialogHelper;
begin
  FOpenDialog.DefaultExt := S;
  Result := Self;
end;

function TOpenDialogHelper.Execute(Multi: Boolean): string;
begin
  var SavedHooks := TStyleManager.SystemHooks;
  try
    TStyleManager.SystemHooks := SavedHooks - [shDialogs];
    if Multi then
      FOpenDialog.Options := FOpenDialog.Options + [ofAllowMultiSelect]
    else
      FOpenDialog.Options := FOpenDialog.Options - [ofAllowMultiSelect];
    if FOpenDialog.Execute then
      begin
        if Multi then
          Result := FOpenDialog.Files.Text.Trim
        else
          Result := FOpenDialog.FileName
      end
    else
      Result := ':none';
  finally
//    FOpenDialog.Free;
    TStyleManager.SystemHooks := SavedHooks;
  end;
end;

function TOpenDialogHelper.Execute(var FileName: string; Multi: Boolean): Boolean;
begin
  var SavedHooks := TStyleManager.SystemHooks;
  try
    TStyleManager.SystemHooks := SavedHooks - [shDialogs];
    if Multi then
      FOpenDialog.Options := FOpenDialog.Options + [ofAllowMultiSelect]
    else
      FOpenDialog.Options := FOpenDialog.Options - [ofAllowMultiSelect];
    Result := FOpenDialog.Execute;
    if Result then
      begin
        if Multi then
          FileName := FOpenDialog.Files.Text.Trim
        else
          FileName := FOpenDialog.FileName;
      end;
  finally
//    FOpenDialog.Free;
    TStyleManager.SystemHooks := SavedHooks;
  end;
end;

function TOpenDialogHelper.Filter(const S: string): TOpenDialogHelper;
begin
  FOpenDialog.Filter := S;
  Result := Self;
end;

function TOpenDialogHelper.FilterIndex(const Index: Integer): TOpenDialogHelper;
begin
  FOpenDialog.FilterIndex := index;
  Result := Self;
end;

function TOpenDialogHelper.InitialDir(const S: string): TOpenDialogHelper;
var
  Path: string;
begin
  if S.StartsWith('..\') then
    Path := TPath.GetFullPath(TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), S))
  else
    Path := TPath.GetDirectoryName(S);
  FOpenDialog.InitialDir := Path;
  Result := Self;
end;

class function TOpenDialogHelper.Use(ADialog: TOpenDialog): TOpenDialogHelper;
begin
  Result := Default(TOpenDialogHelper);

  if Assigned(ADialog) then
    Result.FOpenDialog := ADialog
  else
    Result.FOpenDialog := TOpenDialog.Create(Application);
end;

end.
