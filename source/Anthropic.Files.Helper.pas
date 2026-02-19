unit Anthropic.Files.Helper;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.IOUtils;

type
  TApplyResolvedFilesProc = reference to procedure(const AbsRoot: string; const AbsFiles: TArray<string>);

  TFileHelper = record
    /// <summary>
    /// Returns all files contained in a folder and its subfolders.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This helper enumerates files recursively (<c>soAllDirectories</c>).
    /// </para>
    /// <para>
    /// • <paramref name="Folder"/> may be an absolute path or a path relative to the current
    /// module directory (the directory of the executable or loaded module).
    /// </para>
    /// <para>
    /// • The returned file paths are absolute, normalized paths suitable for opening streams.
    /// </para>
    /// <para>
    /// • When <paramref name="Folder"/> is empty, an empty array is returned.
    /// </para>
    /// </remarks>
    /// <param name="Folder">
    /// Folder to scan. Can be absolute or relative to the module directory.
    /// </param>
    /// <returns>
    /// An array of absolute file paths found under <paramref name="Folder"/> (recursively).
    /// </returns>
    /// <exception cref="System.SysUtils.Exception">
    /// Raised when the resolved folder does not exist.
    /// </exception>
    class function FilesFromDir(const Folder: string): TArray<string>; static;

    /// <summary>
    /// Resolves a root directory and a set of file paths to absolute paths and
    /// applies an operation to the resolved values.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This helper normalizes <paramref name="RootDir"/> to an absolute path and
    /// removes any trailing directory separator.
    /// </para>
    /// <para>
    /// • Each entry in <paramref name="Files"/> may be an absolute or relative path.
    /// Relative file paths are resolved against the normalized <paramref name="RootDir"/>.
    /// </para>
    /// <para>
    /// • All resolved file paths are absolute and suitable for opening file streams
    /// or for subsequent validation against <paramref name="RootDir"/>.
    /// </para>
    /// <para>
    /// • When <paramref name="Files"/> is empty, this method performs no action and
    /// the <paramref name="Apply"/> callback is not invoked.
    /// </para>
    /// <para>
    /// • The <paramref name="Apply"/> callback receives the normalized root directory
    /// and the array of resolved file paths, allowing the caller to apply custom logic
    /// (for example, adding files to a multipart form payload).
    /// </para>
    /// </remarks>
    /// <param name="RootDir">
    /// Root directory used as the base for resolving relative file paths.
    /// Must not be empty.
    /// </param>
    /// <param name="Files">
    /// File paths to resolve. Each item may be absolute or relative.
    /// </param>
    /// <param name="Apply">
    /// Callback invoked with the resolved root directory and resolved file paths.
    /// </param>
    /// <exception cref="System.SysUtils.Exception">
    /// Raised when <paramref name="RootDir"/> is empty, when a file path is empty,
    /// or when path resolution fails.
    /// </exception>
    class procedure NormalizeRootAndFiles(
      const RootDir: string;
      const Files: TArray<string>;
      const Apply: TApplyResolvedFilesProc); static;
  end;

implementation

{ TFileHelper }

class function TFileHelper.FilesFromDir(const Folder: string): TArray<string>;
var
  AbsFolder: string;
begin
  Result := [];
  if Folder.IsEmpty then
    Exit(Result);

  var BaseDir := ExtractFilePath(GetModuleName(HInstance));

  if TPath.IsPathRooted(Folder) then
    AbsFolder := TPath.GetFullPath(Folder)
  else
    AbsFolder := TPath.GetFullPath(TPath.Combine(BaseDir, Folder));

  AbsFolder := ExcludeTrailingPathDelimiter(AbsFolder);

  if not TDirectory.Exists(AbsFolder) then
    raise Exception.Create('Directory does not exist: ' + AbsFolder);

  Result := TDirectory.GetFiles(AbsFolder, '*', TSearchOption.soAllDirectories);
end;

class procedure TFileHelper.NormalizeRootAndFiles(const RootDir: string;
  const Files: TArray<string>; const Apply: TApplyResolvedFilesProc);
var
  AbsRoot: string;
  AbsFiles: TArray<string>;
begin
  if not Assigned(Apply) then
    Exit;

  if Length(Files) = 0 then
    Exit;

  if RootDir.IsEmpty then
    raise Exception.Create('RootDir cannot be empty.');

  AbsRoot := ExcludeTrailingPathDelimiter(TPath.GetFullPath(RootDir));

  SetLength(AbsFiles, Length(Files));
  for var I := 0 to High(Files) do
    begin
      if Files[I].IsEmpty then
        raise Exception.Create('File path cannot be empty.');

      if TPath.IsPathRooted(Files[I]) then
        AbsFiles[I] := TPath.GetFullPath(Files[I])
      else
        AbsFiles[I] := TPath.GetFullPath(TPath.Combine(AbsRoot, Files[I]));
    end;

  Apply(AbsRoot, AbsFiles);
end;

end.
