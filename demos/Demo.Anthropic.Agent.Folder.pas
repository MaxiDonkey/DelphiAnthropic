unit Demo.Anthropic.Agent.Folder;

interface

uses
  System.SysUtils,
  Anthropic;

type
  /// <summary>
  /// One local file uploaded to the Files API, ready to be mounted as a
  /// session "file" resource inside the managed-agent cloud sandbox.
  /// </summary>
  TFolderUploadedFile = record
    {--- Files API id returned by the upload }
    FileId: string;
    {--- absolute path inside the sandbox }
    MountPath: string;
    {--- path relative to the local folder root }
    RelativePath: string;
  end;

  /// <summary>
  /// Progress callback invoked once, just before each file is uploaded.
  /// </summary>
  TFolderUploadProgress = reference to procedure(
    const Current, Total: Integer; const RelativePath: string);

  TFolderUploader = record
  public const
    {--- mount_path prefix sent to the API for each uploaded file. }
    MountRoot = '/workspace/project';

    {--- Effective path the agent actually sees: "file" resources surface
         inside the sandbox under /mnt/session/uploads + their mount_path.
         This is the path to put in prompts, NOT MountRoot. }
    SandboxRoot = '/mnt/session/uploads/workspace/project';

    {--- Demo guardrails: a managed-agent run is billed per token and per
         minute, so the "drop a small project here" folder is deliberately
         bounded. A real project should stay well under these limits. }
    MaxFiles = 60;
    MaxFileSize = 256 * 1024;
  private
    class function NormalizeFolder(const Path: string): string; static;
    class function ToRelative(const FullPath, RootFolder: string): string; static;
    class function IsEligible(const FullPath, RootFolder: string): Boolean; static;
  public
    /// <summary>
    /// Checks the selected project folder is usable. Returns False and a
    /// human-readable reason when no project is selected or its directory no
    /// longer exists.
    /// </summary>
    class function TryValidate(const LocalFolder: string;
      out Error: string): Boolean; static;

    /// <summary>
    /// Enumerates the reviewable files of <c>LocalFolder</c>, uploads each to
    /// the Files API and returns the descriptors needed to mount them under
    /// <c>MountRoot</c>. Blocking — call from a worker thread. Raises on a
    /// missing/empty folder or on any upload failure.
    /// </summary>
    class function Upload(const Client: IAnthropic; const LocalFolder: string;
      const OnProgress: TFolderUploadProgress = nil): TArray<TFolderUploadedFile>; static;
  end;

implementation

{$REGION 'Dev note'}
(*

  Local-project resource support for the pythia-anthropic VCL demo.

  A Managed Agent runs in a cloud sandbox, so a local Windows folder cannot
  be mounted directly. This unit bridges the gap entirely on the demo side
  (the SDK is never forked):

    1. enumerate the reviewable source files of the selected project folder,
    2. upload each one through the Files API (Anthropic.Files),
    3. return descriptors that the session flow turns into "file" session
       resources, each mounted at MountRoot + '/' + <relative path>.

  The agent then explores the rebuilt tree under /workspace/project with
  read / glob / grep.

  Only text / source files are uploaded: compiled output, binaries and
  oversized files would only burn upload time and tokens. The file count and
  per-file size are capped; this is a demo meant for a SMALL project.

*)
{$ENDREGION}

uses
  System.IOUtils, System.Generics.Collections;

const
  {--- Text / source extensions worth reviewing. Anything else is skipped. }
  ELIGIBLE_EXTENSIONS: array[0..28] of string = (
    '.pas', '.dpr', '.dpk', '.dproj', '.inc', '.dfm', '.fmx',
    '.txt', '.md', '.json', '.xml', '.yaml', '.yml', '.ini', '.cfg',
    '.cs', '.c', '.h', '.cpp', '.hpp', '.py', '.js', '.ts',
    '.html', '.htm', '.css', '.sql', '.bat', '.sh');

  {--- Directory name fragments never worth uploading for a code review:
       Delphi local history, build output, version-control metadata. }
  EXCLUDED_DIRS: array[0..5] of string = (
    '__history', '__recovery', '.git', 'win32', 'win64', 'osx64');

{ TFolderUploader }

class function TFolderUploader.NormalizeFolder(const Path: string): string;
begin
  Result := Path.Trim;
  if Result.IsEmpty then
    Exit;

  {--- The project selector can return forward slashes depending on how the
       folder was originally added. Normalize to a Windows path before IO. }
  Result := Result.Replace('/', '\', [rfReplaceAll]);
  Result := ExcludeTrailingPathDelimiter(Result);
end;

class function TFolderUploader.ToRelative(const FullPath, RootFolder: string): string;
begin
  Result := FullPath;
  if Result.StartsWith(RootFolder, True) then
    Result := Result.Substring(RootFolder.Length);

  {--- The sandbox is Linux: emit a forward-slash relative path. }
  Result := Result.Replace('\', '/', [rfReplaceAll]);
  while Result.StartsWith('/') do
    Result := Result.Substring(1);
end;

class function TFolderUploader.IsEligible(const FullPath, RootFolder: string): Boolean;
begin
  Result := False;

  var Ext := TPath.GetExtension(FullPath).ToLowerInvariant;
  var ExtOk := False;
  for var Candidate in ELIGIBLE_EXTENSIONS do
    if Candidate = Ext then
      begin
        ExtOk := True;
        Break;
      end;
  if not ExtOk then
    Exit;

  var RelLower := ('/' + ToRelative(FullPath, RootFolder)).ToLowerInvariant;
  for var Dir in EXCLUDED_DIRS do
    if RelLower.Contains('/' + Dir + '/') then
      Exit;

  try
    {--- An oversized "source" file is almost always generated or binary. }
    if TFile.GetSize(FullPath) > MaxFileSize then
      Exit;
  except
    Exit;
  end;

  Result := True;
end;

class function TFolderUploader.TryValidate(const LocalFolder: string;
  out Error: string): Boolean;
begin
  Error := '';

  var Folder := NormalizeFolder(LocalFolder);
  if Folder.IsEmpty then
    begin
      Error :=
        'This managed agent needs a selected project folder. Select a ' +
        'project with the Project button before starting the agent.';
      Exit(False);
    end;

  if not TDirectory.Exists(Folder) then
    begin
      Error :=
        'The selected project folder does not exist: ' + Folder;
      Exit(False);
    end;

  Result := True;
end;

class function TFolderUploader.Upload(const Client: IAnthropic;
  const LocalFolder: string;
  const OnProgress: TFolderUploadProgress): TArray<TFolderUploadedFile>;
begin
  Result := [];

  var ValidationError := '';
  if not TryValidate(LocalFolder, ValidationError) then
    raise Exception.Create(ValidationError);

  var Folder := NormalizeFolder(LocalFolder);

  {--- Collect the eligible files first so the upload total is known up
       front and can be reported to the operator. }
  var Eligible := TList<string>.Create;
  try
    for var FullPath in TDirectory.GetFiles(Folder, '*',
      TSearchOption.soAllDirectories) do
      begin
        if Eligible.Count >= MaxFiles then
          Break;
        if IsEligible(FullPath, Folder) then
          Eligible.Add(FullPath);
      end;

    if Eligible.Count = 0 then
      raise Exception.Create(
        'The selected project folder contains no reviewable source file: ' +
        Folder);

    for var I := 0 to Eligible.Count - 1 do
      begin
        var FullPath := Eligible[I];
        var Relative := ToRelative(FullPath, Folder);

        if Assigned(OnProgress) then
          OnProgress(I + 1, Eligible.Count, Relative);

        var Uploaded: Anthropic.TFile := nil;
        try
          try
            Uploaded := Client.Files.Upload(
              procedure (Params: TUploadParams)
              begin
                Params.&File(FullPath);
              end);
          except
            on E: Exception do
              raise Exception.CreateFmt(
                'Upload of project file "%s" failed: %s',
                [Relative, E.Message]);
          end;

          var Entry := Default(TFolderUploadedFile);
          Entry.FileId := Uploaded.Id;
          Entry.RelativePath := Relative;
          Entry.MountPath := MountRoot + '/' + Relative;
          Result := Result + [Entry];
        finally
          Uploaded.Free;
        end;
      end;
  finally
    Eligible.Free;
  end;
end;

end.
