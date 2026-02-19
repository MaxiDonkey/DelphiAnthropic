unit Anthropic.Files;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API.Params, Anthropic.API.MultiFormData, Anthropic.API, Anthropic.Types,
  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  TUploadParams = class(TMultiFormDataParams)
    /// <summary>
    /// Adds a file to be uploaded from a filename.
    /// </summary>
    function &File(const FileName: string): TUploadParams; overload;

    /// <summary>
    /// Adds a file to be uploaded from a stream.
    /// </summary>
    function &File(const Stream: TStream; const FileName: string): TUploadParams; overload;
  end;

  TUploadParamProc = TProc<TUploadParams>;

  TFilesListParams = class(TUrlParam)
  public
    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results
    /// immediately after this object.
    /// </summary>
    function AfterId(const Value: string): TFilesListParams;

    /// <summary>
    /// ID of the object to use as a cursor for pagination. When provided, returns the page of results
    /// immediately before this object.
    /// </summary>
    function BeforeId(const Value: string): TFilesListParams;

    /// <summary>
    /// Number of items to return per page.
    /// </summary>
    /// <param name="Value">
    /// Defaults to 20. Ranges from 1 to 1000.  
    /// </param>
    /// <remarks>
    /// maximum 1000, minimum 1
    /// </remarks>
    function Limit(const Value: Integer): TFilesListParams;
  end;

  TFilesListParamProc = TProc<TFilesListParams>;

  TFile = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    FFilename: string;
    [JsonNameAttribute('mime_type')]
    FMimeType: string;
    [JsonNameAttribute('size_bytes')]
    FSizeBytes: Int64;
    FType: string;
    FDownloadable: Boolean;
  public
    /// <summary>
    /// Unique object identifier.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// RFC 3339 datetime string representing when the file was created.
    /// </summary>
    /// <remarks>
    /// format date-time
    /// </remarks>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// Original filename of the uploaded file.
    /// </summary>
    /// <remarks>
    /// maxLength 500, minLength 1
    /// </remarks>
    property Filename: string read FFilename write FFilename;

    /// <summary>
    /// MIME type of the file.
    /// </summary>
    /// <remarks>
    /// maxLength 255, minLength 1
    /// </remarks>
    property MimeType: string read FMimeType write FMimeType;

    /// <summary>
    /// Size of the file in bytes.
    /// </summary>
    /// <remarks>
    /// minimum 0
    /// </remarks>
    property SizeBytes: Int64 read FSizeBytes write FSizeBytes;

    /// <summary>
    /// For files, this is always "file".
    /// </summary>
    property &Type: string read FType write FType;

    /// <summary>
    /// Whether the file can be downloaded.
    /// </summary>
    property Downloadable: Boolean read FDownloadable write FDownloadable;
  end;

  TFileList = class(TJSONFingerprint)
  private
    FData: TArray<TFile>;
    [JsonNameAttribute('first_id')]
    FFirstId: string;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('last_id')]
    FLastId: string;
  public
    /// <summary>
    /// List of file metadata objects.
    /// </summary>
    property Data: TArray<TFile> read FData write FData;

    /// <summary>
    /// ID of the first file in this page of results.
    /// </summary>
    property FirstId: string read FFirstId write FFirstId;

    /// <summary>
    /// Whether there are more results available.
    /// </summary>
    property HasMore: Boolean read FHasMore write FHasMore;

    /// <summary>
    /// ID of the last file in this page of results.
    /// </summary>
    property LastId: string read FLastId write FLastId;

    destructor Destroy; override;
  end;

  TFileDeleted = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// ID of the deleted file.
    /// </summary>
    property Id: string read FId write FId;

    /// <summary>
    /// For file deletion, this is always "file_deleted".
    /// </summary>
    property &Type: string read FType write FType;
  end;

  TFileDownloaded = class(TJSONFingerprint)
  private
    FData: string;
  public
    procedure SaveToFile(const FileName: string);

    /// <summary>
    /// File content as string base64-encoded
    /// </summary>
    property Data: string read FData write FData;
  end;

  /// <summary>
  /// Defines an asynchronous callback handler for file operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynFile</c> is a type alias for <c>TAsynCallBack&lt;TFile&gt;</c>, specialized for receiving
  /// <c>TFile</c> results from asynchronous file route calls.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Files domain by fixing the
  /// result type to <c>TFile</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TFile&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynFile</c> for callbacks associated with file creation/upload and retrieval operations.
  /// </para>
  /// </remarks>
  TAsynFile = TAsynCallBack<TFile>;

  /// <summary>
  /// Defines a promise-based callback handler for file operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseFile</c> is a type alias for <c>TPromiseCallback&lt;TFile&gt;</c>, specialized for
  /// consuming <c>TFile</c> results through promise-based abstractions.
  /// </para>
  /// <para>
  /// • It binds the generic promise callback infrastructure to the Files domain by fixing the
  /// resolved value type to <c>TFile</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TFile&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseFile</c> when awaiting file upload or retrieval results via
  /// <c>AsyncAwait*</c> helper methods.
  /// </para>
  /// </remarks>
  TPromiseFile = TPromiseCallback<TFile>;

  /// <summary>
  /// Defines an asynchronous callback handler for file list operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynFileList</c> is a type alias for <c>TAsynCallBack&lt;TFileList&gt;</c>, specialized for
  /// receiving paginated file list results.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Files domain by fixing the
  /// result type to <c>TFileList</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TFileList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynFileList</c> for callbacks associated with file listing operations, including
  /// paginated queries with cursor-based navigation.
  /// </para>
  /// </remarks>
  TAsynFileList = TAsynCallBack<TFileList>;

  /// <summary>
  /// Defines a promise-based callback handler for file list operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseFileList</c> is a type alias for <c>TPromiseCallback&lt;TFileList&gt;</c>, specialized
  /// for consuming paginated file list results through promise-based abstractions.
  /// </para>
  /// <para>
  /// • It binds the generic promise callback infrastructure to the Files domain by fixing the
  /// resolved value type to <c>TFileList</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TFileList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseFileList</c> when awaiting file listing results via <c>AsyncAwaitList</c> helper
  /// methods, including cursor-based pagination scenarios.
  /// </para>
  /// </remarks>
  TPromiseFileList =  TPromiseCallback<TFileList>;

  /// <summary>
  /// Defines an asynchronous callback handler for file deletion operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynFileDeleted</c> is a type alias for <c>TAsynCallBack&lt;TFileDeleted&gt;</c>, specialized
  /// for receiving file deletion result payloads.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Files domain by fixing the
  /// result type to <c>TFileDeleted</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability and intent clarity while preserving the complete behavior
  /// and lifecycle semantics of <c>TAsynCallBack&lt;TFileDeleted&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynFileDeleted</c> for callbacks associated with file deletion operations, including
  /// explicit delete requests and cleanup workflows.
  /// </para>
  /// </remarks>
  TAsynFileDeleted = TAsynCallBack<TFileDeleted>;

  /// <summary>
  /// Defines a promise-based callback handler for file deletion operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseFileDeleted</c> is a type alias for <c>TPromiseCallback&lt;TFileDeleted&gt;</c>,
  /// specialized for consuming file deletion result payloads through promise-based abstractions.
  /// </para>
  /// <para>
  /// • It binds the generic promise callback infrastructure to the Files domain by fixing the
  /// resolved value type to <c>TFileDeleted</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TFileDeleted&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseFileDeleted</c> when awaiting file deletion results via <c>AsyncAwaitDelete</c>
  /// helper methods.
  /// </para>
  /// </remarks>
  TPromiseFileDeleted = TPromiseCallback<TFileDeleted>;

  /// <summary>
  /// Defines an asynchronous callback handler for file download operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynFileDownloaded</c> is a type alias for <c>TAsynCallBack&lt;TFileDownloaded&gt;</c>,
  /// specialized for receiving <c>TFileDownloaded</c> payloads from asynchronous download calls.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Files domain by fixing the
  /// result type to <c>TFileDownloaded</c>.
  /// </para>
  /// <para>
  /// • This alias improves API readability by clearly signaling that the callback delivers downloaded
  /// file content (typically base64-encoded) rather than metadata.
  /// </para>
  /// <para>
  /// • Use <c>TAsynFileDownloaded</c> for callbacks associated with <c>Download</c> / <c>AsynDownload</c>
  /// operations, especially when you want a non-blocking workflow and to persist the content via
  /// <c>TFileDownloaded.SaveToFile</c>.
  /// </para>
  /// </remarks>
  TAsynFileDownloaded = TAsynCallBack<TFileDownloaded>;

  /// <summary>
  /// Defines a promise-based callback handler for file download operations.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseFileDownloaded</c> is a type alias for
  /// <c>TPromiseCallback&lt;TFileDownloaded&gt;</c>, specialized for consuming
  /// downloaded file payloads through promise-based abstractions.
  /// </para>
  /// <para>
  /// • It binds the generic promise callback infrastructure to the Files domain
  /// by fixing the resolved value type to <c>TFileDownloaded</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing async/await-style
  /// consumption semantics for file download operations.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseFileDownloaded</c> when awaiting the result of a file
  /// <c>Download</c> operation via <c>AsyncAwaitDownload</c>, especially when the
  /// downloaded content must be persisted using
  /// <c>TFileDownloaded.SaveToFile</c>.
  /// </para>
  /// </remarks>
  TPromiseFileDownloaded = TPromiseCallback<TFileDownloaded>;

  TAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Delete(const FileId: string): TFileDeleted; virtual; abstract;

    function List: TFileList; overload; virtual; abstract;

    function List(const ParamProc: TProc<TFilesListParams>): TFileList; overload; virtual; abstract;

    function Retrieve(const FileId: string): TFile; virtual; abstract;

    function Upload(const ParamProc: TProc<TUploadParams>): TFile; virtual; abstract;

    function Download(const FileId: string): TFileDownloaded; virtual; abstract;
  end;

  TAsynchronousSupport = class(TAbstractSupport)
  protected
    procedure AsynDelete(
      const FileId: string;
      const CallBacks: TFunc<TAsynFileDeleted>);

    procedure AsynDownload(
      const FileId: string;
      const CallBacks: TFunc<TAsynFileDownloaded>);

    procedure AsynList(
      const CallBacks: TFunc<TAsynFileList>); overload;

    procedure AsynList(
      const ParamProc: TProc<TFilesListParams>;
      const CallBacks: TFunc<TAsynFileList>); overload;

    procedure AsynRetrieve(
      const FileId: string;
      const CallBacks: TFunc<TAsynFile>);

    procedure AsynUpload(
      const ParamProc: TProc<TUploadParams>;
      const CallBacks: TFunc<TAsynFile>);
  end;

  TFilesRoute = class(TAsynchronousSupport)
  public
    /// <summary>
    /// Deletes a file by its unique identifier.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method issues a synchronous delete request to the Files endpoint for the specified file.
    /// </para>
    /// <para>
    /// • On success, it returns a <c>TFileDeleted</c> object confirming the deletion and identifying
    /// the removed file.
    /// </para>
    /// <para>
    /// • If the file does not exist or the operation is not permitted, an API error is raised.
    /// </para>
    /// <para>
    /// • Use the asynchronous or promise-based counterparts for non-blocking deletion workflows.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to delete.
    /// </param>
    function Delete(const FileId: string): TFileDeleted; override;

    /// <summary>
    /// Downloads the content of a file by its unique identifier.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Files content endpoint and retrieves
    /// the raw file payload associated with the specified file.
    /// </para>
    /// <para>
    /// • The <c>FileId</c> parameter identifies the file to download. The operation is permitted
    /// only if the file is marked as downloadable by the API.
    /// </para>
    /// <para>
    /// • On success, the returned <c>TFileDownloaded</c> instance contains the file content
    /// encoded as a Base64 string in its <c>Data</c> property.
    /// </para>
    /// <para>
    /// • The downloaded content can be persisted to disk using
    /// <c>TFileDownloaded.SaveToFile</c>.
    /// </para>
    /// <para>
    /// • For non-blocking workflows, prefer <c>AsynDownload</c> or
    /// <c>AsyncAwaitDownload</c>.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to download.
    /// </param>
    function Download(const FileId: string): TFileDownloaded; override;

    /// <summary>
    /// Retrieves a list of uploaded files.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method issues a synchronous request to the Files endpoint to retrieve a page of file
    /// metadata objects.
    /// </para>
    /// <para>
    /// • The returned <c>TFileList</c> contains the file entries as well as pagination information,
    /// including cursor identifiers and availability of additional results.
    /// </para>
    /// <para>
    /// • This overload retrieves the default page of results using server-defined pagination settings.
    /// </para>
    /// <para>
    /// • Use the parameterized overload to control pagination or filtering, or the asynchronous and
    /// promise-based variants for non-blocking access.
    /// </para>
    /// </remarks>
    function List: TFileList; overload; override;

    /// <summary>
    /// Retrieves a list of uploaded files using custom query parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method issues a synchronous request to the Files endpoint with user-defined listing
    /// parameters.
    /// </para>
    /// <para>
    /// • The <paramref name="ParamProc"/> callback is used to configure pagination options such as
    /// cursors and page size via <c>TFilesListParams</c>.
    /// </para>
    /// <para>
    /// • The returned <c>TFileList</c> includes file metadata entries and pagination state, enabling
    /// incremental navigation through large result sets.
    /// </para>
    /// <para>
    /// • Use this overload when fine-grained control over pagination is required, or prefer the
    /// asynchronous and promise-based variants for non-blocking workflows.
    /// </para>
    /// </remarks>
    /// <param name="ParamProc">
    /// Procedure used to configure file listing parameters.
    /// </param>
    function List(const ParamProc: TProc<TFilesListParams>): TFileList; overload; override;

    /// <summary>
    /// Retrieves metadata for a specific file.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method issues a synchronous request to the Files endpoint to retrieve metadata for the
    /// specified file identifier.
    /// </para>
    /// <para>
    /// • On success, it returns a <c>TFile</c> object describing the file, including its name, size,
    /// MIME type, and creation timestamp.
    /// </para>
    /// <para>
    /// • If the file does not exist or access is not permitted, an API error is raised.
    /// </para>
    /// <para>
    /// • Use the asynchronous or promise-based counterparts when non-blocking access is required.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to retrieve.
    /// </param>
    function Retrieve(const FileId: string): TFile; override;

    /// <summary>
    /// Uploads a file to the Files endpoint.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method issues a synchronous multipart/form-data request to upload a file to the Files
    /// endpoint.
    /// </para>
    /// <para>
    /// • The <paramref name="ParamProc"/> callback is used to configure the upload payload, including
    /// the file source (stream or filename) and associated metadata.
    /// </para>
    /// <para>
    /// • On success, it returns a <c>TFile</c> object representing the uploaded file and its assigned
    /// identifier.
    /// </para>
    /// <para>
    /// • Use the asynchronous or promise-based counterparts for non-blocking upload workflows or when
    /// integrating uploads into reactive pipelines.
    /// </para>
    /// </remarks>
    /// <param name="ParamProc">
    /// Procedure used to configure upload parameters.
    /// </param>
    function Upload(const ParamProc: TProc<TUploadParams>): TFile; override;

    /// <summary>
    /// Asynchronously deletes a file and returns a promise representing the operation.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates a non-blocking file deletion request to the Files endpoint.
    /// </para>
    /// <para>
    /// • It returns a <c>TPromise&lt;TFileDeleted&gt;</c> that resolves when the deletion operation
    /// completes successfully or rejects if an error occurs.
    /// </para>
    /// <para>
    /// • The optional <paramref name="Callbacks"/> factory allows registration of promise lifecycle
    /// callbacks such as start, success, and error handlers.
    /// </para>
    /// <para>
    /// • Use this method to integrate file deletion into promise-based or <c>async/await</c>-style
    /// workflows without blocking the calling thread.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to delete.
    /// </param>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise callbacks.
    /// </param>
    function AsyncAwaitDelete(
      const FileId: string;
      const Callbacks: TFunc<TPromiseFileDeleted> = nil): TPromise<TFileDeleted>;

    /// <summary>
    /// Downloads the content of a file using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous file download workflow into a
    /// <c>TPromise&lt;TFileDownloaded&gt;</c>, enabling non-blocking consumption with
    /// async/await semantics.
    /// </para>
    /// <para>
    /// • The <c>FileId</c> parameter identifies the file to download. The operation is
    /// permitted only if the file is marked as downloadable by the API.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of promise lifecycle
    /// events such as start, progress, success, error, and cancellation while still
    /// returning a promise.
    /// </para>
    /// <para>
    /// • On successful resolution, the promise yields a <c>TFileDownloaded</c> instance
    /// containing the file payload encoded as Base64 in its <c>Data</c> property.
    /// </para>
    /// <para>
    /// • The downloaded content can be persisted to disk using
    /// <c>TFileDownloaded.SaveToFile</c> once the promise resolves.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to download.
    /// </param>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise lifecycle callbacks.
    /// </param>
    function AsyncAwaitDownload(
      const FileId: string;
      const Callbacks: TFunc<TPromiseFileDownloaded> = nil): TPromise<TFileDownloaded>;

    /// <summary>
    /// Asynchronously retrieves a list of uploaded files and returns a promise.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates a non-blocking request to the Files endpoint to retrieve a page of file
    /// metadata objects.
    /// </para>
    /// <para>
    /// • It returns a <c>TPromise&lt;TFileList&gt;</c> that resolves with the file list and associated
    /// pagination information when the request completes.
    /// </para>
    /// <para>
    /// • The optional <paramref name="Callbacks"/> factory allows registration of promise lifecycle
    /// handlers such as start, success, and error callbacks.
    /// </para>
    /// <para>
    /// • This overload retrieves the default page of results using server-defined pagination settings
    /// and is suitable for simple listing scenarios.
    /// </para>
    /// </remarks>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise callbacks.
    /// </param>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseFileList> = nil): TPromise<TFileList>; overload;

    /// <summary>
    /// Asynchronously retrieves a list of uploaded files using custom query parameters and returns a promise.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates a non-blocking request to the Files endpoint with user-defined listing
    /// parameters.
    /// </para>
    /// <para>
    /// • The <paramref name="ParamProc"/> callback is used to configure pagination options such as
    /// cursors and page size via <c>TFilesListParams</c>.
    /// </para>
    /// <para>
    /// • It returns a <c>TPromise&lt;TFileList&gt;</c> that resolves with the file list and associated
    /// pagination state when the request completes.
    /// </para>
    /// <para>
    /// • Use this overload when fine-grained control over pagination is required in promise-based or
    /// <c>async/await</c>-style workflows.
    /// </para>
    /// </remarks>
    /// <param name="ParamProc">
    /// Procedure used to configure file listing parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise callbacks.
    /// </param>
    function AsyncAwaitList(
      const ParamProc: TProc<TFilesListParams>;
      const Callbacks: TFunc<TPromiseFileList> = nil): TPromise<TFileList>; overload;

    /// <summary>
    /// Asynchronously retrieves metadata for a specific file and returns a promise.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates a non-blocking request to the Files endpoint to retrieve metadata for the
    /// specified file identifier.
    /// </para>
    /// <para>
    /// • It returns a <c>TPromise&lt;TFile&gt;</c> that resolves with the file metadata when the request
    /// completes successfully or rejects if an error occurs.
    /// </para>
    /// <para>
    /// • The optional <paramref name="Callbacks"/> factory allows registration of promise lifecycle
    /// handlers such as start, success, and error callbacks.
    /// </para>
    /// <para>
    /// • Use this method to integrate file retrieval into promise-based or <c>async/await</c>-style
    /// workflows without blocking the calling thread.
    /// </para>
    /// </remarks>
    /// <param name="FileId">
    /// Unique identifier of the file to retrieve.
    /// </param>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise callbacks.
    /// </param>
    function AsyncAwaitRetrieve(
      const FileId: string;
      const Callbacks: TFunc<TPromiseFile> = nil): TPromise<TFile>; overload;

    /// <summary>
    /// Asynchronously uploads a file and returns a promise.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method initiates a non-blocking multipart/form-data request to upload a file to the Files
    /// endpoint.
    /// </para>
    /// <para>
    /// • The <paramref name="ParamProc"/> callback is used to configure the upload payload, including
    /// the file source (stream or filename) and associated metadata.
    /// </para>
    /// <para>
    /// • It returns a <c>TPromise&lt;TFile&gt;</c> that resolves with the uploaded file metadata when
    /// the operation completes successfully or rejects if an error occurs.
    /// </para>
    /// <para>
    /// • Use this method to integrate file uploads into promise-based or <c>async/await</c>-style
    /// workflows without blocking the calling thread.
    /// </para>
    /// </remarks>
    /// <param name="ParamProc">
    /// Procedure used to configure upload parameters.
    /// </param>
    /// <param name="Callbacks">
    /// Optional factory for configuring promise callbacks.
    /// </param>
    function AsyncAwaitUpload(
      const ParamProc: TProc<TUploadParams>;
      const Callbacks: TFunc<TPromiseFile> = nil): TPromise<TFile>; overload;
  end;

implementation

uses
  Anthropic.Net.MediaCodec;

{ TUploadParams }

function TUploadParams.&File(const FileName: string): TUploadParams;
begin
  AddFile('file', FileName);
  Result := Self;
end;

function TUploadParams.&File(const Stream: TStream;
  const FileName: string): TUploadParams;
begin
  {$IF RTLVersion > 35.0}
  AddStream('file', Stream, True, FileName);
  {$ELSE}
  AddStream('file', Stream, FileName);
  {$ENDIF}
  Result := Self;
end;

{ TFilesRoute }

function TFilesRoute.AsyncAwaitDelete(const FileId: string;
  const Callbacks: TFunc<TPromiseFileDeleted>): TPromise<TFileDeleted>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFileDeleted>(
    procedure(const CallbackParams: TFunc<TAsynFileDeleted>)
    begin
      Self.AsynDelete(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitDownload(const FileId: string;
  const Callbacks: TFunc<TPromiseFileDownloaded>): TPromise<TFileDownloaded>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFileDownloaded>(
    procedure(const CallbackParams: TFunc<TAsynFileDownloaded>)
    begin
      Self.AsynDownload(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseFileList>): TPromise<TFileList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFileList>(
    procedure(const CallbackParams: TFunc<TAsynFileList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitList(const ParamProc: TProc<TFilesListParams>;
  const Callbacks: TFunc<TPromiseFileList>): TPromise<TFileList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFileList>(
    procedure(const CallbackParams: TFunc<TAsynFileList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitRetrieve(const FileId: string;
  const Callbacks: TFunc<TPromiseFile>): TPromise<TFile>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFile>(
    procedure(const CallbackParams: TFunc<TAsynFile>)
    begin
      Self.AsynRetrieve(FileId, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.AsyncAwaitUpload(const ParamProc: TProc<TUploadParams>;
  const Callbacks: TFunc<TPromiseFile>): TPromise<TFile>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TFile>(
    procedure(const CallbackParams: TFunc<TAsynFile>)
    begin
      Self.AsynUpload(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TFilesRoute.List: TFileList;
begin
  Result := API.Get<TFileList>('files');
end;

function TFilesRoute.Delete(const FileId: string): TFileDeleted;
begin
  Result := API.Delete<TFileDeleted>('files/' + FileId);
end;

function TFilesRoute.Download(const FileId: string): TFileDownloaded;
begin
  Result := API.GetMedia<TFileDownloaded>('files/' + FileId + '/content', 'data');
end;

function TFilesRoute.List(const ParamProc: TProc<TFilesListParams>): TFileList;
begin
  Result := API.Get<TFileList, TFilesListParams>('files', ParamProc);
end;

function TFilesRoute.Retrieve(const FileId: string): TFile;
begin
  Result := API.Get<TFile>('files/' + FileId);
end;

function TFilesRoute.Upload(const ParamProc: TProc<TUploadParams>): TFile;
begin
  Result := API.PostForm<TFile, TUploadParams>('files', ParamProc);
end;

{ TFilesListParams }

function TFilesListParams.AfterId(const Value: string): TFilesListParams;
begin
  Result := TFilesListParams(Add('after_id', Value));
end;

function TFilesListParams.BeforeId(const Value: string): TFilesListParams;
begin
  Result := TFilesListParams(Add('before_id', Value));
end;

function TFilesListParams.Limit(const Value: Integer): TFilesListParams;
begin
  Result := TFilesListParams(Add('limit', Value));
end;

{ TFileList }

destructor TFileList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TAsynchronousSupport }

procedure TAsynchronousSupport.AsynDelete(const FileId: string;
  const CallBacks: TFunc<TAsynFileDeleted>);
begin
  with TAsynCallBackExec<TAsynFileDeleted, TFileDeleted>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileDeleted
      begin
        Result := Self.Delete(FileId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynDownload(const FileId: string;
  const CallBacks: TFunc<TAsynFileDownloaded>);
begin
  with TAsynCallBackExec<TAsynFileDownloaded, TFileDownloaded>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileDownloaded
      begin
        Result := Self.Download(FileId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(const CallBacks: TFunc<TAsynFileList>);
begin
  with TAsynCallBackExec<TAsynFileList, TFileList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(
  const ParamProc: TProc<TFilesListParams>;
  const CallBacks: TFunc<TAsynFileList>);
begin
  with TAsynCallBackExec<TAsynFileList, TFileList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFileList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(const FileId: string;
  const CallBacks: TFunc<TAsynFile>);
begin
  with TAsynCallBackExec<TAsynFile, TFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFile
      begin
        Result := Self.Retrieve(FileId);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynUpload(const ParamProc: TProc<TUploadParams>;
  const CallBacks: TFunc<TAsynFile>);
begin
  with TAsynCallBackExec<TAsynFile, TFile>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TFile
      begin
        Result := Self.Upload(ParamProc);
      end);
  finally
    Free;
  end;
end;

{ TFileDownloaded }

procedure TFileDownloaded.SaveToFile(const FileName: string);
begin
  TMediaCodec.TryDecodeBase64ToFile(Data, FileName);
end;

end.
