unit Anthropic.Batches;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API.Params, Anthropic.API, Anthropic.Types, Anthropic.Exceptions,
  Anthropic.Chat.Request,

  Anthropic.Async.Support, Anthropic.Async.Promise;

type
  /// <summary>
  /// The <c>TBatchParams</c> class is used to manage and define parameters for a batch of messages.
  /// It provides methods to customize the batch with specific identifiers and additional parameters.
  /// </summary>
  TBatchParams = class(TJSONParam)
    /// <summary>
    /// Developer-provided ID created for each request in a Message Batch. Useful for matching results
    /// to requests, as results may be given out of request order.
    /// </summary>
    /// <param name="Value">
    /// A string representing the custom ID to be added to the batch. maxLength 64, minLength 1
    /// </param>
    /// <returns>
    /// The updated <c>TBatchParams</c> instance with the custom ID included.
    /// </returns>
    /// <remarks>
    /// Must be unique for each request within the Message Batch.
    /// </remarks>
    function CustomId(const Value: string): TBatchParams;

    /// <summary>
    /// Messages API creation parameters for the individual request.
    /// </summary>
    function Params(const Value: TChatParams): TBatchParams; overload;

    class function New: TBatchParams;
  end;

  /// <summary>
  /// The <c>TRequestParams</c> class is used to manage and define request parameters for sending message batches.
  /// It allows you to specify multiple batch requests as part of a single request operation.
  /// </summary>
  TRequestParams = class(TJSONParam)
  public
    /// <summary>
    /// List of requests for prompt completion. Each is an individual request to create a Message.
    /// </summary>
    /// <param name="Value">
    /// An array of <c>TBatchParams</c> instances, where each represents the parameters for an individual batch request.
    /// </param>
    /// <returns>
    /// The updated <c>TRequestParams</c> instance containing the specified batch requests.
    /// </returns>
    function Requests(Value: TArray<TBatchParams>): TRequestParams; overload;

    /// <summary>
    /// List of requests for prompt completion. Each is an individual request to create a Message.
    /// </summary>
    /// <param name="FilePath">
    /// The JSONL filename.
    /// </param>
    /// <returns>
    /// The updated <c>TRequestParams</c> instance containing the specified batch requests.
    /// </returns>
    function Requests(Value: string): TRequestParams; overload;
  end;

  TRequestParamProc = TProc<TRequestParams>;

  /// <summary>
  /// The <c>TRequestCounts</c> class represents the counts of different statuses related to batch processing.
  /// It tracks the number of batches that are currently being processed, successfully completed, errored, canceled, and expired.
  /// </summary>
  /// <remarks>
  /// This class provides an overview of the state of batch processing by categorizing the results into several status types,
  /// helping to monitor the success and failure rates of batch operations.
  /// </remarks>
  TRequestCounts = class
  private
    FProcessing: Int64;
    FSucceeded: Int64;
    FErrored: Int64;
    FCanceled: Int64;
    FExpired: Int64;
  public
    /// <summary>
    /// Number of requests in the Message Batch that are processing.
    /// </summary>
    property Processing: Int64 read FProcessing write FProcessing;

    /// <summary>
    /// Number of requests in the Message Batch that have completed successfully.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Succeeded: Int64 read FSucceeded write FSucceeded;

    /// <summary>
    /// Number of requests in the Message Batch that encountered an error.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Errored: Int64 read FErrored write FErrored;

    /// <summary>
    /// Number of requests in the Message Batch that have been canceled.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Canceled: Int64 read FCanceled write FCanceled;

    /// <summary>
    /// Number of requests in the Message Batch that have expired.
    /// <para>
    /// This is zero until processing of the entire Message Batch has ended.
    /// </para>
    /// </summary>
    property Expired: Int64 read FExpired write FExpired;
  end;

  /// <summary>
  /// The <c>TBatch</c> class represents a batch of messages in the system.
  /// It contains detailed information about the batch, including its processing status, request counts, timestamps, and related URLs.
  /// </summary>
  /// <remarks>
  /// This class provides key details for managing and tracking a batch of messages, such as the batch's unique identifier,
  /// its current state (in progress, canceled, or ended), and related metadata. It is essential for operations that involve handling
  /// message batches in a structured and organized manner.
  /// </remarks>
  TBatch = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
    [JsonNameAttribute('archived_at')]
    FArchivedAt: string;
    [JsonNameAttribute('processing_status')]
    FProcessingStatus: TProcessingStatusType;
    [JsonNameAttribute('request_counts')]
    FRequestCounts: TRequestCounts;
    [JsonNameAttribute('ended_at')]
    FEndedAt: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    [JsonNameAttribute('expires_at')]
    FExpiresAt: string;
    [JsonNameAttribute('cancel_initiated_at')]
    FCancelInitiatedAt: string;
    [JsonNameAttribute('results_url')]
    FResultsUrl: string;
  public
    /// <summary>
    /// Unique object identifier.
    /// </summary>
    /// <remarks>
    /// The format and length of IDs may change over time.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// For Message Batches, this is always "message_batch".
    /// </summary>
    /// <remarks>
    /// Available options: message_batch
    /// </remarks>
    property &Type: string read FType write FType;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which the Message Batch was archived and its
    /// results became unavailable.
    /// </summary>
    /// <remarks>
    /// format date-time
    /// </remarks>
    property ArchivedAt: string read FArchivedAt write FArchivedAt;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which cancellation was initiated for the Message Batch. Specified only if cancellation was initiated.
    /// </summary>
    property CancelInitiatedAt: string read FCancelInitiatedAt write FCancelInitiatedAt;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which the Message Batch was created.
    /// </summary>
    property CreatedAt: string read FCreatedAt write FCreatedAt;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which processing for the Message Batch ended. Specified only once processing ends.
    /// </summary>
    /// <remarks>
    /// Processing ends when every request in a Message Batch has either succeeded, errored, canceled, or expired.
    /// </remarks>
    property EndedAt: string read FEndedAt write FEndedAt;

    /// <summary>
    /// RFC 3339 datetime string representing the time at which the Message Batch will expire and end processing, which is 24 hours after creation.
    /// </summary>
    property ExpiresAt: string read FExpiresAt write FExpiresAt;

    /// <summary>
    /// Processing status of the Message Batch.
    /// </summary>
    /// <remarks>
    /// Available options: in_progress, canceling, ended
    /// </remarks>
    property ProcessingStatus: TProcessingStatusType read FProcessingStatus write FProcessingStatus;

    /// <summary>
    /// Tallies requests within the Message Batch, categorized by their status.
    /// </summary>
    /// <remarks>
    /// Requests start as processing and move to one of the other statuses only once processing of the entire batch ends. The sum of all values always matches the total number of requests in the batch.
    /// </remarks>
    property RequestCounts: TRequestCounts read FRequestCounts write FRequestCounts;

    /// <summary>
    /// URL to a .jsonl file containing the results of the Message Batch requests. Specified only once processing ends.
    /// </summary>
    /// <remarks>
    /// Results in the file are not guaranteed to be in the same order as requests. Use the <b>custom_id</b> field to match results to requests.
    /// </remarks>
    property ResultsUrl: string read FResultsUrl write FResultsUrl;

    /// <summary>
    /// Destructor to clean up resources used by this <c>TBatch</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as <b>RequestCounts</>, is properly released when the object is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TBatchList</c> class represents a collection of batch objects, along with metadata about the batch list.
  /// It includes information about whether there are more batches to be fetched and provides identifiers for pagination purposes.
  /// </summary>
  /// <remarks>
  /// This class is used to handle lists of batches returned from the API, enabling pagination through the first and last batch identifiers
  /// and indicating whether additional batches are available beyond the current list.
  /// </remarks>
  TBatchList = class(TJSONFingerprint)
  private
    FData: TArray<TBatch>;
    [JsonNameAttribute('has_more')]
    FHasMore: Boolean;
    [JsonNameAttribute('first_id')]
    FFirstId: string;
    [JsonNameAttribute('last_id')]
    FLastId: string;
  public
    /// <summary>
    /// Array of batches of messages
    /// </summary>
    property Data: TArray<TBatch> read FData write FData;

    /// <summary>
    /// Indicates if there are more results in the requested page direction.
    /// </summary>
    property HasMore: Boolean read FHasMore write FHasMore;

    /// <summary>
    /// First ID in the data list. Can be used as the before_id for the previous page.
    /// </summary>
    property FirstId: string read FFirstId write FFirstId;

    /// <summary>
    /// Last ID in the data list. Can be used as the after_id for the next page.
    /// </summary>
    property LastId: string read FLastId write FLastId;

    destructor Destroy; override;
  end;

  /// <summary>
  /// The <c>TBatchDelete</c> class represents a batch deletion operation.
  /// It provides information about the identifier and type of the batch being deleted.
  /// </summary>
  /// <remarks>
  /// This class is used to manage the deletion of batches, enabling the application
  /// to track which batch is being removed from the system. It encapsulates the batch
  /// identifier and type information necessary for deletion requests.
  /// </remarks>
  TBatchDelete = class(TJSONFingerprint)
  private
    FId: string;
    FType: string;
  public
    /// <summary>
    /// Gets or sets the unique identifier of the batch to be deleted.
    /// </summary>
    /// <remarks>
    /// This property specifies the unique batch ID that is used to identify
    /// the batch to be removed from the system.
    /// </remarks>
    property Id: string read FId write FId;

    /// <summary>
    /// Gets or sets the type of the batch being deleted.
    /// </summary>
    /// <remarks>
    /// For most cases, this will be set to "message_batch" to indicate
    /// that the batch being deleted is of type message batch.
    /// </remarks>
    property &Type: string read FType write FType;
  end;

  /// <summary>
  /// The <c>TListParams</c> class is used to define parameters for retrieving lists of batches.
  /// It allows for pagination by setting limits, specifying batch IDs to start after, or ending before.
  /// </summary>
  /// <remarks>
  /// This class helps in controlling the number of results returned in list queries and enables efficient data navigation
  /// through the use of pagination parameters such as <c>Limit</c>, <c>AfterId</c>, and <c>BeforeId</c>.
  /// <para>
  /// <b>--- Warning:</b> The parameters <c>AfterId</c> and <c>BeforeId</c> are mutually exclusive, meaning that both cannot be used simultaneously
  /// in a single query. Ensure that only one of these parameters is set at a time to avoid conflicts.
  /// </para>
  /// </remarks>
  TListParams = class(TUrlParam)
  public
    /// <summary>
    /// Sets the limit for the number of batches to be retrieved.
    /// </summary>
    /// <param name="Value">
    /// An integer representing the limit. The valid range is 1 to 100.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified limit.
    /// </returns>
    /// <exception cref="Exception">
    /// Thrown if the value is less than 1 or greater than 100.
    /// </exception>
    /// <remarks>
    /// The default value of limit set to 20.
    /// </remarks>
    function Limit(const Value: Integer): TListParams;

    /// <summary>
    /// Sets the batch ID that will be used as a reference to fetch batches created after it.
    /// </summary>
    /// <param name="Value">
    /// A string representing the batch ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified <c>after_id</c> value.
    /// </returns>
    function AfterId(const Value: string): TListParams;

    /// <summary>
    /// Sets the batch ID that will be used as a reference to fetch batches created before it.
    /// </summary>
    /// <param name="Value">
    /// A string representing the batch ID.
    /// </param>
    /// <returns>
    /// The current instance of <c>TListParams</c> with the specified <c>before_id</c> value.
    /// </returns>
    function BeforeId(const Value: string): TListParams;
  end;

  TListParamProc = TProc<TListParams>;

  /// <summary>
  /// Defines an asynchronous callback handler for batch responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynBatch</c> is a type alias for <c>TAsynCallBack&lt;TBatch&gt;</c>, specialized for handling
  /// Message Batch objects returned by the Batches endpoints.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatch</c>.
  /// </para>
  /// <para>
  /// • This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TBatch&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynBatch</c> when registering callbacks for batch creation, retrieval, cancellation,
  /// or other operations that return a <c>TBatch</c> result without blocking the calling thread.
  /// </para>
  /// </remarks>
  TAsynBatch = TAsynCallBack<TBatch>;

  /// <summary>
  /// Defines a promise-based callback handler for batch responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseBatch</c> is a type alias for <c>TPromiseCallback&lt;TBatch&gt;</c>, specialized for
  /// consuming Message Batch objects returned by the Batches endpoints.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatch</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TBatch&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseBatch</c> when consuming batch creation, retrieval, cancellation, or other batch
  /// operations through promise-based abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseBatch = TPromiseCallback<TBatch>;

  /// <summary>
  /// Defines an asynchronous callback handler for batch list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynBatchList</c> is a type alias for <c>TAsynCallBack&lt;TBatchList&gt;</c>, specialized for
  /// handling paginated collections of Message Batches returned by the Batches listing endpoints.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatchList</c>.
  /// </para>
  /// <para>
  /// • This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TBatchList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynBatchList</c> when registering callbacks that list Message Batches without blocking
  /// the calling thread.
  /// </para>
  /// </remarks>
  TAsynBatchList = TAsynCallBack<TBatchList>;

  /// <summary>
  /// Defines a promise-based callback handler for batch list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseBatchList</c> is a type alias for <c>TPromiseCallback&lt;TBatchList&gt;</c>, specialized
  /// for consuming paginated collections of Message Batches returned by the Batches listing endpoints.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatchList</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of <c>TPromiseCallback&lt;TBatchList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseBatchList</c> when listing Message Batches through promise-based abstractions
  /// instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseBatchList = TPromiseCallback<TBatchList>;

  /// <summary>
  /// Defines an asynchronous callback handler for string list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynStringList</c> is a type alias for <c>TAsynCallBack&lt;TStringList&gt;</c>, specialized for
  /// handling textual results returned as string lists.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to operations that return
  /// <c>TStringList</c>, such as downloading or materializing batch result files.
  /// </para>
  /// <para>
  /// • This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TStringList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynStringList</c> when registering callbacks that consume textual or file-based
  /// responses without blocking the calling thread.
  /// </para>
  /// </remarks>
  TAsynStringList = TAsynCallBack<TStringList>;

  /// <summary>
  /// Defines a promise-based callback handler for string list responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseStringList</c> is a type alias for <c>TPromiseCallback&lt;TStringList&gt;</c>,
  /// specialized for consuming textual or file-based results returned as string lists.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to operations that return
  /// <c>TStringList</c>, such as retrieving or persisting batch result files.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption
  /// semantics while preserving the complete behavior of
  /// <c>TPromiseCallback&lt;TStringList&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseStringList</c> when consuming textual or file-based responses through
  /// promise-based abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseStringList = TPromiseCallback<TStringList>;

  /// <summary>
  /// Defines an asynchronous callback handler for batch deletion responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TAsynBatchDelete</c> is a type alias for <c>TAsynCallBack&lt;TBatchDelete&gt;</c>, specialized
  /// for handling responses returned by batch deletion operations.
  /// </para>
  /// <para>
  /// • It binds the generic asynchronous callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatchDelete</c>.
  /// </para>
  /// <para>
  /// • This alias improves readability and intent expression in public APIs while preserving the
  /// complete behavior and lifecycle semantics of <c>TAsynCallBack&lt;TBatchDelete&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TAsynBatchDelete</c> when registering callbacks that delete Message Batches without
  /// blocking the calling thread.
  /// </para>
  /// </remarks>
  TAsynBatchDelete = TAsynCallBack<TBatchDelete>;

  /// <summary>
  /// Defines a promise-based callback handler for batch deletion responses.
  /// </summary>
  /// <remarks>
  /// <para>
  /// • <c>TPromiseBatchDelete</c> is a type alias for <c>TPromiseCallback&lt;TBatchDelete&gt;</c>,
  /// specialized for consuming responses returned by batch deletion operations.
  /// </para>
  /// <para>
  /// • It binds the generic promise-based callback infrastructure to the Batches domain by fixing the
  /// callback payload type to <c>TBatchDelete</c>.
  /// </para>
  /// <para>
  /// • This alias improves API clarity by explicitly expressing promise-oriented consumption semantics
  /// while preserving the complete behavior of
  /// <c>TPromiseCallback&lt;TBatchDelete&gt;</c>.
  /// </para>
  /// <para>
  /// • Use <c>TPromiseBatchDelete</c> when consuming batch deletion results through promise-based
  /// abstractions instead of direct callbacks.
  /// </para>
  /// </remarks>
  TPromiseBatchDelete = TPromiseCallback<TBatchDelete>;

  TAbstractSupport = class(TAnthropicAPIRoute)
  protected
    function Cancel(const Id: string): TBatch; virtual; abstract;

    function Create(const ParamProc: TProc<TRequestParams>): TBatch; overload; virtual; abstract;

    function Create(const FilePath: string): TBatch; overload; virtual; abstract;

    function Delete(const Id: string): TBatchDelete; virtual; abstract;

    function List: TBatchList; overload; virtual; abstract;

    function List(const ParamProc: TProc<TListParams>): TBatchList; overload; virtual; abstract;

    function Retrieve(const Id: string): TBatch; overload; virtual; abstract;

    function Retrieve(const Id: string; const FileName: string): TStringList; overload; virtual; abstract;
  end;

  TAsynchronousSupport = class(TAbstractSupport)
  protected
    procedure AsynCancel(
      const Id: string;
      const CallBacks: TFunc<TAsynBatch>);

    procedure AsynCreate(
      const ParamProc: TProc<TRequestParams>;
      const CallBacks: TFunc<TAsynBatch>); overload;

    procedure AsynCreate(
      const FilePath: string;
      const CallBacks: TFunc<TAsynBatch>); overload;

    procedure AsynDelete(
      const Id: string;
      const CallBacks: TFunc<TAsynBatchDelete>);

    procedure AsynList(
      const CallBacks: TFunc<TAsynBatchList>); overload;

    procedure AsynList(
      const ParamProc: TProc<TListParams>;
      const CallBacks: TFunc<TAsynBatchList>); overload;

    procedure AsynRetrieve(
      const Id: string;
      const CallBacks: TFunc<TAsynBatch>); overload;

    procedure ASynRetrieve(
      const Id: string;
      const FileName: string;
      const CallBacks: TFunc<TAsynStringList>); overload;
  end;

  TBatchRoute = class(TAsynchronousSupport)
  public
    /// <summary>
    /// Cancels an in-progress message batch.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the batch cancellation endpoint and attempts to
    /// stop processing of the specified Message Batch.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to cancel. Cancellation is only effective
    /// if the batch is still in progress.
    /// </para>
    /// <para>
    /// • On success, the returned <c>TBatch</c> instance reflects the updated batch state, including
    /// cancellation timestamps and processing status.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynCancel</c> or <c>AsyncAwaitCancel</c>.
    /// </para>
    /// </remarks>
    function Cancel(const Id: string): TBatch; override;

    /// <summary>
    /// Creates a new message batch using the provided request parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batches creation endpoint and
    /// returns a <c>TBatch</c> instance representing the newly created batch.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TRequestParams</c> payload,
    /// including the list of batch requests and their associated message parameters.
    /// </para>
    /// <para>
    /// • On success, the returned batch contains its unique identifier, processing status,
    /// timestamps, and request count metadata.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynCreate</c> or <c>AsyncAwaitCreate</c>.
    /// </para>
    /// </remarks>
    function Create(const ParamProc: TProc<TRequestParams>): TBatch; overload; override;

    /// <summary>
    /// Creates a new message batch from a JSONL file.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batches creation endpoint using
    /// a JSONL file containing the batch request definitions.
    /// </para>
    /// <para>
    /// • The <c>FilePath</c> parameter specifies the path to a JSONL file where each line represents
    /// an individual request within the Message Batch.
    /// </para>
    /// <para>
    /// • On success, the returned <c>TBatch</c> instance represents the newly created batch,
    /// including its unique identifier, processing status, and associated metadata.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynCreate</c> or <c>AsyncAwaitCreate</c>.
    /// </para>
    /// </remarks>
    function Create(const FilePath: string): TBatch; overload; override;

    /// <summary>
    /// Deletes a message batch.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the batch deletion endpoint and permanently
    /// removes the specified Message Batch.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to delete. Deletion is only permitted
    /// for batches that are no longer in progress.
    /// </para>
    /// <para>
    /// • On success, the returned <c>TBatchDelete</c> instance confirms the deletion by providing
    /// the batch identifier and type metadata.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynDelete</c> or <c>AsyncAwaitDelete</c>.
    /// </para>
    /// </remarks>
    function Delete(const Id: string): TBatchDelete; override;

    /// <summary>
    /// Retrieves the list of message batches.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batches listing endpoint and returns
    /// a <c>TBatchList</c> collection containing available message batches.
    /// </para>
    /// <para>
    /// • Batches are returned in reverse chronological order, with the most recently created batches
    /// listed first.
    /// </para>
    /// <para>
    /// • The returned object includes both batch data and pagination metadata, such as
    /// <c>HasMore</c>, <c>FirstId</c>, and <c>LastId</c>.
    /// </para>
    /// <para>
    /// • For paginated access or non-blocking usage, prefer the overloaded <c>List</c> method with
    /// parameters, <c>AsynList</c>, or <c>AsyncAwaitList</c>.
    /// </para>
    /// </remarks>
    function List: TBatchList; overload; override;

    /// <summary>
    /// Retrieves a paginated list of message batches using query parameters.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batches listing endpoint and returns
    /// a <c>TBatchList</c> collection filtered and paginated according to the provided parameters.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure pagination options such as <c>limit</c>,
    /// <c>after_id</c>, and <c>before_id</c>.
    /// </para>
    /// <para>
    /// • The returned object includes both batch data and pagination metadata, enabling cursor-based
    /// navigation through result pages.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynList</c> or <c>AsyncAwaitList</c>.
    /// </para>
    /// </remarks>
    function List(const ParamProc: TProc<TListParams>): TBatchList; overload; override;

    /// <summary>
    /// Retrieves details for a specific message batch.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batch retrieval endpoint and returns
    /// a <c>TBatch</c> instance describing the specified batch.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to retrieve.
    /// </para>
    /// <para>
    /// • On success, the returned object contains batch metadata such as processing status, request
    /// counts, timestamps, and result location information.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynRetrieve</c> or <c>AsyncAwaitRetrieve</c>.
    /// </para>
    /// </remarks>
    function Retrieve(const Id: string): TBatch; overload; override;

    /// <summary>
    /// Retrieves the results of a message batch and saves them to a file.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method performs a blocking request to the Message Batch results endpoint and returns
    /// the batch results as a <c>TStringList</c>.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch whose results are to be retrieved.
    /// </para>
    /// <para>
    /// • The <c>FileName</c> parameter specifies the file path where the retrieved results will be
    /// persisted in JSONL format using UTF-8 encoding.
    /// </para>
    /// <para>
    /// • The returned <c>TStringList</c> contains the raw result payload, which may not be ordered in
    /// the same sequence as the original batch requests.
    /// </para>
    /// <para>
    /// • For non-blocking usage, prefer <c>AsynRetrieve</c> or <c>AsyncAwaitRetrieve</c>.
    /// </para>
    /// </remarks>
    function Retrieve(const Id: string; const FileName: string): TStringList; overload; override;

    /// <summary>
    /// Cancels an in-progress message batch using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous batch cancellation workflow into a
    /// <c>TPromise&lt;TBatch&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to cancel. Cancellation is effective
    /// only while the batch is still in progress.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with an updated <c>TBatch</c> instance reflecting the batch
    /// state after cancellation, or is rejected if an error occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCancel(
      const Id: string;
      const Callbacks: TFunc<TPromiseBatch> = nil): TPromise<TBatch>;

    /// <summary>
    /// Creates a new message batch using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch creation workflow into a
    /// <c>TPromise&lt;TBatch&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure the <c>TRequestParams</c> payload,
    /// including the list of batch requests and their associated message parameters.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatch</c> instance representing the newly created
    /// batch, or is rejected if an error occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreate(
      const ParamProc: TProc<TRequestParams>;
      const Callbacks: TFunc<TPromiseBatch> = nil): TPromise<TBatch>; overload;

    /// <summary>
    /// Creates a new message batch from a JSONL file using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch creation workflow into a
    /// <c>TPromise&lt;TBatch&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>FilePath</c> parameter specifies the path to a JSONL file where each line represents
    /// an individual request within the Message Batch.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatch</c> instance representing the newly created
    /// batch, or is rejected if an error occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitCreate(
      const FilePath: string;
      const Callbacks: TFunc<TPromiseBatch> = nil): TPromise<TBatch>; overload;

    /// <summary>
    /// Deletes a message batch using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch deletion workflow into a
    /// <c>TPromise&lt;TBatchDelete&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to delete. Deletion is permitted only
    /// for batches that are no longer in progress.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatchDelete</c> instance confirming the deletion
    /// or is rejected if an error occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitDelete(
      const Id: string;
      const Callbacks: TFunc<TPromiseBatchDelete> = nil): TPromise<TBatchDelete>;

    /// <summary>
    /// Retrieves the list of message batches using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch listing workflow into a
    /// <c>TPromise&lt;TBatchList&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatchList</c> collection containing available
    /// message batches and pagination metadata.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const Callbacks: TFunc<TPromiseBatchList> = nil): TPromise<TBatchList>; overload;

    /// <summary>
    /// Retrieves a paginated list of message batches using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch listing workflow into a
    /// <c>TPromise&lt;TBatchList&gt;</c>, enabling async/await-style consumption with pagination support.
    /// </para>
    /// <para>
    /// • The <c>ParamProc</c> callback is used to configure pagination options such as <c>limit</c>,
    /// <c>after_id</c>, and <c>before_id</c>.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events while the
    /// paginated batch collection is returned through the promise result.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatchList</c> collection filtered according to the
    /// provided parameters or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitList(
      const ParamProc: TProc<TListParams>;
      const Callbacks: TFunc<TPromiseBatchList> = nil): TPromise<TBatchList>; overload;

    /// <summary>
    /// Retrieves details for a specific message batch using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch retrieval workflow into a
    /// <c>TPromise&lt;TBatch&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch to retrieve.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TBatch</c> instance describing the requested batch
    /// or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitRetrieve(
      const Id: string;
      const Callbacks: TFunc<TPromiseBatch> = nil): TPromise<TBatch>; overload;

    /// <summary>
    /// Retrieves the results of a message batch using an async/await promise-based pattern.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method wraps the asynchronous Message Batch results retrieval workflow into a
    /// <c>TPromise&lt;TStringList&gt;</c>, enabling async/await-style consumption.
    /// </para>
    /// <para>
    /// • The <c>Id</c> parameter identifies the Message Batch whose results are to be retrieved.
    /// </para>
    /// <para>
    /// • The <c>FileName</c> parameter specifies the file path where the retrieved results will be
    /// persisted in JSONL format using UTF-8 encoding.
    /// </para>
    /// <para>
    /// • The optional <c>Callbacks</c> factory allows observation of lifecycle events such as start,
    /// success, error, and cancellation while still returning a promise.
    /// </para>
    /// <para>
    /// • The returned promise resolves with a <c>TStringList</c> containing the raw batch result
    /// payload or is rejected if an error or cancellation occurs.
    /// </para>
    /// </remarks>
    function AsyncAwaitRetrieve(
      const Id: string;
      const FileName: string;
      const Callbacks: TFunc<TPromiseStringList> = nil): TPromise<TStringList>; overload;
  end;

implementation

uses
  System.StrUtils, System.Rtti, Rest.Json, Anthropic.JSONL;

{ TBatcheParamsParams }

function TBatchParams.CustomId(const Value: string): TBatchParams;
begin
  Result := TBatchParams(Add('custom_id', Value));
end;

class function TBatchParams.New: TBatchParams;
begin
  Result := TBatchParams.Create;
end;

function TBatchParams.Params(const Value: TChatParams): TBatchParams;
begin
  Result := TBatchParams(Add('params', Value.Detach));
end;

{ TRequestParams }

function TRequestParams.Requests(Value: TArray<TBatchParams>): TRequestParams;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.Detach);
  Result := TRequestParams(Add('requests', JSONArray));
end;

function TRequestParams.Requests(Value: string): TRequestParams;
var
  JSONArray: TJSONArray;
begin
  if TJSONHelper.TryGetArray(Value, JSONArray) then
    Exit(TRequestParams(Add('requests', JSONArray)));

  raise EAnthropicException.Create('Invalid JSON Array');
end;

{ TBatchRoute }

function TBatchRoute.AsyncAwaitCancel(const Id: string;
  const Callbacks: TFunc<TPromiseBatch>): TPromise<TBatch>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatch>(
    procedure(const CallbackParams: TFunc<TAsynBatch>)
    begin
      Self.AsynCancel(Id, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitCreate(const ParamProc: TProc<TRequestParams>;
  const Callbacks: TFunc<TPromiseBatch>): TPromise<TBatch>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatch>(
    procedure(const CallbackParams: TFunc<TAsynBatch>)
    begin
      Self.AsynCreate(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitCreate(const FilePath: string;
  const Callbacks: TFunc<TPromiseBatch>): TPromise<TBatch>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatch>(
    procedure(const CallbackParams: TFunc<TAsynBatch>)
    begin
      Self.AsynCreate(FilePath, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitDelete(const Id: string;
  const Callbacks: TFunc<TPromiseBatchDelete>): TPromise<TBatchDelete>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatchDelete>(
    procedure(const CallbackParams: TFunc<TAsynBatchDelete>)
    begin
      Self.AsynDelete(Id, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitList(const ParamProc: TProc<TListParams>;
  const Callbacks: TFunc<TPromiseBatchList>): TPromise<TBatchList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatchList>(
    procedure(const CallbackParams: TFunc<TAsynBatchList>)
    begin
      Self.AsynList(ParamProc, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitList(
  const Callbacks: TFunc<TPromiseBatchList>): TPromise<TBatchList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatchList>(
    procedure(const CallbackParams: TFunc<TAsynBatchList>)
    begin
      Self.AsynList(CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitRetrieve(const Id: string;
  const Callbacks: TFunc<TPromiseBatch>): TPromise<TBatch>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TBatch>(
    procedure(const CallbackParams: TFunc<TAsynBatch>)
    begin
      Self.AsynRetrieve(Id, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.AsyncAwaitRetrieve(const Id, FileName: string;
  const Callbacks: TFunc<TPromiseStringList>): TPromise<TStringList>;
begin
  Result := TAsyncAwaitHelper.WrapAsyncAwait<TStringList>(
    procedure(const CallbackParams: TFunc<TAsynStringList>)
    begin
      Self.AsynRetrieve(Id, FileName, CallbackParams);
    end,
    Callbacks);
end;

function TBatchRoute.Cancel(const Id: string): TBatch;
begin
  Result := API.Post<TBatch>(Format('messages/batches/%s/cancel', [Id]));
end;

function TBatchRoute.Create(const FilePath: string): TBatch;
begin
  var JsonLContent := TJSONLHelper.LoadFromFile(FilePath);

  Result := Create(
    procedure (Params: TRequestParams)
    begin
      Params.Requests(JsonLContent);
    end);
end;

function TBatchRoute.Create(const ParamProc: TProc<TRequestParams>): TBatch;
begin
  Result := API.Post<TBatch, TRequestParams>('messages/batches', ParamProc);
end;

function TBatchRoute.Delete(const Id: string): TBatchDelete;
begin
  Result := API.Delete<TBatchDelete>('messages/batches/' + Id);
end;

function TBatchRoute.List: TBatchList;
begin
  Result := API.Get<TBatchList>('messages/batches');
end;

function TBatchRoute.List(const ParamProc: TProc<TListParams>): TBatchList;
begin
  Result := API.Get<TBatchList, TListParams>('messages/batches', ParamProc);
end;

function TBatchRoute.Retrieve(const Id: string): TBatch;
begin
  Result := API.Get<TBatch>('messages/batches/' + Id);
end;

function TBatchRoute.Retrieve(
  const Id: string;
  const FileName: string): TStringList;
begin
  Result := TStringList.Create;
  with Result do
    begin
      var Response := API.Get(Format('messages/batches/%s/results', [Id]));
      Text := Response;
      SaveToFile(FileName, TEncoding.UTF8);
    end;
end;

{ TBatchList }

destructor TBatchList.Destroy;
begin
  for var Item in FData do
    Item.Free;
  inherited;
end;

{ TBatch }

destructor TBatch.Destroy;
begin
  if Assigned(FRequestCounts) then
    FRequestCounts.Free;
  inherited;
end;

{ TListParams }

function TListParams.AfterId(const Value: string): TListParams;
begin
  Result := TListParams(Add('after_id', Value));
end;

function TListParams.BeforeId(const Value: string): TListParams;
begin
  Result := TListParams(Add('before_id', Value));
end;

function TListParams.Limit(const Value: Integer): TListParams;
begin
  Result := TListParams(Add('limit', Value));
end;

{ TAsynchronousSupport }

procedure TAsynchronousSupport.AsynCreate(
  const ParamProc: TProc<TRequestParams>;
  const CallBacks: TFunc<TAsynBatch>);
begin
  with TAsynCallBackExec<TAsynBatch, TBatch>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatch
      begin
        Result := Self.Create(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynCancel(
  const Id: string;
  const CallBacks: TFunc<TAsynBatch>);
begin
  with TAsynCallBackExec<TAsynBatch, TBatch>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatch
      begin
        Result := Self.Cancel(Id);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynCreate(
  const FilePath: string;
  const CallBacks: TFunc<TAsynBatch>);
begin
  with TAsynCallBackExec<TAsynBatch, TBatch>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatch
      begin
        Result := Self.Create(FilePath);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynDelete(
  const Id: string;
  const CallBacks: TFunc<TAsynBatchDelete>);
begin
  with TAsynCallBackExec<TAsynBatchDelete, TBatchDelete>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchDelete
      begin
        Result := Self.Delete(Id);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(
  const ParamProc: TProc<TListParams>;
  const CallBacks: TFunc<TAsynBatchList>);
begin
  with TAsynCallBackExec<TAsynBatchList, TBatchList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchList
      begin
        Result := Self.List(ParamProc);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(
  const Id: string;
  const CallBacks: TFunc<TAsynBatch>);
begin
  with TAsynCallBackExec<TAsynBatch, TBatch>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatch
      begin
        Result := Self.Retrieve(Id);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynRetrieve(
  const Id: string;
  const FileName: string;
  const CallBacks: TFunc<TAsynStringList>);
begin
  with TAsynCallBackExec<TAsynStringList, TStringList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TStringList
      begin
        Result := Self.Retrieve(Id, FileName);
      end);
  finally
    Free;
  end;
end;

procedure TAsynchronousSupport.AsynList(
  const CallBacks: TFunc<TAsynBatchList>);
begin
  with TAsynCallBackExec<TAsynBatchList, TBatchList>.Create(CallBacks) do
  try
    Sender := Use.Param.Sender;
    OnStart := Use.Param.OnStart;
    OnSuccess := Use.Param.OnSuccess;
    OnError := Use.Param.OnError;
    Run(
      function: TBatchList
      begin
        Result := Self.List;
      end);
  finally
    Free;
  end;
end;

end.

