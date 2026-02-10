unit Anthropic.Batches.Support;

interface

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

uses
  System.SysUtils, System.Classes, REST.JsonReflect, System.JSON, System.Threading,
  REST.Json.Types, Anthropic.Chat.Responses;

type
  TBatcheResultItem = class;

  /// <summary>
  /// Provides an interface for managing batch results and associated file information.
  /// </summary>
  /// <remarks>
  /// The IBatcheResults interface allows for the retrieval of batch result items
  /// and management of the file from which these results are loaded. It ensures
  /// that the specified file exists before loading and provides access to individual
  /// batch results through the Batches property.
  /// </remarks>
  IBatcheResults = interface
    ['{6C5431EA-9862-49A8-8F2C-91D8AD7A45BC}']
    /// <summary>
    /// Retrieves the name of the file associated with the batch results.
    /// </summary>
    /// <value>
    /// A string representing the filename.
    /// </value>
    function GetFileName: string;
    /// <summary>
    /// Sets the name of the file associated with the batch results.
    /// </summary>
    /// <param name="Value">
    /// A string representing the filename to be set.
    /// </param>
    /// <remarks>
    /// This method validates the existence of the specified file before setting the filename.
    /// If the file does not exist, an exception is raised.
    /// </remarks>
    procedure SetFileName(const Value: string);
    /// <summary>
    /// Retrieves the array of batch result items.
    /// </summary>
    /// <value>
    /// An array of <see cref="TBatcheResultItem"/> instances representing individual batch results.
    /// </value>
    function GetBatches: TArray<TBatcheResultItem>;
    /// <summary>
    /// Gets the collection of batch result items.
    /// </summary>
    /// <value>
    /// An array of <see cref="TBatcheResultItem"/> representing the batch results.
    /// </value>
    property Batches: TArray<TBatcheResultItem> read GetBatches;
    /// <summary>
    /// Gets or sets the name of the file associated with the batch results.
    /// </summary>
    /// <value>
    /// A string representing the filename.
    /// </value>
    /// <remarks>
    /// When setting the FileName property, the interface ensures that the specified file exists.
    /// It then loads and parses the batch results from the file.
    /// </remarks>
    property FileName: string read GetFileName write SetFileName;
  end;

  /// <summary>
  /// Factory class responsible for creating instances of <see cref="IBatcheResults"/>.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResultsFactory</c> provides a centralized way to instantiate <c>IBatcheResults</c>
  /// objects, optionally initializing them with a specified file. This ensures that all
  /// instances are created consistently throughout the application.
  /// </remarks>
  TBatcheResultsFactory = class
    /// <summary>
    /// Creates a new instance of <see cref="IBatcheResults"/>.
    /// </summary>
    /// <param name="FileName">
    /// An optional parameter specifying the file name to initialize the <c>IBatcheResults</c> instance.
    /// If provided, the factory will load batch results from the specified file.
    /// </param>
    /// <returns>
    /// Returns an instance of <c>IBatcheResults</c>. If <paramref name="FileName"/> is provided and valid,
    /// the returned instance will be initialized with the data from the file.
    /// </returns>
    /// <remarks>
    /// The factory method ensures that the created <c>IBatcheResults</c> instance is properly initialized.
    /// If the <paramref name="FileName"/> does not exist, an exception will be raised.
    /// </remarks>
    class function CreateInstance(const FileName: string = ''): IBatcheResults;
  end;

  /// <summary>
  /// Represents the result of a single batch operation.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResult</c> class contains detailed information about the outcome of a batch request,
  /// including the type of result and the associated chat message.
  /// </remarks>
  TBatcheResult = class
  private
    FType: string;
    FMessage: TChat;
  public
    /// <summary>
    /// Gets or sets the type of the batch result.
    /// </summary>
    /// <remarks>
    /// This property typically indicates the nature or category of the result.
    /// </remarks>
    property &type: string read Ftype write Ftype;
    /// <summary>
    /// Gets or sets the chat message associated with the batch result.
    /// </summary>
    /// <remarks>
    /// The <c>Message</c> property holds the <c>TChat</c> object that contains the detailed information
    /// of the chat message resulting from the batch operation.
    /// </remarks>
    property Message: TChat read FMessage write FMessage;
    /// <summary>
    /// Destructor to clean up resources used by this <c>TBatcheResult</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the <c>Message</c> object, are properly released
    /// when the instance is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

  /// <summary>
  /// Represents an individual item within a batch result.
  /// </summary>
  /// <remarks>
  /// The <c>TBatcheResultItem</c> class encapsulates a single result item from a batch operation,
  /// including a custom identifier and the corresponding batch result.
  /// </remarks>
  TBatcheResultItem = class
  private
    [JsonNameAttribute('custom_id')]
    FCustomId: string;
    FResult: TBatcheResult;
  public
    /// <summary>
    /// Gets or sets the custom identifier associated with the batch result item.
    /// </summary>
    /// <remarks>
    /// The <c>CustomId</c> property allows for matching results to specific requests within a batch
    /// by using a unique custom identifier provided during the batch creation.
    /// </remarks>
    property CustomId: string read FCustomId write FCustomId;
    /// <summary>
    /// Gets or sets the result of the batch operation.
    /// </summary>
    /// <remarks>
    /// The <c>Result</c> property holds the <c>TBatcheResult</c> object that contains the outcome
    /// and details of the specific batch request associated with the <c>CustomId</c>.
    /// </remarks>
    property Result: TBatcheResult read FResult write FResult;
    /// <summary>
    /// Destructor to clean up resources used by this <c>TBatcheResultItem</c> instance.
    /// </summary>
    /// <remarks>
    /// The destructor ensures that any allocated resources, such as the <c>Result</c> object, are properly released
    /// when the instance is no longer needed.
    /// </remarks>
    destructor Destroy; override;
  end;

implementation

uses
  REST.Json;

type
  TBatcheResults = class(TInterfacedObject, IBatcheResults)
  private
    FBatches: TArray<TBatcheResultItem>;
    FFileName: string;
    function GetFileName: string;
    procedure SetFileName(const Value: string);
    procedure Clear;
    procedure Reflexion(const Value: string);
    function GetBatches: TArray<TBatcheResultItem>;
  public
    destructor Destroy; override;
  end;

{ TBatcheResults }

procedure TBatcheResults.Clear;
begin
  for var Item in FBatches do
    Item.Free;
  FBatches := [];
end;

destructor TBatcheResults.Destroy;
begin
  Clear;
  inherited;
end;

function TBatcheResults.GetBatches: TArray<TBatcheResultItem>;
begin
  Result := FBatches;
end;

function TBatcheResults.GetFileName: string;
begin
  Result := FFileName;
end;

procedure TBatcheResults.Reflexion(const Value: string);
begin
  FBatches := FBatches + [TJson.JsonToObject<TBatcheResultItem>(Value)];
end;

procedure TBatcheResults.SetFileName(const Value: string);
begin
  if not FileExists(Value) then
    raise Exception.CreateFmt('File not found' + sLineBreak + '%s', [Value]);
  Clear;
  FFileName := Value;
  with TStringList.Create do
  try
    LoadFromFile(FFileName, TEncoding.UTF8);
    with GetEnumerator do
    try
      while MoveNext do
        Reflexion(Current);
    finally
      Free;
    end;
  finally
    Free;
  end;
end;

{ TBatcheResultsFactory }

class function TBatcheResultsFactory.CreateInstance(const FileName: string): IBatcheResults;
begin
  Result := TBatcheResults.Create;
  if not FileName.IsEmpty then
    Result.FileName := FileName;
end;

{ TBatcheResultItem }

destructor TBatcheResultItem.Destroy;
begin
  if Assigned(FResult) then
    FResult.Free;
  inherited;
end;

{ TBatcheResult }

destructor TBatcheResult.Destroy;
begin
  if Assigned(FMessage) then
    FMessage.Free;
  inherited;
end;

end.
