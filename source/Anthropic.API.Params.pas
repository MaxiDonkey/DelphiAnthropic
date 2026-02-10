unit Anthropic.API.Params;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.Classes, System.JSON, System.SysUtils, System.Types, System.RTTI,
  REST.JsonReflect, REST.Json.Interceptors, System.Generics.Collections;

const
  NULL = 'null';

type
  /// <summary>
  /// Represents a reference to a procedure that takes a single argument of type T and returns no value.
  /// </summary>
  /// <param name="T">
  /// The type of the argument that the referenced procedure will accept.
  /// </param>
  /// <remarks>
  /// This type is useful for defining callbacks or procedures that operate on a variable of type T, allowing for more flexible and reusable code.
  /// </remarks>
  TProcRef<T> = reference to procedure(var Arg: T);

  TJSONInterceptorStringToString = class(TJSONInterceptor)
    constructor Create; reintroduce;
  protected
    RTTI: TRttiContext;
  end;

  TUrlParam = class
  strict private
    FMap: TDictionary<string,string>;
  protected
    function Encode(const S: string): string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(const Name, Value: string): TUrlParam; overload; virtual;
    function Add(const Name: string; Value: Boolean): TUrlParam; overload; virtual;
    function Add(const Name: string; Value: Integer): TUrlParam; overload; virtual;
    function Add(const Name: string; Value: Int64): TUrlParam; overload; virtual;
    function Add(const Name: string; Value: Double): TUrlParam; overload; virtual;
    function Add(const Name: string; const Value: TArray<string>): TUrlParam; overload; virtual;

    function Remove(const Name: string): TUrlParam; virtual;
    function ToQueryString: string;
  end;

  /// <summary>
  /// Represents a base class for all classes obtained after deserialization.
  /// </summary>
  /// <remarks>
  /// This class is designed to store the raw JSON string returned by the API,
  /// allowing applications to access the original JSON response if needed.
  /// </remarks>
  TJSONFingerprint = class
  private
    FJSONResponse: string;

  protected
    /// <summary>
    /// Updates and rebuilds polymorphic content structures after JSON deserialization.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method is responsible for processing content fields that cannot be reliably handled by
    /// automatic RTTI-based deserialization, such as polymorphic or dynamically typed JSON nodes.
    /// </para>
    /// <para>
    /// • It is typically invoked as part of the post-deserialization lifecycle and relies on the
    /// JSONResponse property to extract and rebuild the appropriate internal content representations.
    /// </para>
    /// <para>
    /// • This method is not intended to be called directly by user code and should be considered an
    /// internal implementation detail of the deserialization process.
    /// </para>
    /// </remarks>
    procedure ContentUpdate; virtual;

    /// <summary>
    /// Builds and routes a stream event to its strongly typed internal representation.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method is responsible for interpreting the raw streaming JSON payload and constructing
    /// the appropriate event-specific object graph (for example, content_block_start, content_block_delta,
    /// message_start, message_delta, and message_stop).
    /// </para>
    /// <para>
    /// • It is typically invoked as part of the post-deserialization lifecycle and relies on the
    /// JSONResponse property to parse and bind the event payload to the corresponding internal fields.
    /// </para>
    /// <para>
    /// • This method is not intended to be called directly by user code and should be considered an
    /// internal implementation detail of the streaming deserialization process.
    /// </para>
    procedure StreamEventBuilder; virtual;

    /// <summary>
    /// Executes internal post-deserialization processing for the current instance.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method serves as a protected lifecycle hook that is invoked after the JSON payload has
    /// been fully deserialized, bound, and the JSONResponse property assigned.
    /// </para>
    /// <para>
    /// • It allows derived classes to perform additional internal initialization steps such as invoking
    /// ContentUpdate, StreamEventBuilder, or other class-specific post-processing logic.
    /// </para>
    /// <para>
    /// • This method is not intended to be called directly by user code and should be considered an
    /// internal extension point of the deserialization infrastructure.
    /// </para>
    /// </remarks>
    procedure AfterDeserialize; virtual;

  public
    /// <summary>
    /// Finalizes the deserialization process by executing all internal post-deserialization hooks.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This method is intended to be called exclusively by the deserialization infrastructure after
    /// the object has been fully instantiated, bound, and its JSONResponse property assigned.
    /// </para>
    /// <para>
    /// • It invokes the protected lifecycle hook AfterDeserialize, which allows derived classes to perform
    /// internal post-processing such as rebuilding polymorphic content structures, constructing
    /// stream-specific event data, and finalizing derived or computed properties.
    /// </para>
    /// <para>
    /// • This method is not intended to be called by user code and should be considered part of the
    /// internal deserialization contract. Calling it outside of the deserialization lifecycle may result
    /// in undefined or inconsistent object state.
    /// </para>
    /// </remarks>
    procedure InternalFinalizeDeserialize;

    /// <summary>
    /// Gets or sets the raw JSON string returned by the API.
    /// </summary>
    /// <remarks>
    /// Typically, the API returns a single JSON string, which is stored in this property.
    /// </remarks>
    property JSONResponse: string read FJSONResponse write FJSONResponse;
  end;

  TOptionalContent = class
  private
    FHasValue: Boolean;
  public
    /// <summary>
    /// Indicates whether this instance contains a value provided by the deserialization process.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • A value of <c>True</c> means that this instance was populated from an actual payload
    /// returned by the API and not merely created as a placeholder.
    /// </para>
    /// <para>
    /// • A value of <c>False</c> indicates that no corresponding content was present in the
    /// response, even though the instance itself exists.
    /// </para>
    /// </remarks>
    property HasValue: Boolean read FHasValue;

    /// <summary>
    /// Marks this instance as containing a value provided by the deserialization process.
    /// </summary>
    procedure MarkHasValue; inline;
  end;

  TJSONParam = class
  private
    FJSON: TJSONObject;
    procedure SetJSON(const Value: TJSONObject);
    function GetCount: Integer;
    procedure EnsureJSONAllocated; inline;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const Key: string; const Value: string): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Integer): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Extended): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: Boolean): TJSONParam; overload; virtual;
    function Add(const Key: string; const Value: TDateTime; Format: string): TJSONParam; overload; virtual;

    /// <summary>
    /// Adds a JSON value and transfers ownership of Value to the internal JSONObject.
    /// </summary>
    function Add(const Key: string; const Value: TJSONValue): TJSONParam; overload; virtual;

    /// <summary>
    /// Adds a clone of Value.JSON (does not transfer ownership of Value).
    /// </summary>
    function Add(const Key: string; const Value: TJSONParam): TJSONParam; overload; virtual;

    function Add(const Key: string; Value: TArray<string>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<Integer>): TJSONParam; overload; virtual;
    function Add(const Key: string; Value: TArray<Extended>): TJSONParam; overload; virtual;

    /// <summary>
    /// Adds an array and transfers ownership of the array elements to the internal JSONObject.
    /// </summary>
    function Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam; overload; virtual;

    /// <summary>
    /// Adds an array and transfers ownership of each TJSONParam JSON object into the created array.
    /// Each item is freed by this method.
    /// </summary>
    function Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam; overload; virtual;

    function GetOrCreateObject(const Name: string): TJSONObject;
    function GetOrCreate<T: TJSONValue, constructor>(const Name: string): T;

    procedure Delete(const Key: string); virtual;
    procedure Clear; virtual;

    property Count: Integer read GetCount;
    property JSON: TJSONObject read FJSON write SetJSON;

    function ToJsonString(FreeObject: Boolean = False): string; virtual;
    function ToFormat(FreeObject: Boolean = False): string;
    function ToStringPairs: TArray<TPair<string, string>>;
    function ToStream: TStringStream;

    /// <summary>
    /// Return the JSON value in a TJSONObject and then release the TJSONParam instance.
    /// Consuming/move semantics (internal wrapper usage).
    /// </summary>
    function Detach: TJSONObject;
  end;

  TJSONHelper = record
    class function StringToJson(const Value: string): TJSONObject; static;
    class function StringToJsonArray(const Value: string): TJSONArray; static;
    class function ToJsonArray<T: TJSONParam>(const Value: TArray<T>): TJSONArray; overload; static;
    class function ToJsonArray(const Value: TArray<TJSONObject>): TJSONArray; overload; static;

    class function TryParse(const Value: string; out Json: TJSONValue): Boolean; static;
    class function TryGetObject(const Value: string; out Obj: TJSONObject): Boolean; static;
    class function TryGetArray(const Value: string; out Arr: TJSONArray): Boolean; static;
  end;

const
  DATE_FORMAT = 'YYYY-MM-DD';
  TIME_FORMAT = 'HH:NN:SS';
  DATE_TIME_FORMAT = DATE_FORMAT + ' ' + TIME_FORMAT;

implementation

uses
  System.DateUtils, System.NetEncoding;

{ TJSONInterceptorStringToString }

constructor TJSONInterceptorStringToString.Create;
begin
  ConverterType := ctString;
  ReverterType := rtString;
end;

{ Fetch }

type
  Fetch<T> = class
    type
      TFetchProc = reference to procedure(const Element: T);
  public
    class procedure All(const Items: TArray<T>; Proc: TFetchProc);
  end;

{ Fetch<T> }

class procedure Fetch<T>.All(const Items: TArray<T>; Proc: TFetchProc);
var
  Item: T;
begin
  for Item in Items do
    Proc(Item);
end;

{ TJSONParam }

function TJSONParam.Add(const Key, Value: string): TJSONParam;
begin
  EnsureJSONAllocated;
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONValue): TJSONParam;
begin
  EnsureJSONAllocated;
  Delete(Key);
  FJSON.AddPair(Key, Value);
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TJSONParam): TJSONParam;
begin
  {--- Clone semantics for single TJSONParam to avoid consuming the argument }
  EnsureJSONAllocated;
  if Value = nil then
    begin
      Delete(Key);
      FJSON.AddPair(Key, TJSONNull.Create);
      Exit(Self);
    end;

  Add(Key, TJSONValue(Value.JSON.Clone));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: TDateTime; Format: string): TJSONParam;
begin
  if Format.IsEmpty then
    Format := DATE_TIME_FORMAT;
  Add(Key, FormatDateTime(Format, System.DateUtils.TTimeZone.local.ToUniversalTime(Value)));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Boolean): TJSONParam;
begin
  Add(Key, TJSONBool.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Integer): TJSONParam;
begin
  Add(Key, TJSONNumber.Create(Value));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; const Value: Extended): TJSONParam;
var
  fs: TFormatSettings;
begin
  fs := TFormatSettings.Create('en-US');
  Add(Key, TJSONNumber.Create(FormatFloat('0.############', Value, fs)));
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONValue>): TJSONParam;
begin
  EnsureJSONAllocated;
  var JArr := TJSONArray.Create;
  try
    Fetch<TJSONValue>.All(Value, JArr.AddElement);
    Add(Key, JArr);
  except
    JArr.Free;
    raise;
  end;
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<TJSONParam>): TJSONParam;
begin
  EnsureJSONAllocated;
  var JArr := TJSONArray.Create;
  try
    for var Item in Value do
    try
      if Item = nil then
      begin
        JArr.AddElement(TJSONNull.Create);
        Continue;
      end;

      {--- Transfer ownership of Item.JSON into the array:
           - if Item.JSON is nil, treat it as empty object. }
      if Item.FJSON = nil then
        Item.FJSON := TJSONObject.Create;

      JArr.AddElement(Item.FJSON);
      Item.FJSON := nil;
    finally
      Item.Free;
    end;

    {--- transfers ownership of JArr to FJSON }
    Add(Key, JArr);
  except
    JArr.Free;
    raise;
  end;
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<Extended>): TJSONParam;
begin
  EnsureJSONAllocated;
  var JArr := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        JArr.Add(Item);
      end;
    Add(Key, JArr);
  except
    JArr.Free;
    raise;
  end;
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<Integer>): TJSONParam;
begin
  EnsureJSONAllocated;
  var JArr := TJSONArray.Create;
  try
    for var Item in Value do
      JArr.Add(Item);
    Add(Key, JArr);
  except
    JArr.Free;
    raise;
  end;
  Result := Self;
end;

function TJSONParam.Add(const Key: string; Value: TArray<string>): TJSONParam;
begin
  EnsureJSONAllocated;
  var JArr := TJSONArray.Create;
  try
    for var Item in Value do
      JArr.Add(Item);
    Add(Key, JArr);
  except
    JArr.Free;
    raise;
  end;
  Result := Self;
end;

procedure TJSONParam.Clear;
begin
  FJSON.Free;
  FJSON := TJSONObject.Create;
end;

constructor TJSONParam.Create;
begin
  FJSON := TJSONObject.Create;
end;

procedure TJSONParam.Delete(const Key: string);
begin
  EnsureJSONAllocated;
  var Item := FJSON.RemovePair(Key);
  if Assigned(Item) then
    Item.Free;
end;

destructor TJSONParam.Destroy;
begin
  if Assigned(FJSON) then
    FJSON.Free;
  inherited;
end;

function TJSONParam.GetCount: Integer;
begin
  EnsureJSONAllocated;
  Result := FJSON.Count;
end;

function TJSONParam.GetOrCreate<T>(const Name: string): T;
var
  ExistingValue: TJSONValue;
begin
  EnsureJSONAllocated;

  {--- Fast path: correct typed value exists }
  if FJSON.TryGetValue<T>(Name, Result) then
    Exit;

  {--- If key exists but is wrong type (or null), replace cleanly (avoid duplicate keys) }
  if FJSON.TryGetValue<TJSONValue>(Name, ExistingValue) then
    Delete(Name);

  Result := T.Create;
  FJSON.AddPair(Name, Result);
end;

function TJSONParam.GetOrCreateObject(const Name: string): TJSONObject;
begin
  Result := GetOrCreate<TJSONObject>(Name);
end;

function TJSONParam.Detach: TJSONObject;
{--- consuming semantics (internal usage) }
begin
  EnsureJSONAllocated;
  Result := FJSON;
  FJSON := nil;
  Free;
end;

procedure TJSONParam.EnsureJSONAllocated;
begin
  if FJSON = nil then
    FJSON := TJSONObject.Create;
end;

procedure TJSONParam.SetJSON(const Value: TJSONObject);
begin
  {--- Preserve ownership invariant: TJSONParam owns FJSON.
       If Value = nil -> reset to empty object.}
  if FJSON = Value then
    Exit;

  FJSON.Free;

  if Value <> nil then
    FJSON := Value
  else
    FJSON := TJSONObject.Create;
end;

function TJSONParam.ToFormat(FreeObject: Boolean): string;
begin
  EnsureJSONAllocated;
  Result := FJSON.Format(4);
  if FreeObject then
    Free;
end;

function TJSONParam.ToJsonString(FreeObject: Boolean): string;
begin
  EnsureJSONAllocated;
  Result := FJSON.ToJSON;
  if FreeObject then
    Free;
end;

function TJSONParam.ToStream: TStringStream;
begin
  Result := TStringStream.Create;
  try
    Result.WriteString(ToJsonString(False));
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

function TJSONParam.ToStringPairs: TArray<TPair<string, string>>;
begin
  EnsureJSONAllocated;
  SetLength(Result, 0);
  for var Pair in FJSON do
    Result := Result + [TPair<string, string>.Create(Pair.JsonString.Value, Pair.JsonValue.AsType<string>)];
end;

{ TUrlParam }

constructor TUrlParam.Create;
begin
  inherited;
  FMap := TDictionary<string,string>.Create;
end;

destructor TUrlParam.Destroy;
begin
  FMap.Free;
  inherited;
end;

function TUrlParam.Encode(const S: string): string;
begin
  Result := TNetEncoding.URL.Encode(S).Replace('+','%20');
end;

function TUrlParam.Add(const Name, Value: string): TUrlParam;
begin
  if Value.IsEmpty then
    Exit(Self);

  FMap.AddOrSetValue(Name, Encode(Value));
  Result := Self;
end;

function TUrlParam.Add(const Name: string; Value: Boolean): TUrlParam;
begin
  Result := Add(Name, BoolToStr(Value, True).ToLower);
end;

function TUrlParam.Add(const Name: string; Value: Integer): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

function TUrlParam.Add(const Name: string; Value: Int64): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

function TUrlParam.Add(const Name: string; Value: Double): TUrlParam;
begin
  Result := Add(Name, Value.ToString);
end;

function TUrlParam.Add(const Name: string; const Value: TArray<string>): TUrlParam;
begin
  Result := Add(Name, string.Join(',', Value).Trim);
end;

function TUrlParam.Remove(const Name: string): TUrlParam;
begin
  FMap.Remove(Name);
  Result := Self;
end;

function TUrlParam.ToQueryString: string;
begin
  var StringBuilder := TStringBuilder.Create;
  try
    var First := True;
    for var Item in FMap.Keys do
      begin
        if not First then
          StringBuilder.Append('&')
        else
          First := False;

        StringBuilder.Append(
          Encode(Item))
            .Append('=')
            .Append(FMap[Item]
        );
      end;
    Result := '?' + StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

{ TJSONFingerprint }

procedure TJSONFingerprint.AfterDeserialize;
begin

end;

procedure TJSONFingerprint.ContentUpdate;
begin

end;

procedure TJSONFingerprint.InternalFinalizeDeserialize;
begin
  AfterDeserialize;
end;

procedure TJSONFingerprint.StreamEventBuilder;
begin

end;

{ TJSONHelper }

class function TJSONHelper.StringToJson(const Value: string): TJSONObject;
begin
  var JSON := TJSONObject.ParseJSONValue(Value);
  if not Assigned(JSON) then
    raise Exception.CreateFmt('Invalid JSON: %s', [Value]);

  if not (JSON is TJSONObject) then
    begin
      JSON.Free;
      raise Exception.Create('JSON is not an object');
    end;

  Result := TJSONObject(JSON);
end;

class function TJSONHelper.StringToJsonArray(const Value: string): TJSONArray;
begin
  var JSON := TJSONObject.ParseJSONValue(Value);
  if not Assigned(JSON) then
    raise Exception.CreateFmt('Invalid JSON: %s', [Value]);

  if not (JSON is TJSONArray) then
    begin
      JSON.Free;
      raise Exception.Create('JSON is not an array');
    end;

  Result := TJSONArray(JSON);
end;

class function TJSONHelper.ToJsonArray(
  const Value: TArray<TJSONObject>): TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        if Item = nil then
          Continue;

        Result.Add(Item);
      end;
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONHelper.ToJsonArray<T>(const Value: TArray<T>): TJSONArray;
begin
  Result := TJSONArray.Create;
  try
    for var Item in Value do
      begin
        if Item = nil then
          Continue;

        Result.Add(Item.Detach);
      end;
  except
    Result.Free;
    raise;
  end;
end;

class function TJSONHelper.TryGetArray(const Value: string;
  out Arr: TJSONArray): Boolean;
var
  JSONValue: TJSONValue;
begin
  Arr := nil;
  if not TryParse(Value, JSONValue) then
    Exit(False);

  if JSONValue is TJSONArray then
    begin
      Arr := TJSONArray(JSONValue);
      Exit(True);
    end;

  JSONValue.Free;
  Result := False;
end;

class function TJSONHelper.TryGetObject(const Value: string;
  out Obj: TJSONObject): Boolean;
var
  JSONValue: TJSONValue;
begin
  Obj := nil;
  if not TryParse(Value, JSONValue) then
    Exit(False);

  if JSONValue is TJSONObject then
    begin
      Obj := TJSONObject(JSONValue);
      Exit(True);
    end;

  JSONValue.Free;
  Result := False;
end;

class function TJSONHelper.TryParse(const Value: string;
  out Json: TJSONValue): Boolean;
begin
  Json := nil;
  try
    Json := TJSONObject.ParseJSONValue(Value);
    Result := Json <> nil;
  except
    Json.Free;
    Json := nil;
    Result := False;
  end;
end;

{ TOptionalContent }

procedure TOptionalContent.MarkHasValue;
begin
  FHasValue := True;
end;

end.

