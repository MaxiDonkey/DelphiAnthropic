unit Anthropic.Exceptions;

interface

uses
  System.SysUtils, Anthropic.Errors;

type
  EAnthropicException = class(Exception)
  private
    FCode: Int64;
    FMsg: string;
    FType: string;
    FRequestID: string;
  public
    constructor Create(const ACode: Int64; const AError: TErrorCore); reintroduce; overload;
    constructor Create(const ACode: Int64; const Value: string); reintroduce; overload;
    property Code: Int64 read FCode write FCode;
    property &Type: string read FType write FType;
    property Msg: string read FMsg write FMsg;
    property RequestID: string read FRequestID write FRequestID;
  end;

  /// <summary>
  /// The <c>EAnthropicExceptionAPI</c> class represents a generic API-related exception.
  /// It is thrown when there is an issue with the API configuration or request process,
  /// such as a missing API token, invalid base URL, or other configuration errors.
  /// This class serves as a base for more specific API exceptions.
  /// </summary>
  EAnthropicExceptionAPI = class(Exception);

  /// <summary>
  /// <c>EAnthropicInvalidRequest</c>: There was an issue with the format or content of your request.
  /// We may also use this error type for other 4XX status codes not listed below.
  /// </summary>
  EAnthropicInvalidRequest = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicRateLimited</c>: Your account has hit a rate limit.
  /// </summary>
  EAnthropicRateLimited = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicAuthentication</c>: There's an issue with your API key.
  /// </summary>
  EAnthropicAuthentication = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicPermissionDenied</c>: Your API key does not have permission to use the specified
  /// resource.
  /// </summary>
  EAnthropicPermissionDenied = class(EAnthropicException);

  /// <summary>
  /// An <c>EAnthropicInvalidResponse</c> error occurs when the API response is either empty or not in the expected format.
  /// This error indicates that the API did not return a valid response that can be processed, possibly due to a server-side issue,
  /// a malformed request, or unexpected input data.
  /// </summary>
  EAnthropicInvalidResponse = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicNotFound</c>: The requested resource was not found.
  /// </summary>
  EAnthropicNotFound = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicRequestTooLarge</c>: Request exceeds the maximum allowed number of bytes.
  /// The maximum request size is 32 MB for standard API endpoints.
  /// </para>
  /// </summary>
  EAnthropicRequestTooLarge = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicApiInternalError</c>: An unexpected error has occurred internal to Anthropic's systems.
  /// </summary>
  EAnthropicApiInternalError = class(EAnthropicException);

  /// <summary>
  /// <c>EAnthropicOverloaded</c>: The API is temporarily overloaded.
  /// </summary>
  /// <remarks>
  /// 529 errors can occur when APIs experience high traffic across all users.
  /// <para>
  /// • In rare cases, if your organization has a sharp increase in usage, you might see 429 errors due to
  /// acceleration limits on the API. To avoid hitting acceleration limits, ramp up your traffic gradually
  /// and maintain consistent usage patterns.
  /// </para>
  /// </remarks>
  EAnthropicOverloaded = class(EAnthropicException);

  {$REGION 'Dev note'}

  (*
     Streaming
     ---------
     • When receiving a streaming response via SSE, it's possible that an error can occur after returning
       a 200 response, in which case error handling wouldn't follow these standard mechanisms.

     • See streaming-related error handling in Anthropic.Streaming.


     Request size limits
     -------------------
     • The API enforces request size limits to ensure optimal performance:

      +---------------------+--------------------------+
      | Endpoint Type	      |   Maximum Request Size   |
      +---------------------+--------------------------+
      | Messages API        |   32 MB                  |
      | Token Counting API  |   32 MB                  |
      | Batch API           |   256 MB                 |
      | Files API           |   500 MB                 |
      +---------------------+--------------------------+

      If you exceed these limits, you'll receive a 413 request_too_large error.
      The error is returned from Cloudflare before the request reaches our API servers.


     Long-Running Requests
     ---------------------
     • For long-running requests—especially those with execution times that may exceed 10 minutes—the use
       of the streaming Messages API or the Message Batches API is recommended.

     • Setting a high max_tokens value for non-streaming requests is discouraged unless the streaming
       Messages API or the Message Batches API is used.


     Network Interruption Handling
     -----------------------------
     • Some network infrastructures may terminate idle TCP connections after a variable timeout. This
       behavior can cause requests to fail or time out without returning a response.

     • The Message Batches API helps mitigate this risk by using an asynchronous, polling-based model,
       removing the requirement for a continuously open network connection.


     Direct API Integrations
     -----------------------
     • For direct API integrations, enabling TCP socket keep-alive can reduce the impact of idle connection
       timeouts on certain networks.


     SDK Behavior
     ------------
     • This SDK provides access to the underlying HTTP stack via IHttpClientAPI, allowing consumers to
       configure ResponseTimeout, ConnectionTimeout, and SendTimeout. These timeouts should be adjusted
       based on expected request duration and network conditions. For long-running operations, prefer
       streaming or Message Batches to avoid reliance on a continuously open connection.

       Note: Detailed timeout behavior is documented in HttpClient-related units.
  *)

  {$ENDREGION}

implementation

{ EAnthropicException }

constructor EAnthropicException.Create(const ACode: Int64; const Value: string);
begin
  Code := ACode;
  Msg := Value;
  inherited Create(Format('error %s: %s', [ACode.ToString, Msg]));
end;

constructor EAnthropicException.Create(const ACode: Int64;
  const AError: TErrorCore);
begin
  Code := ACode;
  Msg := (AError as TError).Error.Message;
  FRequestID := (AError as TError).RequestId;

  if FRequestID.IsEmpty then
    inherited Create(Format(
     'error (%s): '+ sLineBreak +
     '  • %s',
     [Code.ToString, Msg]))
  else
    inherited Create(Format(
      'error (%s): '+ sLineBreak +
      '  • %s'  + sLineBreak +
      '  • %s',
      [Code.ToString, Msg, 'request_id: ' + RequestID]));
end;

end.
