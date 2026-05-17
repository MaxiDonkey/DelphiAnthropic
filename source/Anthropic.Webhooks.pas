unit Anthropic.Webhooks;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Net.URLClient,
  REST.Json.Types,
  Anthropic.API.Params, Anthropic.Types;

type
  EAnthropicWebhookException = class(Exception);
  EAnthropicWebhookMissingSigningKey = class(EAnthropicWebhookException);
  EAnthropicWebhookMissingHeader = class(EAnthropicWebhookException);
  EAnthropicWebhookInvalidSignature = class(EAnthropicWebhookException);
  EAnthropicWebhookStalePayload = class(EAnthropicWebhookException);
  EAnthropicWebhookInvalidPayload = class(EAnthropicWebhookException);

  TWebhookEventData = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('organization_id')]
    FOrganizationId: string;
    FType: string;
    [JsonNameAttribute('vault_id')]
    FVaultId: string;
    [JsonNameAttribute('workspace_id')]
    FWorkspaceId: string;
  public
    property Id: string read FId write FId;
    property OrganizationId: string read FOrganizationId write FOrganizationId;
    property &Type: string read FType write FType;
    property VaultId: string read FVaultId write FVaultId;
    property WorkspaceId: string read FWorkspaceId write FWorkspaceId;

    function TryGetEventType(out Value: TWebhookEventType): Boolean;
    function EventType: TWebhookEventType;
    function ResourceKind: TWebhookResourceKind;
    function IsSessionEvent: Boolean;
    function IsSessionThreadEvent: Boolean;
    function IsVaultEvent: Boolean;
    function IsVaultCredentialEvent: Boolean;
  end;

  TWebhookEvent = class(TJSONFingerprint)
  private
    FId: string;
    [JsonNameAttribute('created_at')]
    FCreatedAt: string;
    FData: TWebhookEventData;
    FType: string;
  public
    destructor Destroy; override;

    property Id: string read FId write FId;
    property CreatedAt: string read FCreatedAt write FCreatedAt;
    property Data: TWebhookEventData read FData write FData;
    property &Type: string read FType write FType;
  end;

  TWebhookVerifier = record
  public const
    DefaultToleranceSeconds = 300;
  private const
    SigningKeyPrefix = 'whsec_';
  private
    class function BuildSignedPayload(const WebhookId, WebhookTimestamp: string;
      const Payload: TBytes): TBytes; static;

    class function ConstantTimeEquals(const A, B: TBytes): Boolean; static;

    class procedure CopyBytes(const Source: TBytes; var Target: TBytes;
      var Offset: Integer); static;

    class function DecodeBase64(const Value: string): TBytes; static;

    class function DecodeSigningSecret(const SigningKey: string): TBytes; static;

    class function ExtractSignature(const Fragment: string;
      out Signature: string): Boolean; static;

    class function GetHeaderValue(const Headers: TNetHeaders;
      const Names: array of string; out Value: string): Boolean; static;

    class function ParseEvent(const PayloadText: string): TWebhookEvent; static;

    class procedure VerifyTimestamp(const Timestamp: string;
      const MaxAgeSeconds: Integer); static;

  public
    class function Verify(const Payload: string; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds): Boolean; overload; static;

    class function Verify(const Payload: TBytes; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds): Boolean; overload; static;

    class procedure VerifyOrRaise(const Payload: string; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds); overload; static;

    class procedure VerifyOrRaise(const Payload: TBytes; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds); overload; static;

    class function Unwrap(const Payload: string; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds): TWebhookEvent; overload; static;

    class function Unwrap(const Payload: TBytes; const Headers: TNetHeaders;
      const SigningKey: string;
      const MaxAgeSeconds: Integer = DefaultToleranceSeconds): TWebhookEvent; overload; static;
  end;

  TWebhookReceiver = class
  private
    FSigningKey: string;
    FMaxAgeSeconds: Integer;
  public
    constructor Create(const SigningKey: string;
      const MaxAgeSeconds: Integer = TWebhookVerifier.DefaultToleranceSeconds);

    function Verify(const Payload: string; const Headers: TNetHeaders): Boolean; overload;
    function Verify(const Payload: TBytes; const Headers: TNetHeaders): Boolean; overload;
    function Unwrap(const Payload: string; const Headers: TNetHeaders): TWebhookEvent; overload;
    function Unwrap(const Payload: TBytes; const Headers: TNetHeaders): TWebhookEvent; overload;

    property SigningKey: string read FSigningKey write FSigningKey;
    property MaxAgeSeconds: Integer read FMaxAgeSeconds write FMaxAgeSeconds;
  end;

implementation

uses
  System.DateUtils, System.Hash, System.JSON, System.NetEncoding,
  REST.Json,
  Anthropic.Api.JsonFingerprintBinder;

{ TWebhookEventData }

function TWebhookEventData.EventType: TWebhookEventType;
begin
  if not TryGetEventType(Result) then
    raise EAnthropicWebhookInvalidPayload.CreateFmt(
      'Unknown webhook event type "%s"', [FType]);
end;

function TWebhookEventData.IsSessionEvent: Boolean;
begin
  Result := ResourceKind = TWebhookResourceKind.session;
end;

function TWebhookEventData.IsSessionThreadEvent: Boolean;
begin
  Result := ResourceKind = TWebhookResourceKind.session_thread;
end;

function TWebhookEventData.IsVaultCredentialEvent: Boolean;
begin
  Result := ResourceKind = TWebhookResourceKind.vault_credential;
end;

function TWebhookEventData.IsVaultEvent: Boolean;
begin
  Result := ResourceKind = TWebhookResourceKind.vault;
end;

function TWebhookEventData.ResourceKind: TWebhookResourceKind;
begin
  var Value: TWebhookEventType;
  if TryGetEventType(Value) then
    Result := Value.ResourceKind
  else
    Result := TWebhookResourceKind.unknown;
end;

function TWebhookEventData.TryGetEventType(
  out Value: TWebhookEventType): Boolean;
begin
  Result := TWebhookEventType.TryParse(FType, Value);
end;

{ TWebhookEvent }

destructor TWebhookEvent.Destroy;
begin
  FData.Free;
  inherited;
end;

{ TWebhookVerifier }

class function TWebhookVerifier.BuildSignedPayload(const WebhookId,
  WebhookTimestamp: string; const Payload: TBytes): TBytes;
var
  IdBytes: TBytes;
  TimestampBytes: TBytes;
  Offset: Integer;
begin
  IdBytes := TEncoding.UTF8.GetBytes(WebhookId);
  TimestampBytes := TEncoding.UTF8.GetBytes(WebhookTimestamp);

  SetLength(Result, Length(IdBytes) + Length(TimestampBytes) + Length(Payload) + 2);

  Offset := 0;
  CopyBytes(IdBytes, Result, Offset);
  Result[Offset] := Ord('.');
  Inc(Offset);
  CopyBytes(TimestampBytes, Result, Offset);
  Result[Offset] := Ord('.');
  Inc(Offset);
  CopyBytes(Payload, Result, Offset);
end;

class function TWebhookVerifier.ConstantTimeEquals(const A, B: TBytes): Boolean;
var
  Diff: Byte;
begin
  if Length(A) <> Length(B) then
    Exit(False);

  Diff := 0;
  for var I := 0 to High(A) do
    Diff := Diff or (A[I] xor B[I]);

  Result := Diff = 0;
end;

class procedure TWebhookVerifier.CopyBytes(const Source: TBytes;
  var Target: TBytes; var Offset: Integer);
begin
  if Length(Source) = 0 then
    Exit;

  Move(Source[0], Target[Offset], Length(Source));
  Inc(Offset, Length(Source));
end;

class function TWebhookVerifier.DecodeBase64(const Value: string): TBytes;
begin
  var Normalized := Value.Trim.Replace('-', '+').Replace('_', '/');
  while (Length(Normalized) mod 4) <> 0 do
    Normalized := Normalized + '=';

  Result := TNetEncoding.Base64.DecodeStringToBytes(Normalized);
end;

class function TWebhookVerifier.DecodeSigningSecret(
  const SigningKey: string): TBytes;
begin
  var Key := SigningKey.Trim;

  if Key.IsEmpty then
    raise EAnthropicWebhookMissingSigningKey.Create('Webhook signing key is empty');

  if Key.StartsWith(SigningKeyPrefix) then
    Key := Key.Substring(Length(SigningKeyPrefix));

  try
    Result := DecodeBase64(Key);
  except
    raise EAnthropicWebhookInvalidSignature.Create(
      'Webhook signing key is not valid base64');
  end;
end;

class function TWebhookVerifier.ExtractSignature(const Fragment: string;
  out Signature: string): Boolean;
begin
  Signature := EmptyStr;

  var S := Fragment.Trim;
  if S.IsEmpty then
    Exit(False);

  var Pos := S.IndexOf(',');
  if Pos >= 0 then
    begin
      if not SameText(S.Substring(0, Pos).Trim, 'v1') then
        Exit(False);

      Signature := S.Substring(Pos + 1).Trim;
      Exit(not Signature.IsEmpty);
    end;

  Pos := S.IndexOf('=');
  if Pos >= 0 then
    begin
      if not SameText(S.Substring(0, Pos).Trim, 'v1') then
        Exit(False);

      Signature := S.Substring(Pos + 1).Trim;
      Exit(not Signature.IsEmpty);
    end;

  Signature := S;
  Result := True;
end;

class function TWebhookVerifier.GetHeaderValue(const Headers: TNetHeaders;
  const Names: array of string; out Value: string): Boolean;
begin
  for var Header in Headers do
    for var Name in Names do
      if SameText(Header.Name, Name) then
        begin
          Value := Header.Value;
          Exit(True);
        end;

  Value := EmptyStr;
  Result := False;
end;

class function TWebhookVerifier.ParseEvent(
  const PayloadText: string): TWebhookEvent;
var
  JsonValue: TJSONValue;
  Formatted: string;
begin
  JsonValue := TJSONObject.ParseJSONValue(PayloadText);
  if JsonValue = nil then
    raise EAnthropicWebhookInvalidPayload.Create('Webhook payload is not valid JSON');

  try
    if not (JsonValue is TJSONObject) then
      raise EAnthropicWebhookInvalidPayload.Create('Webhook payload is not a JSON object');

    Formatted := JsonValue.Format;
  finally
    JsonValue.Free;
  end;

  try
    Result := TJson.JsonToObject<TWebhookEvent>(PayloadText);
  except
    raise EAnthropicWebhookInvalidPayload.Create('Webhook payload does not match the event schema');
  end;

  if Result = nil then
    raise EAnthropicWebhookInvalidPayload.Create('Webhook payload could not be parsed');

  try
    if Result.Data = nil then
      raise EAnthropicWebhookInvalidPayload.Create('Webhook payload is missing data');

    Result.JSONPayload := PayloadText;
    Result.JSONResponse := Formatted;
    TJSONFingerprintBinder.Bind(Result, Formatted);
    Result.InternalFinalizeDeserialize;
  except
    Result.Free;
    raise;
  end;
end;

class procedure TWebhookVerifier.VerifyOrRaise(const Payload: string;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer);
begin
  VerifyOrRaise(TEncoding.UTF8.GetBytes(Payload), Headers, SigningKey, MaxAgeSeconds);
end;

class procedure TWebhookVerifier.VerifyOrRaise(const Payload: TBytes;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer);
var
  WebhookId: string;
  WebhookTimestamp: string;
  SignatureHeader: string;
  Secret: TBytes;
  SignedPayload: TBytes;
  ExpectedSignature: TBytes;
begin
  if not GetHeaderValue(Headers, ['webhook-id', 'x-webhook-id'], WebhookId) then
    raise EAnthropicWebhookMissingHeader.Create('Missing webhook-id header');

  if not GetHeaderValue(Headers, ['webhook-timestamp', 'x-webhook-timestamp'], WebhookTimestamp) then
    raise EAnthropicWebhookMissingHeader.Create('Missing webhook-timestamp header');

  if not GetHeaderValue(Headers, ['webhook-signature', 'x-webhook-signature'], SignatureHeader) then
    raise EAnthropicWebhookMissingHeader.Create('Missing webhook-signature header');

  VerifyTimestamp(WebhookTimestamp, MaxAgeSeconds);

  Secret := DecodeSigningSecret(SigningKey);
  SignedPayload := BuildSignedPayload(WebhookId, WebhookTimestamp, Payload);
  ExpectedSignature := THashSHA2.GetHMACAsBytes(
    SignedPayload, Secret, THashSHA2.TSHA2Version.SHA256);

  for var Fragment in SignatureHeader.Split([' ']) do
    begin
      var SignatureValue: string;
      if not ExtractSignature(Fragment, SignatureValue) then
        Continue;

      try
        var ActualSignature := DecodeBase64(SignatureValue);
        if ConstantTimeEquals(ExpectedSignature, ActualSignature) then
          Exit;
      except
        Continue;
      end;
    end;

  raise EAnthropicWebhookInvalidSignature.Create('Webhook signature is invalid');
end;

class procedure TWebhookVerifier.VerifyTimestamp(const Timestamp: string;
  const MaxAgeSeconds: Integer);
var
  TimestampValue: Int64;
  NowUnix: Int64;
  Delta: Int64;
begin
  if not TryStrToInt64(Timestamp.Trim, TimestampValue) then
    raise EAnthropicWebhookInvalidSignature.Create('Webhook timestamp is invalid');

  if MaxAgeSeconds <= 0 then
    Exit;

  NowUnix := DateTimeToUnix(Now, False);
  Delta := NowUnix - TimestampValue;
  if Delta < 0 then
    Delta := -Delta;

  if Delta > MaxAgeSeconds then
    raise EAnthropicWebhookStalePayload.Create('Webhook payload is stale');
end;

class function TWebhookVerifier.Verify(const Payload: string;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer): Boolean;
begin
  Result := Verify(TEncoding.UTF8.GetBytes(Payload), Headers, SigningKey, MaxAgeSeconds);
end;

class function TWebhookVerifier.Verify(const Payload: TBytes;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer): Boolean;
begin
  try
    VerifyOrRaise(Payload, Headers, SigningKey, MaxAgeSeconds);
    Result := True;
  except
    Result := False;
  end;
end;

class function TWebhookVerifier.Unwrap(const Payload: string;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer): TWebhookEvent;
begin
  VerifyOrRaise(Payload, Headers, SigningKey, MaxAgeSeconds);
  Result := ParseEvent(Payload);
end;

class function TWebhookVerifier.Unwrap(const Payload: TBytes;
  const Headers: TNetHeaders; const SigningKey: string;
  const MaxAgeSeconds: Integer): TWebhookEvent;
begin
  VerifyOrRaise(Payload, Headers, SigningKey, MaxAgeSeconds);
  Result := ParseEvent(TEncoding.UTF8.GetString(Payload));
end;

{ TWebhookReceiver }

constructor TWebhookReceiver.Create(const SigningKey: string;
  const MaxAgeSeconds: Integer);
begin
  inherited Create;
  FSigningKey := SigningKey;
  FMaxAgeSeconds := MaxAgeSeconds;
end;

function TWebhookReceiver.Unwrap(const Payload: string;
  const Headers: TNetHeaders): TWebhookEvent;
begin
  Result := TWebhookVerifier.Unwrap(Payload, Headers, FSigningKey, FMaxAgeSeconds);
end;

function TWebhookReceiver.Unwrap(const Payload: TBytes;
  const Headers: TNetHeaders): TWebhookEvent;
begin
  Result := TWebhookVerifier.Unwrap(Payload, Headers, FSigningKey, FMaxAgeSeconds);
end;

function TWebhookReceiver.Verify(const Payload: string;
  const Headers: TNetHeaders): Boolean;
begin
  Result := TWebhookVerifier.Verify(Payload, Headers, FSigningKey, FMaxAgeSeconds);
end;

function TWebhookReceiver.Verify(const Payload: TBytes;
  const Headers: TNetHeaders): Boolean;
begin
  Result := TWebhookVerifier.Verify(Payload, Headers, FSigningKey, FMaxAgeSeconds);
end;

end.
