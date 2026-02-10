unit Anthropic.Chat.Beta;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes,  System.JSON,
  REST.JsonReflect, REST.Json.Types,
  Anthropic.API, Anthropic.API.Params, Anthropic.Types, Anthropic.API.JsonSafeReader;

type
  {$REGION 'Text Citation'}

  TTextCitation = class
  private
    [JsonReflectAttribute(ctString, rtString, TCitationsTypeInterceptor)]
    FType: TCitationsType;
    [JsonNameAttribute('cited_text')]
    FCitedText: string;
    [JsonNameAttribute('document_index')]
    FDocumentIndex: Double;
    [JsonNameAttribute('document_title')]
    FDocumentTitle: string;
    [JsonNameAttribute('end_char_index')]
    FEndCharIndex: Double;
    [JsonNameAttribute('file_id')]
    FFileId: string;
    [JsonNameAttribute('start_char_index')]
    FStartCharIndex: Double;

    [JsonNameAttribute('end_page_number')]
    FEndPageNumber: Double;
    [JsonNameAttribute('start_page_number')]
    FStartPageNumber: Double;

    [JsonNameAttribute('end_block_index')]
    FEndBlockIndex: Double;
    [JsonNameAttribute('start_block_index')]
    FStartBlockIndex: Double;

    [JsonNameAttribute('encrypted_index')]
    FEncryptedIndex: string;
    FTitle: string;
    FUrl: string;

    [JsonNameAttribute('search_result_index')]
    FSearchResultIndex: Double;
    FSource: string;
  public
    property &Type: TCitationsType read FType write FType;
    property CitedText: string read FCitedText write FCitedText;
    property DocumentIndex: Double read FDocumentIndex write FDocumentIndex;
    property DocumentTitle: string read FDocumentTitle write FDocumentTitle;
    property FileId: string read FFileId write FFileId;

    {--- char_location }
    property EndCharIndex: Double read FEndCharIndex write FEndCharIndex;
    property StartCharIndex: Double read FStartCharIndex write FStartCharIndex;

    {--- page_location }
    property EndPageNumber: Double read FEndPageNumber write FEndPageNumber;
    property StartPageNumber: Double read FStartPageNumber write FStartPageNumber;

    {--- content_block_location }
    property EndBlockIndex: Double read FEndBlockIndex write FEndBlockIndex;
    property StartBlockIndex: Double read FStartBlockIndex write FStartBlockIndex;

    {--- web_search_result_location }
    property EncryptedIndex: string read FEncryptedIndex write FEncryptedIndex;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;

    {--- search_result_location }
    property SearchResultIndex: Double read FSearchResultIndex write FSearchResultIndex;
    property Source: string read FSource write FSource;
  end;

  TCitationConfig = class
  private
    FEnabled: Boolean;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  {$ENDREGION}

  TToolResultErrorCode = class
  private
    FType: string;
    [JsonNameAttribute('error_code')]
    FErrorCode: string;
  public
    property &Type: string read FType write FType;
    property ErrorCode: string read FErrorCode write FErrorCode;
  end;

  TToolResultErrorCodeMessage = class(TToolResultErrorCode)
  private
    [JsonNameAttribute('error_message')]
    FErrorMessage: string;
  public
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
  end;

  {$REGION 'WebSearchToolResultBlock'}

  TWebSearchToolResultBlockContent = class(TToolResultErrorCode)
  private
    FType: string;
    [JsonNameAttribute('encrypted_content')]
    FEncryptedContent: string;
    [JsonNameAttribute('page_age')]
    FPageAge: string;
    FTitle: string;
    FUrl: string;
  public
    property &Type: string read FType write FType;
    property EncryptedContent: string read FEncryptedContent write FEncryptedContent;
    property PageAge: string read FPageAge write FPageAge;
    property Title: string read FTitle write FTitle;
    property Url: string read FUrl write FUrl;
  end;

  TWebSearchToolResultBlock = class(TOptionalContent)
  private
    FContent: TArray<TWebSearchToolResultBlockContent>;
  public
    property Content: TArray<TWebSearchToolResultBlockContent> read FContent write FContent;
    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'WebFetchToolResultBlock'}

  TDocumentSource = class
  private
    FType: string;
    FData: string;
    [JsonNameAttribute('media_type')]
    FMediaType: string;
  public
    property &Type: string read FType write FType;

    property Data: string read FData write FData;

    property MediaType: string read FMediaType write FMediaType;
  end;

  TDocumentBlock = class
  private
    FType: string;
    FTitle: string;
    FCitations: TCitationConfig;
    FSource: TDocumentSource;
  public
    property &Type: string read FType write FType;

    /// <summary>
    /// Citation configuration for the document
    /// </summary>
    property Citations: TCitationConfig read FCitations write FCitations;

    property Source: TDocumentSource read FSource write FSource;

    /// <summary>
    /// The title of the document
    /// </summary>
    property Title: string read FTitle write FTitle;

    destructor Destroy; override;
  end;

  TWebFetchToolResultBlockContent = class(TToolResultErrorCode)
  private
    FType: string;
    [JsonNameAttribute('retrieved_at')]
    FContent: TDocumentBlock;
    FRetrievedAt: string;
    FUrl: string;
  public
    property &Type: string read FType write FType;

    property Content: TDocumentBlock read FContent write FContent;

    /// <summary>
    /// ISO 8601 timestamp when the content was retrieved
    /// </summary>
    property RetrievedAt: string read FRetrievedAt write FRetrievedAt;

    /// <summary>
    /// Fetched content URL
    /// </summary>
    property Url: string read FUrl write FUrl;

    destructor Destroy; override;
  end;

  TWebFetchToolResultBlock = class(TOptionalContent)
  private
    FContent: TWebFetchToolResultBlockContent;
  public
    property Content: TWebFetchToolResultBlockContent read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'CodeExecutionToolResultBlock'}

  TCodeExecutionOutputBlock = class
  private
    FType: string;
    [JsonNameAttribute('file_id')]
    FFileId: string;
  public
    property &Type: string read FType write FType;
    property FileId: string read FFileId write FFileId;
  end;

  TCodeExecutionToolResultBlockContent = class(TToolResultErrorCode)
  private
    FType: string;
    FContent: TArray<TCodeExecutionOutputBlock>;
    [JsonNameAttribute('return_code')]
    FReturnCode: Double;
    FStderr: string;
    FStdout: string;
  public
    property &Type: string read FType write FType;

    property Content: TArray<TCodeExecutionOutputBlock> read FContent write FContent;
    property ReturnCode: Double read FReturnCode write FReturnCode;
    property Stderr: string read FStderr write FStderr;
    property Stdout: string read FStdout write FStdout;

    destructor Destroy; override;
  end;

  TCodeExecutionToolResultBlock = class(TOptionalContent)
  private
    FContent: TCodeExecutionToolResultBlockContent;
  public
    property Content: TCodeExecutionToolResultBlockContent read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'BashCodeExecutionToolResultBlock'}

  TBashCodeExecutionResultBlock = class
  private
    FType: string;
    [JsonNameAttribute('file_id')]
    FFileId: string;
  public
    property &Type: string read FType write FType;
    property FileId: string read FFileId write FFileId;
  end;

  TBashCodeExecutionToolResultBlockContent = class(TToolResultErrorCode)
  private
    FType: string;
    FContent: TArray<TBashCodeExecutionResultBlock>;
    [JsonNameAttribute('return_code')]
    FReturnCode: Double;
    FStderr: string;
    FStdout: string;
  public
    property &Type: string read FType write FType;

    property Content: TArray<TBashCodeExecutionResultBlock> read FContent write FContent;
    property ReturnCode: Double read FReturnCode write FReturnCode;
    property Stderr: string read FStderr write FStderr;
    property Stdout: string read FStdout write FStdout;

    destructor Destroy; override;
  end;

  TBashCodeExecutionToolResultBlock = class(TOptionalContent)
  private
    FContent: TBashCodeExecutionToolResultBlockContent;
  public
    property Content: TBashCodeExecutionToolResultBlockContent read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'TextEditorCodeExecutionToolResultBlock'}

  TTextEditorCodeExecutionToolResultBlockContent = class(TToolResultErrorCodeMessage)
  private
    FType: string;
    // TextEditorCodeExecutionViewResultBlock
    FContent: string;
    [JsonNameAttribute('file_type')]
    FFileType: string;
    [JsonNameAttribute('num_lines')]
    FNumLines: Int64;
    [JsonNameAttribute('start_line')]
    FStartLine: Int64;
    [JsonNameAttribute('total_lines')]
    FTotalLines: Int64;

    // TextEditorCodeExecutionCreateResultBlock
    [JsonNameAttribute('is_file_update')]
    FIsFileUpdate: Boolean;

    // TextEditorCodeExecutionStrReplaceResultBlock
    FLines: TArray<string>;
    [JsonNameAttribute('new_start')]
    FNewStart: Int64;
    [JsonNameAttribute('old_lines')]
    FOldLines: Int64;
    [JsonNameAttribute('old_start')]
    FOldStart: Int64;
  public
    property &Type: string read FType write FType;

    property Content: string read FContent write FContent;
    property FileType: string read FFileType write FFileType;
    property NumLines: Int64 read FNumLines write FNumLines;
    property StartLine: Int64 read FStartLine write FStartLine;
    property TotalLines: Int64 read FTotalLines write FTotalLines;

    property IsFileUpdate: Boolean read FIsFileUpdate write FIsFileUpdate;

    property Lines: TArray<string> read FLines write FLines;
    property NewStart: Int64 read FNewStart write FNewStart;
    property OldLines: Int64 read FOldLines write FOldLines;
    property OldStart: Int64 read FOldStart write FOldStart;
  end;

  TTextEditorCodeExecutionToolResultBlock = class(TOptionalContent)
  private
    FContent: TTextEditorCodeExecutionToolResultBlockContent;
  public
    property Content: TTextEditorCodeExecutionToolResultBlockContent read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'ToolSearchToolResultBlock'}

  TToolReferenceBlock = class
  private
    FType: string;
    [JsonNameAttribute('tool_name')]
    FToolName: string;
  public
    property &Type: string read FType write FType;

    property ToolName: string read FToolName write FToolName;
  end;

  TToolSearchToolResultBlockContent = class(TToolResultErrorCodeMessage)
  private
    FType: string;
    [JsonNameAttribute('tool_references')]
    FToolReferences: TArray<TToolReferenceBlock>;
  public
    property &Type: string read FType write FType;

    property ToolReferences: TArray<TToolReferenceBlock> read FToolReferences write FToolReferences;

    destructor Destroy; override;
  end;

  TToolSearchToolResultBlock = class(TOptionalContent)
  private
    FContent: TToolSearchToolResultBlockContent;
  public
    property Content: TToolSearchToolResultBlockContent read FContent write FContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  {$REGION 'MCPToolResultBlock'}

  TMCPToolResultBlockContent = class
  private
    FType: string;
    FCitations: TArray<TTextCitation>;
    FText: string;
  public
    property &Type: string read FType write FType;

    /// <summary>
    /// Citations supporting the text block.
    /// </summary>
    /// <remarks>
    /// The type of citation returned will depend on the type of document being cited. Citing a PDF results
    /// in page_location, plain text results in char_location, and content document results in
    /// content_block_location.
    /// </remarks>
    property Citations: TArray<TTextCitation> read FCitations write FCitations;

    property Text: string read FText write FText;

    destructor Destroy; override;
  end;

  TMCPToolResultBlock = class(TOptionalContent)
  private
    FType: string;
    FContent: TArray<TMCPToolResultBlockContent>;
    FStringContent: string;
  public
    property &Type: string read FType write FType;

    property Content: TArray<TMCPToolResultBlockContent> read FContent write FContent;

    property StringContent: string read FStringContent write FStringContent;

    destructor Destroy; override;
  end;

  {$ENDREGION}

  TToolContent = class
  private
    FType: TContentBlockType;
    FRaw: string;
    FWebSearchToolResultBlock: TWebSearchToolResultBlock;
    FWebFetchToolResultBlock: TWebFetchToolResultBlock;
    FCodeExecutionToolResultBlock: TCodeExecutionToolResultBlock;
    FBashCodeExecutionToolResultBlock: TBashCodeExecutionToolResultBlock;
    FTextEditorCodeExecutionToolResultBlock: TTextEditorCodeExecutionToolResultBlock;
    FToolSearchToolResultBlock: TToolSearchToolResultBlock;
    FMCPToolResultBlock: TMCPToolResultBlock;
    procedure SetRaw(const Value: string);
  public
    property Raw: string read FRaw write SetRaw;

    property WebSearchToolResultBlock: TWebSearchToolResultBlock read FWebSearchToolResultBlock;
    property WebFetchToolResultBlock: TWebFetchToolResultBlock read FWebFetchToolResultBlock write FWebFetchToolResultBlock;
    property CodeExecutionToolResultBlock: TCodeExecutionToolResultBlock read FCodeExecutionToolResultBlock write FCodeExecutionToolResultBlock;
    property BashCodeExecutionToolResultBlock: TBashCodeExecutionToolResultBlock read FBashCodeExecutionToolResultBlock write FBashCodeExecutionToolResultBlock;
    property TextEditorCodeExecutionToolResultBlock: TTextEditorCodeExecutionToolResultBlock read FTextEditorCodeExecutionToolResultBlock write FTextEditorCodeExecutionToolResultBlock;
    property ToolSearchToolResultBlock: TToolSearchToolResultBlock read FToolSearchToolResultBlock write FToolSearchToolResultBlock;
    property MCPToolResultBlock: TMCPToolResultBlock read FMCPToolResultBlock write FMCPToolResultBlock;

    constructor Create;
    destructor Destroy; override;

    class function OverloadBuilder(const RawSource: string; const AType: TContentBlockType): TToolContent;
  end;

implementation

{ TWebSearchToolResultBlock }

destructor TWebSearchToolResultBlock.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TToolContent }

constructor TToolContent.Create;
begin
  inherited Create;
  FWebSearchToolResultBlock := TWebSearchToolResultBlock.Create;

  FWebFetchToolResultBlock := TWebFetchToolResultBlock.Create;
  FCodeExecutionToolResultBlock := TCodeExecutionToolResultBlock.Create;
  FBashCodeExecutionToolResultBlock := TBashCodeExecutionToolResultBlock.Create;
  FTextEditorCodeExecutionToolResultBlock := TTextEditorCodeExecutionToolResultBlock.Create;
  FToolSearchToolResultBlock := TToolSearchToolResultBlock.Create;
  FMCPToolResultBlock := TMCPToolResultBlock.Create;
end;

destructor TToolContent.Destroy;
begin
  FWebSearchToolResultBlock.Free;
  FWebFetchToolResultBlock.Free;
  FCodeExecutionToolResultBlock.Free;
  FBashCodeExecutionToolResultBlock.Free;
  FTextEditorCodeExecutionToolResultBlock.Free;
  FToolSearchToolResultBlock.Free;
  FMCPToolResultBlock.Free;
  inherited;
end;

class function TToolContent.OverloadBuilder(const RawSource: string;
  const AType: TContentBlockType): TToolContent;
begin
  Result := TToolContent.Create;
  Result.FType := AType;
  Result.Raw := RawSource;
end;

procedure TToolContent.SetRaw(const Value: string);
begin
  FRaw := Value;
  case FType of
    TContentBlockType.text: ;
    TContentBlockType.thinking: ;
    TContentBlockType.redacted_thinking: ;
    TContentBlockType.tool_use: ;
    TContentBlockType.server_tool_use: ;

    TContentBlockType.web_search_tool_result:
      begin
        FWebSearchToolResultBlock.Free;
        FWebSearchToolResultBlock := TApiDeserializer.Parse<TWebSearchToolResultBlock>(FRaw);
        FWebSearchToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.web_fetch_tool_result:
      begin
        FWebFetchToolResultBlock.Free;
        FWebFetchToolResultBlock := TApiDeserializer.Parse<TWebFetchToolResultBlock>(FRaw);
        FWebFetchToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.code_execution_tool_result:
      begin
        FCodeExecutionToolResultBlock.Free;
        FCodeExecutionToolResultBlock := TApiDeserializer.Parse<TCodeExecutionToolResultBlock>(FRaw);
        FCodeExecutionToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.bash_code_execution_tool_result:
      begin
        FBashCodeExecutionToolResultBlock.Free;
        FBashCodeExecutionToolResultBlock := TApiDeserializer.Parse<TBashCodeExecutionToolResultBlock>(FRaw);
        FBashCodeExecutionToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.text_editor_code_execution_tool_result:
      begin
        FTextEditorCodeExecutionToolResultBlock.Free;
        FTextEditorCodeExecutionToolResultBlock := TApiDeserializer.Parse<TTextEditorCodeExecutionToolResultBlock>(FRaw);
        FTextEditorCodeExecutionToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.tool_search_tool_result:
      begin
        FToolSearchToolResultBlock.Free;
        FToolSearchToolResultBlock := TApiDeserializer.Parse<TToolSearchToolResultBlock>(FRaw);
        FToolSearchToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.mcp_tool_use: ;

    TContentBlockType.mcp_tool_result:
      begin
        var CurrentJson := TJsonReader.Parse(FRaw);

        {--- when UnionMember0 is a string }
        if CurrentJson.IsStringNode('content') then
          begin
            FMCPToolResultBlock.&Type := 'text';
            FMCPToolResultBlock.StringContent := CurrentJson.AsString('content');
            FMCPToolResultBlock.MarkHasValue;
            Exit;
          end;

        {--- when UnionMember0 is an array of BetaTextBlock }
        FMCPToolResultBlock.Free;
        FMCPToolResultBlock := TApiDeserializer.Parse<TMCPToolResultBlock>(FRaw);
        FMCPToolResultBlock.&Type := 'array';
        FMCPToolResultBlock.MarkHasValue;
      end;

    TContentBlockType.container_upload: ;
  end;
end;

{ TWebFetchToolResultBlockContent }

destructor TWebFetchToolResultBlockContent.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TDocumentBlock }

destructor TDocumentBlock.Destroy;
begin
  if Assigned(FCitations) then
    FCitations.Free;
  if Assigned(FSource) then
    FSource.Free;
  inherited;
end;

{ TWebFetchToolResultBlock }

destructor TWebFetchToolResultBlock.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TCodeExecutionToolResultBlockContent }

destructor TCodeExecutionToolResultBlockContent.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TCodeExecutionToolResultBlock }

destructor TCodeExecutionToolResultBlock.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TBashCodeExecutionToolResultBlockContent }

destructor TBashCodeExecutionToolResultBlockContent.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

{ TBashCodeExecutionToolResultBlock }

destructor TBashCodeExecutionToolResultBlock.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TTextEditorCodeExecutionToolResultBlock }

destructor TTextEditorCodeExecutionToolResultBlock.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TToolSearchToolResultBlockContent }

destructor TToolSearchToolResultBlockContent.Destroy;
begin
  for var Item in FToolReferences do
    Item.Free;
  inherited;
end;

{ TToolSearchToolResultBlock }

destructor TToolSearchToolResultBlock.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

{ TMCPToolResultBlockContent }

destructor TMCPToolResultBlockContent.Destroy;
begin
  for var Item in FCitations do
    Item.Free;
  inherited;
end;

{ TMCPToolResultBlock }

destructor TMCPToolResultBlock.Destroy;
begin
  for var Item in FContent do
    Item.Free;
  inherited;
end;

end.
