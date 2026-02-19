unit Anthropic.Chat.Request;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  Anthropic.API.Params, Anthropic.Types, Anthropic.Exceptions, Anthropic.Functions.Core,
  Anthropic.Schema;

type

{$REGION 'TMessageParam'}

  TCacheControlEphemeral = class(TJSONParam)
    function &Type(const Value: string = 'ephemeral'): TCacheControlEphemeral;
    function Ttl(const Value: string): TCacheControlEphemeral;

    class function New: TCacheControlEphemeral;
  end;

  TContentBlockParam = class abstract(TJSONParam);

    {$REGION 'ContentBlockParam inheritage'}

      TTextCitationParam = class abstract(TJSONParam);

      {$REGION 'TTextCitationParam inheritage'}

        TCitationCharLocationParam = class(TTextCitationParam)
          function &Type(const Value: string = 'char_location'): TCitationCharLocationParam;
          function CitedText(const Value: string): TCitationCharLocationParam;
          function DocumentIndex(const Value: Double): TCitationCharLocationParam;
          function DocumentTitle(const Value: string): TCitationCharLocationParam;
          function EndCharIndex(const Value: Double): TCitationCharLocationParam;
          function StartCharIndex(const Value: Double): TCitationCharLocationParam;

          class function New: TCitationCharLocationParam;
        end;

        TCitationPageLocationParam = class(TTextCitationParam)
          function &Type(const Value: string = 'page_location'): TCitationPageLocationParam;
          function CitedText(const Value: string): TCitationPageLocationParam;
          function DocumentIndex(const Value: Double): TCitationPageLocationParam;
          function DocumentTitle(const Value: string): TCitationPageLocationParam;
          function EndPageNumber(const Value: Double): TCitationPageLocationParam;
          function StartPageNCumber(const Value: Double): TCitationPageLocationParam;

          class function New: TCitationPageLocationParam;
        end;

        TCitationContentBlockLocationParam = class(TTextCitationParam)
          function &Type(const Value: string = 'content_block_location'): TCitationContentBlockLocationParam;
          function CitedText(const Value: string): TCitationContentBlockLocationParam;
          function DocumentIndex(const Value: Double): TCitationContentBlockLocationParam;
          function DocumentTitle(const Value: string): TCitationContentBlockLocationParam;
          function EndBlockIndex(const Value: Double): TCitationContentBlockLocationParam;
          function StartBlockIndex(const Value: Double): TCitationContentBlockLocationParam;

          class function New: TCitationContentBlockLocationParam;
        end;

        TCitationWebSearchResultLocationParam = class(TTextCitationParam)
          function &Type(const Value: string = 'web_search_result_location'): TCitationWebSearchResultLocationParam;
          function CitedText(const Value: string): TCitationWebSearchResultLocationParam;
          function EncryptedIndex(const Value: string): TCitationWebSearchResultLocationParam;
          function Title(const Value: string): TCitationWebSearchResultLocationParam;
          function Url(const Value: string): TCitationWebSearchResultLocationParam;

          class function New: TCitationWebSearchResultLocationParam;
        end;

        TCitationSearchResultLocationParam = class(TTextCitationParam)
          function &Type(const Value: string = 'search_result_location'): TCitationSearchResultLocationParam;
          function CitedText(const Value: string): TCitationSearchResultLocationParam;
          function EndBlockIndex(const Value: Double): TCitationSearchResultLocationParam;
          function SearchResultIndex(const Value: Double): TCitationSearchResultLocationParam;
          function Source(const Value: string): TCitationSearchResultLocationParam;
          function StartBlockIndex(const Value: Double): TCitationSearchResultLocationParam;
          function Title(const Value: string): TCitationSearchResultLocationParam;

          class function New: TCitationSearchResultLocationParam;
        end;

      {$ENDREGION}

    TTextBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'text'): TTextBlockParam;

      function Text(const Value: string): TTextBlockParam;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TTextBlockParam;

      function Citations(const Value: TArray<TTextCitationParam>): TTextBlockParam;

      class function New: TTextBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'ImageBlockParam inheritage'}

      TImageSource = class abstract(TJsonParam);

      {$REGION 'TImageSource inheritage'}

        TBase64ImageSource = class(TImageSource)
          function &Type(const Value: string = 'base64'): TBase64ImageSource;
          function Data(const Value: string): TBase64ImageSource;
          function MediaType(const Value: string): TBase64ImageSource;

          class function New: TBase64ImageSource;
        end;

        TURLImageSource = class(TImageSource)
          function &Type(const Value: string = 'url'): TURLImageSource;
          function Url(const Value: string): TURLImageSource;

          class function New: TURLImageSource;
        end;

        TFileImageSource = class(TImageSource)  //beta
          function &Type(const Value: string = 'file'): TFileImageSource;
          function FileId(const Value: string): TFileImageSource;

          class function New: TFileImageSource;
        end;

      {$ENDREGION}

      TImage = record
        class function Base64Source: TBase64ImageSource; static;
        class function UrlSource: TURLImageSource; static;
        class function FileSource: TFileImageSource; static;
      end;

    TImageBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'image'): TImageBlockParam;
      function Source(const Value: TImageSource): TImageBlockParam; overload;
      function Source(const Base64OrUri: string; const MimeType: string = ''): TImageBlockParam; overload;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TImageBlockParam;

      class function New: TImageBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'DocumentBlockParam inheritage'}

      TDocumentSourceParam = class abstract(TJsonParam);

      {$REGION 'TDocumentSource inheritage'}

        TBase64PDFSource = class(TDocumentSourceParam)
          function &Type(const Value: string = 'base64'): TBase64PDFSource;
          function Data(const Value: string): TBase64PDFSource;
          function MediaType(const Value: string = 'application/pdf'): TBase64PDFSource;

          class function New: TBase64PDFSource;
        end;

        TPlainTextSource = class(TDocumentSourceParam)
          function &Type(const Value: string = 'text'): TPlainTextSource;
          function Data(const Value: string): TPlainTextSource;
          function MediaType(const Value: string = 'text/plain'): TPlainTextSource;

          class function New: TPlainTextSource;
        end;

        {--- Only for TTextBlockParam or ImageBlockParam   }
        TContentBlockSourceContent = class(TContentBlockParam);

        TContentBlockSource = class(TDocumentSourceParam)
          function &Type(const Value: string = 'content'): TContentBlockSource;
          function Content(const Value: TArray<TContentBlockSourceContent>): TContentBlockSource; overload;
          function Content(const Value: string): TContentBlockSource; overload;

          class function New: TContentBlockSource;
        end;

        TURLPDFSource = class(TDocumentSourceParam)
          function &Type(const Value: string = 'url'): TURLPDFSource;
          function Url(const Value: string): TURLPDFSource;

          class function New: TURLPDFSource;
        end;

        TFileDocumentSource = class(TDocumentSourceParam) //beta
          function &Type(const Value: string = 'file'): TFileDocumentSource;
          function FileId(const Value: string): TFileDocumentSource;

          class function New: TFileDocumentSource;
        end;

      {$ENDREGION}

      TCitationsConfigParam = class(TJsonParam)
        function Enabled(const Value: Boolean): TCitationsConfigParam;
      end;

      TDocument = record
        class function Base64PDF: TBase64PDFSource; static;
        class function PlainText: TPlainTextSource; static;
        class function ContentBlock: TContentBlockSource; static;
        class function UrlPdf: TURLPDFSource; static;
        class function FileDocument: TFileDocumentSource; static;
      end;

    TDocumentBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'document'): TDocumentBlockParam;
      function Source(const Value: TDocumentSourceParam): TDocumentBlockParam; overload;
      function Source(const Value: TArray<TContentBlockParam>): TDocumentBlockParam; overload;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TDocumentBlockParam;

      function Citations(const Value: TCitationsConfigParam): TDocumentBlockParam; overload;
      function Citations(const Value: Boolean): TDocumentBlockParam; overload;
      function Context(const Value: string): TDocumentBlockParam;
      function Title(const Value: string): TDocumentBlockParam;

      class function New: TDocumentBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'SearchResultBlockParam inheritage'}

    TSearchResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'search_result'): TSearchResultBlockParam;
      function Content(const Value: TArray<TTextBlockParam>): TSearchResultBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TSearchResultBlockParam;
      function Citations(const Value: TCitationsConfigParam): TSearchResultBlockParam;
      function Source(const Value: string): TSearchResultBlockParam;
      function Title(const Value: string): TSearchResultBlockParam;

      class function New: TSearchResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'ThinkingBlockParam inheritage'}

    TThinkingBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'thinking'): TThinkingBlockParam;
      function Signature(const Value: string): TThinkingBlockParam;
      function Thinking(const Value: string): TThinkingBlockParam;

      class function New: TThinkingBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'RedactedThinkingBlockParam inheritage'}

    TRedactedThinkingBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'redacted_thinking'): TRedactedThinkingBlockParam;
      function Data(const Value: string): TRedactedThinkingBlockParam;

      class function New: TRedactedThinkingBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'ToolUseBlockParam inheritage'}

    TCallerParam = class abstract(TJSONParam);

      {$REGION 'ToolUseBlockParam inheritage'}

      TDirectCaller = class(TJSONParam)
        function &Type(const Value: string = 'direct'): TDirectCaller;

        class function New: TDirectCaller;
      end;

      TServerToolCaller = class(TJSONParam)
        function &Type(const Value: string = 'code_execution_20250825'): TServerToolCaller;
        function ToolId(const Value: string): TServerToolCaller;
      end;

      {$ENDREGION}

    TToolUseBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'tool_use'): TToolUseBlockParam;
      function Id(const Value: string): TToolUseBlockParam;
      function Input(const Value: TJSONObject): TToolUseBlockParam; overload;
      function Input(const Value: string): TToolUseBlockParam; overload;
      function Name(const Value: string): TToolUseBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TToolUseBlockParam;
      function Caller(const Value: TDirectCaller): TToolUseBlockParam; overload;
      function Caller(const Value: TServerToolCaller): TToolUseBlockParam; overload;

      class function New: TToolUseBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolReferenceBlockParam  inheritage'}

    TToolReferenceBlockParam = class(TContentBlockParam)  //beta
      function &Type(const Value: string = 'tool_reference'): TToolReferenceBlockParam;
      function ToolName(const Value: string): TToolReferenceBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TToolReferenceBlockParam;

      class function New: TToolReferenceBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'ToolResultBlockParam  inheritage'}

      TToolResultBlockParam = class(TContentBlockParam)
        function &Type(const Value: string = 'tool_result'): TToolResultBlockParam;
        function CacheControl(const Value: TCacheControlEphemeral): TToolResultBlockParam;
        function Content(const Value: TArray<TContentBlockParam>): TToolResultBlockParam; overload;
        function Content(const Value: string): TToolResultBlockParam; overload;
        function IsError(const Value: Boolean): TToolResultBlockParam;
        function ToolUseId(const Value: string): TToolResultBlockParam;

        class function New: TToolResultBlockParam;
      end;

    {$ENDREGION}

    {$REGION 'ServerToolUseBlockParam  inheritage'}

    TServerToolUseBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'server_tool_use'): TServerToolUseBlockParam;
      function Id(const Value: string): TServerToolUseBlockParam;
      function Input(const Value: TJSONObject): TServerToolUseBlockParam; overload;
      function Input(const Value: string): TServerToolUseBlockParam; overload;
      function Name(const Value: TServerToolUseName): TServerToolUseBlockParam; overload;
      function Name(const Value: string): TServerToolUseBlockParam; overload;
      function CacheControl(const Value: TCacheControlEphemeral): TServerToolUseBlockParam;
      function Caller(const Value: TDirectCaller): TServerToolUseBlockParam; overload;
      function Caller(const Value: TServerToolCaller): TServerToolUseBlockParam; overload;

      class function New: TServerToolUseBlockParam;
    end;

    {$ENDREGION}

    {$REGION 'WebSearchToolResultBlockParam  inheritage'}

    TWebSearchToolResultBlockParamContent = class(TJSONParam);

      {$REGION 'WebSearchToolResultBlockParamContent  inheritage'}

      TWebSearchToolResultBlockItem = class(TWebSearchToolResultBlockParamContent)
        function &Type(const Value: string = 'web_search_result'): TWebSearchToolResultBlockItem;
        function EncryptedContent(const Value: string): TWebSearchToolResultBlockItem;
        function Title(const Value: string): TWebSearchToolResultBlockItem;
        function Url(const Value: string): TWebSearchToolResultBlockItem;
        function PageAge(const Value: string): TWebSearchToolResultBlockItem;

        class function New: TWebSearchToolResultBlockItem;
      end;

      TWebSearchToolRequestError = class(TWebSearchToolResultBlockParamContent)
        function &Type(const Value: string = 'web_search_tool_result_error'): TWebSearchToolRequestError;
        function ErrorCode(const Value: TWebSearchError): TWebSearchToolRequestError; overload;
        function ErrorCode(const Value: string): TWebSearchToolRequestError; overload;

        class function New: TWebSearchToolRequestError;
      end;

      {$ENDREGION}

    TWebSearchToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'web_search_tool_result'): TWebSearchToolResultBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TWebSearchToolResultBlockParam;
      function Content(const Value: TArray<TWebSearchToolResultBlockItem>): TWebSearchToolResultBlockParam; overload;
      function Content(const Value: TWebSearchToolRequestError): TWebSearchToolResultBlockParam; overload;
      function ToolUseId(const Value: string): TWebSearchToolResultBlockParam;

      class function New: TWebSearchToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] WebFetchToolResultBlockParam  inheritage'}

      {$REGION '[beta] WebFetchToolResult'}

      TWebFetchToolResultErrorBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'web_fetch_tool_result_error'): TWebFetchToolResultErrorBlockParam;
        function ErrorCode(const Value: string): TWebFetchToolResultErrorBlockParam; overload;
        function ErrorCode(const Value: TWebSearchError): TWebFetchToolResultErrorBlockParam; overload;

        class function New: TWebFetchToolResultErrorBlockParam;
      end;

      TWebFetchBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'web_fetch_result'): TWebFetchBlockParam;
        function Content(const Value: TDocumentBlockParam): TWebFetchBlockParam;
        function Url(const Value: string): TWebFetchBlockParam;
        function RetrievedAt(const Value: string): TWebFetchBlockParam;

        class function New: TWebFetchBlockParam;
      end;

      {$ENDREGION}

    TWebFetchToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'web_fetch_tool_result'): TWebFetchToolResultBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TWebFetchToolResultBlockParam;
      function Content(const Value: TWebFetchToolResultErrorBlockParam): TWebFetchToolResultBlockParam; overload;
      function Content(const Value: TWebFetchBlockParam): TWebFetchToolResultBlockParam; overload;
      function ToolUseId(const Value: string): TWebFetchToolResultBlockParam;

      class function New: TWebFetchToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] CodeExecutionToolResultBlockParam  inheritage'}

      {$REGION '[beta] CodeExecutionTool'}

      TCodeExecutionToolResultErrorParam = class(TJSONParam)
        function &Type(const Value: string = 'code_execution_tool_result_error'): TCodeExecutionToolResultErrorParam;
        function ErrorCode(const Value: TWebSearchError): TCodeExecutionToolResultErrorParam; overload;
        function ErrorCode(const Value: string): TCodeExecutionToolResultErrorParam; overload;

        class function New: TCodeExecutionToolResultErrorParam;
      end;

      TCodeExecutionOutputBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'code_execution_output'): TCodeExecutionOutputBlockParam;
        function FileId(const Value: string): TCodeExecutionOutputBlockParam;

        class function New: TCodeExecutionOutputBlockParam;
      end;

      TCodeExecutionResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'code_execution_result'): TCodeExecutionResultBlockParam;
        function Content(const Value: TArray<TCodeExecutionOutputBlockParam>): TCodeExecutionResultBlockParam;
        function ReturnCode(const Value: Integer): TCodeExecutionResultBlockParam;
        function Stderr(const Value: string): TCodeExecutionResultBlockParam;
        function Stdout(const Value: string): TCodeExecutionResultBlockParam;

        class function New: TCodeExecutionResultBlockParam;
      end;

      {$ENDREGION}

    TCodeExecutionToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'code_execution_tool_result'): TCodeExecutionToolResultBlockParam;
      function Content(const Value: TCodeExecutionToolResultErrorParam): TCodeExecutionToolResultBlockParam; overload;
      function Content(const Value: TCodeExecutionResultBlockParam): TCodeExecutionToolResultBlockParam; overload;
      function CacheControl(const Value: TCacheControlEphemeral): TCodeExecutionToolResultBlockParam;
      function ToolUseId(const Value: string): TCodeExecutionToolResultBlockParam;

      class function New: TCodeExecutionToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] BashCodeExecutionToolResultBlockParam  inheritage'}

      {$REGION '[beta] BashCodeExecutionToolResultErrorPara'}

      TBashCodeExecutionToolResultErrorPara = class(TJSONParam)
        function &Type(const Value: string = 'bash_code_execution_tool_result_error'): TBashCodeExecutionToolResultErrorPara;
        function ErrorCode(const Value: TWebSearchError): TBashCodeExecutionToolResultErrorPara; overload;
        function ErrorCode(const Value: string): TBashCodeExecutionToolResultErrorPara; overload;

        class function New: TBashCodeExecutionToolResultErrorPara;
      end;

      TBashCodeExecutionOutputBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'bash_code_execution_output'): TBashCodeExecutionOutputBlockParam;
        function FileId(const Value: string): TBashCodeExecutionOutputBlockParam;

        class function New: TBashCodeExecutionOutputBlockParam;
      end;

      TBashCodeExecutionResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'bash_code_execution_result'): TBashCodeExecutionResultBlockParam;
        function Content(const Value: TArray<TBashCodeExecutionOutputBlockParam>): TBashCodeExecutionResultBlockParam;
        function ReturnCode(const Value: Integer): TBashCodeExecutionResultBlockParam;
        function Stderr(const Value: string): TBashCodeExecutionResultBlockParam;
        function Stdout(const Value: string): TBashCodeExecutionResultBlockParam;

        class function New: TBashCodeExecutionResultBlockParam;
      end;

      {$ENDREGION}

    TBashCodeExecutionToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'bash_code_execution_tool_result'): TBashCodeExecutionToolResultBlockParam;
      function Content(const Value: TBashCodeExecutionToolResultErrorPara): TBashCodeExecutionToolResultBlockParam; overload;
      function Content(const Value: TBashCodeExecutionResultBlockParam): TBashCodeExecutionToolResultBlockParam; overload;
      function CacheControl(const Value: TCacheControlEphemeral): TBashCodeExecutionToolResultBlockParam;
      function ToolUseId(const Value: string): TBashCodeExecutionToolResultBlockParam;

      class function New: TBashCodeExecutionToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] TextEditorCodeExecutionToolResultBlockParam  inheritage'}

      {$REGION '[beta] TextEditorCodeExecutionToolResul'}

      TTextEditorCodeExecutionToolResultErrorParam = class(TJSONParam)
        function &Type(const Value: string = 'text_editor_code_execution_tool_result_error'): TTextEditorCodeExecutionToolResultErrorParam;
        function ErrorCode(const Value: TWebSearchError): TTextEditorCodeExecutionToolResultErrorParam; overload;
        function ErrorCode(const Value: string): TTextEditorCodeExecutionToolResultErrorParam; overload;
        function ErrorMessage(const Value: string): TTextEditorCodeExecutionToolResultErrorParam;

        class function New: TTextEditorCodeExecutionToolResultErrorParam;
      end;

      TTextEditorCodeExecutionViewResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'text_editor_code_execution_view_result'): TTextEditorCodeExecutionViewResultBlockParam;
        function Content(const Value: string): TTextEditorCodeExecutionViewResultBlockParam;
        function FileType(const Value: string): TTextEditorCodeExecutionViewResultBlockParam; overload;
        function FileType(const Value: TFileType): TTextEditorCodeExecutionViewResultBlockParam; overload;
        function NumLines(const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;
        function StartLine(const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;
        function TotalLines(const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;

        class function New: TTextEditorCodeExecutionViewResultBlockParam;
      end;

      TTextEditorCodeExecutionCreateResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'text_editor_code_execution_create_result'): TTextEditorCodeExecutionCreateResultBlockParam;
        function IsFileUpdate(const Value: Boolean): TTextEditorCodeExecutionCreateResultBlockParam;

        class function New: TTextEditorCodeExecutionCreateResultBlockParam;
      end;

      TTextEditorCodeExecutionStrReplaceResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'text_editor_code_execution_str_replace_result'): TTextEditorCodeExecutionStrReplaceResultBlockParam;
        function Lines(const Value: TArray<Integer>): TTextEditorCodeExecutionStrReplaceResultBlockParam;
        function NewLines(const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
        function NewStart(const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
        function OldLines(const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
        function OldStart(const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;

        class function New: TTextEditorCodeExecutionStrReplaceResultBlockParam;
      end;

      {$ENDREGION}

    TTextEditorCodeExecutionToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'text_editor_code_execution_tool_result'): TTextEditorCodeExecutionToolResultBlockParam;
      function Content(const Value: TTextEditorCodeExecutionToolResultErrorParam): TTextEditorCodeExecutionToolResultBlockParam; overload;
      function Content(const Value: TTextEditorCodeExecutionViewResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam; overload;
      function Content(const Value: TTextEditorCodeExecutionCreateResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam; overload;
      function Content(const Value: TTextEditorCodeExecutionStrReplaceResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam; overload;
      function CacheControl(const Value: TCacheControlEphemeral): TTextEditorCodeExecutionToolResultBlockParam;
      function ToolUseId(const Value: string): TTextEditorCodeExecutionToolResultBlockParam;

      class function New: TTextEditorCodeExecutionToolResultBlockParam;
    end;


    {$ENDREGION}

    {$REGION '[beta] ToolSearchToolResultBlockParam  inheritage'}

      {$REGION '[beta] ToolSearchToolResult'}

      TToolSearchToolResultErrorParam = class(TJSONParam)
        function &Type(const Value: string = 'tool_search_tool_result_error'): TToolSearchToolResultErrorParam;
        function ErrorCode(const Value: string): TToolSearchToolResultErrorParam; overload;
        function ErrorCode(const Value: TWebSearchError): TToolSearchToolResultErrorParam; overload;

        class function New: TToolSearchToolResultErrorParam;
      end;

      TToolSearchToolSearchResultBlockParam = class(TJSONParam)
        function &Type(const Value: string = 'tool_search_tool_search_result'): TToolSearchToolSearchResultBlockParam;
        function ToolReferences(const Value: TArray<TToolReferenceBlockParam>): TToolSearchToolSearchResultBlockParam;

        class function New: TToolSearchToolSearchResultBlockParam;
      end;

      {$ENDREGION}

    TToolSearchToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'tool_search_tool_result'): TToolSearchToolResultBlockParam;
      function Content(const Value: TToolSearchToolResultErrorParam): TToolSearchToolResultBlockParam; overload;
      function Content(const Value: TToolSearchToolSearchResultBlockParam): TToolSearchToolResultBlockParam; overload;
      function CacheControl(const Value: TCacheControlEphemeral): TToolSearchToolResultBlockParam;
      function ToolUseId(const Value: string): TToolSearchToolResultBlockParam;

      class function New: TToolSearchToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] MCPToolUseBlockParam  inheritage'}

    TMCPToolUseBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'mcp_tool_use'): TMCPToolUseBlockParam;
      function Id(const Value: string): TMCPToolUseBlockParam;
      function Input(const Value: TJSONObject): TMCPToolUseBlockParam; overload;
      function Input(const Value: string): TMCPToolUseBlockParam; overload;
      function Name(const Value: string): TMCPToolUseBlockParam;

      /// <summary>
      /// The name of the MCP server
      /// </summary>
      function ServerName(const Value: string): TMCPToolUseBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TMCPToolUseBlockParam;

      class function New: TMCPToolUseBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] RequestMCPToolResultBlockParam  inheritage'}

    TMCPToolResultBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'mcp_tool_result'): TMCPToolResultBlockParam;
      function Content(const Value: string): TMCPToolResultBlockParam; overload;
      function Content(const Value: TArray<TTextBlockParam>): TMCPToolResultBlockParam; overload;
      function ToolUseId(const Value: string): TMCPToolResultBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TMCPToolResultBlockParam;
      function IsError(const Value: Boolean): TMCPToolResultBlockParam;

      class function New: TMCPToolResultBlockParam;
    end;

    {$ENDREGION}

    {$REGION '[beta] ContainerUploadBlockParam  inheritage'}

    TContainerUploadBlockParam = class(TContentBlockParam)
      function &Type(const Value: string = 'container_upload'): TContainerUploadBlockParam;
      function FileId(const Value: string): TContainerUploadBlockParam;
      function CacheControl(const Value: TCacheControlEphemeral): TContainerUploadBlockParam;

      class function New: TContainerUploadBlockParam;
    end;

    {$ENDREGION}

  TMessageParam = class(TJSONParam)
    function Content(const Value: TArray<TContentBlockParam>): TMessageParam; overload;
    function Content(const Value: string): TMessageParam; overload;
    function Role(const Value: TMessageRole): TMessageParam; overload;
    function Role(const Value: string): TMessageParam; overload;

    class function New: TMessageParam;
  end;

{$ENDREGION}

{$REGION 'ThinkingConfigParam'}

  TThinkingConfigParam = class(TJSONParam)
    function &Type(const Value: TThinkingType): TThinkingConfigParam; overload;
    function &Type(const Value: string): TThinkingConfigParam; overload;

    /// <summary>
    /// Determines how many tokens Claude can use for its internal reasoning process. Larger budgets
    /// can enable more thorough analysis for complex problems, improving response quality.
    /// </summary>
    /// <remarks>
    /// Must be more than 1024 and less than max_tokens.
    /// </remarks>
    function BudgetTokens(const Value: Integer): TThinkingConfigParam;

    class function New(const State: TThinkingType): TThinkingConfigParam; overload;
    class function New(const Value: string): TThinkingConfigParam; overload;
  end;

{$ENDREGION}

{$REGION 'ToolChoice'}

  TToolChoice = class(TJSONParam)
    function &Type(const Value: TToolChoiceType): TToolChoice; overload;
    function &Type(const Value: string): TToolChoice; overload;

    /// <summary>
    /// Whether to disable parallel tool use.
    /// </summary>
    /// <remarks>
    /// Defaults to false. If set to true, the model will output at most one tool use.
    /// </remarks>
    function DisableParallelToolUse(const Value: Boolean): TToolChoice;

    /// <summary>
    /// The name of the tool to use when type = "tool"
    /// </summary>
    function Name(const Value: string): TToolChoice;

    class function New(const Value: TToolChoiceType): TToolChoice; overload;
    class function New(const Value: string): TToolChoice; overload;
  end;

{$ENDREGION}

{$REGION 'ToolUnion'}

  TToolUnion = class abstract(TJSONParam);

  {$REGION 'Tool'}

    TInputSchema = class(TJSONParam)
      function &Type(const Value: string = 'object'): TInputSchema;
      function Properties(const Value: TJSONObject): TInputSchema; overload;
      function Properties(const Value: string): TInputSchema; overload;

      class function New: TInputSchema;
    end;

    TTool = class(TToolUnion)
      function &Type(const Value: string = 'custom'): TTool;

      function FunctionPlugin(const Value: IFunctionCore): TTool;

      /// <summary>
      /// JSON schema for this tool's input.
      /// </summary>
      /// <remarks>
      /// This defines the shape of the input that your tool accepts and that the model will produce.
      /// </remarks>
      function InputSchema(const Value: TSchemaParams): TTool; overload;

      /// <summary>
      /// JSON schema for this tool's input.
      /// </summary>
      /// <remarks>
      /// This defines the shape of the input that your tool accepts and that the model will produce.
      /// </remarks>
      function InputSchema(const Value: string): TTool; overload;

      /// <summary>
      /// JSON schema for this tool's input.
      /// </summary>
      /// <remarks>
      /// This defines the shape of the input that your tool accepts and that the model will produce.
      /// </remarks>
      function InputSchema(const Value: TInputSchema): TTool; overload;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// <para>
      /// • This is how the tool will be called by the model and in tool_use blocks.
      /// </para>
      /// <para>
      /// • maxLength 128, minLength 1
      /// </para>
      /// </remarks>
      function Name(const Value: string): TTool;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TTool;

      /// <summary>
      /// Description of what this tool does.
      /// </summary>
      /// <remarks>
      /// Tool descriptions should be as detailed as possible. The more information that the model has
      /// about what the tool is and how to use it, the better it will perform. You can use natural
      /// language descriptions to reinforce important aspects of the tool input JSON schema.
      /// </remarks>
      function Description(const Value: string): TTool;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TTool; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TTool; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TTool;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TTool;

      {$ENDREGION}

      class function New: TTool;
    end;

  {$ENDREGION}

  {$REGION 'ToolBash20250124'}

    TToolBash20250124 = class(TToolUnion)
      function &Type(const Value: string = 'bash_20250124'): TToolBash20250124;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'bash'): TToolBash20250124;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolBash20250124;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolBash20250124; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolBash20250124; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolBash20250124;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolBash20250124;

      {$ENDREGION}

      class function New: TToolBash20250124;
    end;

  {$ENDREGION}

  {$REGION 'ToolTextEditor20250124'}

    TToolTextEditor20250124 = class(TToolUnion)
      function &Type(const Value: string = 'bash_20250124'): TToolTextEditor20250124;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'str_replace_editor'): TToolTextEditor20250124;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolTextEditor20250124;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolTextEditor20250124; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolTextEditor20250124; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolTextEditor20250124;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolTextEditor20250124;

      {$ENDREGION}

      class function New: TToolTextEditor20250124;
    end;

  {$ENDREGION}

  {$REGION 'ToolTextEditor20250429'}

    TToolTextEditor20250429 = class(TToolUnion)
      function &Type(const Value: string = 'bash_20250124'): TToolTextEditor20250429;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'str_replace_based_edit_tool'): TToolTextEditor20250429;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolTextEditor20250429;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolTextEditor20250429; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolTextEditor20250429; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolTextEditor20250429;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolTextEditor20250429;

      {$ENDREGION}

      class function New: TToolTextEditor20250429;
    end;

  {$ENDREGION}

  {$REGION 'ToolTextEditor20250728'}

    TToolTextEditor20250728 = class(TToolUnion)
      function &Type(const Value: string = 'text_editor_20250728'): TToolTextEditor20250728;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'str_replace_based_edit_tool'): TToolTextEditor20250728;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolTextEditor20250728;

      /// <summary>
      /// Maximum number of characters to display when viewing a file. If not specified, defaults to
      /// displaying the full file.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function MaxCharacters(const Value: Integer): TToolTextEditor20250728;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolTextEditor20250728; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolTextEditor20250728; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolTextEditor20250728;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolTextEditor20250728;

      {$ENDREGION}

      class function New: TToolTextEditor20250728;
    end;

  {$ENDREGION}

  {$REGION 'WebSearchTool20250305'}

    TUserLocation = class(TJSONParam)
      function &Type(const Value: string = 'approximate'): TUserLocation;

      /// <summary>
      /// The city of the user.
      /// </summary>
      /// <remarks>
      /// maxLength 255, minLength 1
      /// </remarks>
      function City(const Value: string): TUserLocation;

      /// <summary>
      /// The two letter ISO country code of the user.
      /// </summary>
      /// <remarks>
      /// maxLength 2, minLength 2
      /// </remarks>
      function Country(const Value: string): TUserLocation;

      /// <summary>
      /// The region of the user.
      /// </summary>
      /// <remarks>
      /// maxLength 255, minLength 1
      /// </remarks>
      function Region(const Value: string): TUserLocation;

      /// <summary>
      /// The IANA timezone of the user.
      /// </summary>
      /// <remarks>
      /// maxLength 255, minLength 1
      /// </remarks>
      function Timezone(const Value: string): TUserLocation;

      class function New: TUserLocation;
    end;

    TWebSearchTool20250305 = class(TToolUnion)
      function &Type(const Value: string = 'web_search_20250305'): TWebSearchTool20250305;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'web_search'): TWebSearchTool20250305;

      /// <summary>
      /// If provided, only these domains will be included in results. Cannot be used alongside blocked_domains.
      /// </summary>
      function AllowedDomains(const Value: TArray<string>): TWebSearchTool20250305;

      /// <summary>
      /// If provided, these domains will never appear in results. Cannot be used alongside allowed_domains.
      /// </summary>
      function BlockedDomains(const Value: TArray<string>): TWebSearchTool20250305;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TWebSearchTool20250305;

      /// <summary>
      /// Maximum number of times the tool can be used in the API request.
      /// </summary>
      /// <remarks>
      /// exclusiveMinimum 0
      /// </remarks>
      function MaxUses(const Value: Integer): TWebSearchTool20250305;

      /// <summary>
      /// Parameters for the user's location. Used to provide more relevant search results.
      /// </summary>
      function UserLocation(const Value: TUserLocation): TWebSearchTool20250305;

      {$REGION '[beta]'}

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TWebSearchTool20250305; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TWebSearchTool20250305; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TWebSearchTool20250305;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TWebSearchTool20250305;

      {$ENDREGION}

      class function New: TWebSearchTool20250305;
    end;

  {$ENDREGION}

  {$REGION '[beta]'}

    {$REGION '[beta] ToolBash20241022'}

    TToolBash20241022 = class(TToolUnion)
      function &Type(const Value: string = 'bash_20241022'): TToolBash20241022;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'bash'): TToolBash20241022;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolBash20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolBash20241022; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolBash20241022; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolBash20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolBash20241022;

      class function New: TToolBash20241022;
    end;

    {$ENDREGION}

    {$REGION '[beta] CodeExecutionTool20250522'}

    TCodeExecutionTool20250522 = class(TToolUnion)
      function &Type(const Value: string = 'code_execution_20250522'): TCodeExecutionTool20250522;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'code_execution'): TCodeExecutionTool20250522;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TCodeExecutionTool20250522;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TCodeExecutionTool20250522; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TCodeExecutionTool20250522; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TCodeExecutionTool20250522;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TCodeExecutionTool20250522;

      class function New: TCodeExecutionTool20250522;
    end;

    {$ENDREGION}

    {$REGION '[beta] CodeExecutionTool20250825'}

    TCodeExecutionTool20250825 = class(TToolUnion)
      function &Type(const Value: string = 'code_execution_20250825'): TCodeExecutionTool20250825;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'code_execution'): TCodeExecutionTool20250825;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TCodeExecutionTool20250825;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TCodeExecutionTool20250825; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TCodeExecutionTool20250825; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TCodeExecutionTool20250825;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TCodeExecutionTool20250825;

      class function New: TCodeExecutionTool20250825;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolComputerUse20241022'}

    TToolComputerUse20241022 = class(TToolUnion)
      function &Type(const Value: string = 'computer_20241022'): TToolComputerUse20241022;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'computer'): TToolComputerUse20241022;

      /// <summary>
      /// The height of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayHeightPx(const Value: Integer): TToolComputerUse20241022;

      /// <summary>
      /// The width of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayWidthPx(const Value: Integer): TToolComputerUse20241022;

      /// <summary>
      /// The X11 display number (e.g. 0, 1) for the display.
      /// </summary>
      /// <remarks>
      /// minimum 0
      /// </remarks>
      function DisplayNumber(const Value: Integer): TToolComputerUse20241022;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolComputerUse20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolComputerUse20241022; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolComputerUse20241022; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolComputerUse20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolComputerUse20241022;

      class function New: TToolComputerUse20241022;
    end;

    {$ENDREGION}

    {$REGION '[beta] MemoryTool20250818'}

    TMemoryTool20250818 = class(TToolUnion)
      function &Type(const Value: string = 'memory_20250818'): TMemoryTool20250818;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'memory'): TMemoryTool20250818;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TMemoryTool20250818;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TMemoryTool20250818; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TMemoryTool20250818; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TMemoryTool20250818;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TMemoryTool20250818;

      class function New: TMemoryTool20250818;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolComputerUse20250124'}

    TToolComputerUse20250124 = class(TToolUnion)
      function &Type(const Value: string = 'computer_20250124'): TToolComputerUse20250124;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'computer'): TToolComputerUse20250124;

      /// <summary>
      /// The height of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayHeightPx(const Value: Integer): TToolComputerUse20250124;

      /// <summary>
      /// The width of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayWidthPx(const Value: Integer): TToolComputerUse20250124;

      /// <summary>
      /// The X11 display number (e.g. 0, 1) for the display.
      /// </summary>
      /// <remarks>
      /// minimum 0
      /// </remarks>
      function DisplayNumber(const Value: Integer): TToolComputerUse20250124;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolComputerUse20250124;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolComputerUse20250124; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolComputerUse20250124; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolComputerUse20250124;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolComputerUse20250124;

      class function New: TToolComputerUse20250124;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolTextEditor20241022'}

    TToolTextEditor20241022 = class(TToolUnion)
      function &Type(const Value: string = 'text_editor_20241022'): TToolTextEditor20241022;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'str_replace_editor'): TToolTextEditor20241022;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolTextEditor20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolTextEditor20241022; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolTextEditor20241022; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolTextEditor20241022;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolTextEditor20241022;

      class function New: TToolTextEditor20241022;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolComputerUse20251124'}

    TToolComputerUse20251124 = class(TToolUnion)
      function &Type(const Value: string = 'computer_20251124'): TToolComputerUse20251124;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'computer'): TToolComputerUse20251124;

      /// <summary>
      /// The height of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayHeightPx(const Value: Integer): TToolComputerUse20251124;

      /// <summary>
      /// The width of the display in pixels.
      /// </summary>
      /// <remarks>
      /// minimum 1
      /// </remarks>
      function DisplayWidthPx(const Value: Integer): TToolComputerUse20251124;

      /// <summary>
      /// The X11 display number (e.g. 0, 1) for the display.
      /// </summary>
      /// <remarks>
      /// minimum 0
      /// </remarks>
      function DisplayNumber(const Value: Integer): TToolComputerUse20251124;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolComputerUse20251124;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolComputerUse20251124; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolComputerUse20251124; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolComputerUse20251124;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolComputerUse20251124;

      class function New: TToolComputerUse20251124;
    end;

    {$ENDREGION}

    {$REGION '[beta] WebFetchTool20250910'}

    TWebFetchTool20250910 = class(TToolUnion)
      function &Type(const Value: string = 'web_fetch_20250910'): TWebFetchTool20250910;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'web_fetch'): TWebFetchTool20250910;

      /// <summary>
      /// If provided, only these domains will be included in results. Cannot be used alongside blocked_domains.
      /// </summary>
      function AllowedDomains(const Value: TArray<string>): TWebFetchTool20250910;

      /// <summary>
      /// If provided, these domains will never appear in results. Cannot be used alongside allowed_domains.
      /// </summary>
      function BlockedDomains(const Value: TArray<string>): TWebFetchTool20250910;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TWebFetchTool20250910;

      /// <summary>
      /// Maximum number of times the tool can be used in the API request.
      /// </summary>
      /// <remarks>
      /// exclusiveMinimum 0
      /// </remarks>
      function MaxUses(const Value: Integer): TWebFetchTool20250910;

      /// <summary>
      /// Parameters for the user's location. Used to provide more relevant search results.
      /// </summary>
      function UserLocation(const Value: TUserLocation): TWebFetchTool20250910;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TWebFetchTool20250910; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TWebFetchTool20250910; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TWebFetchTool20250910;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TWebFetchTool20250910;

      class function New: TWebFetchTool20250910;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolSearchToolBm25_20251119'}

    TToolSearchToolBm25_20251119 = class(TToolUnion)
      function &Type(const Value: string = 'tool_search_tool_bm25'): TToolSearchToolBm25_20251119;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'tool_search_tool_bm25'): TToolSearchToolBm25_20251119;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolSearchToolBm25_20251119;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolSearchToolBm25_20251119; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolSearchToolBm25_20251119; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolSearchToolBm25_20251119;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolSearchToolBm25_20251119;

      class function New: TToolSearchToolBm25_20251119;
    end;

    {$ENDREGION}

    {$REGION '[beta] ToolSearchToolRegex20251119'}

    TToolSearchToolRegex20251119 = class(TToolUnion)
      function &Type(const Value: string = 'tool_search_tool_regex_20251119'): TToolSearchToolRegex20251119;

      /// <summary>
      /// Name of the tool.
      /// </summary>
      /// <remarks>
      /// This is how the tool will be called by the model and in tool_use blocks.
      /// </remarks>
      function Name(const Value: string = 'tool_search_tool_regex'): TToolSearchToolRegex20251119;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TToolSearchToolRegex20251119;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<TAllowedCallersType>): TToolSearchToolRegex20251119; overload;

      /// <summary>
      /// [beta]
      /// </summary>
      function AllowedCallers(const Value: TArray<string>): TToolSearchToolRegex20251119; overload;

      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TToolSearchToolRegex20251119;

      /// <summary>
      /// [beta]
      /// </summary>
      function Strict(const Value: Boolean): TToolSearchToolRegex20251119;

      class function New: TToolSearchToolRegex20251119;
    end;

    {$ENDREGION}

    {$REGION '[beta] MCPToolset'}

    TMCPconfigs = class(TJSONParam)
      /// <summary>
      /// [beta] If true, tool will not be included in initial system prompt. Only loaded when returned
      /// via tool_reference from tool search.
      /// </summary>
      function DeferLoading(const Value: Boolean): TMCPconfigs;

      function Enabled(const Value: Boolean): TMCPconfigs;
    end;

    TMCPToolset = class(TToolUnion)
      function &Type(const Value: string = 'mcp_toolset'): TMCPToolset;

      /// <summary>
      /// Name of the MCP server to configure tools for
      /// </summary>
      /// <remarks>
      /// maxLength 255, minLength 1
      /// </remarks>
      function McpServerName(const Value: string): TMCPToolset;

      /// <summary>
      /// Create a cache control breakpoint at this content block.
      /// </summary>
      function CacheControl(const Value: TCacheControlEphemeral): TMCPToolset;

      /// <summary>
      /// Configuration overrides for specific tools, keyed by tool name
      /// </summary>
      function Configs(const Value: TMCPconfigs): TMCPToolset;

      /// <summary>
      /// Default configuration applied to all tools from this server
      /// </summary>
      function DefaultConfig(const Value: TMCPconfigs): TMCPToolset;

      class function New: TMCPToolset;
    end;

    {$ENDREGION}

  {$ENDREGION}

{$ENDREGION}

{$REGION '[beta] ToolUnion'}

  {$REGION '[beta] SkillParams'}

  TSkillParams = class(TJSONParam)
    function &Type(const Value: TSkillType): TSkillParams; overload;
    function &Type(const Value: string): TSkillParams; overload;

    /// <summary>
    /// Skill ID
    /// </summary>
    /// <remarks>
    /// maxLength 64, minLength 1
    /// </remarks>
    function SkillId(const Value: string): TSkillParams;

    /// <summary>
    /// Skill version or 'latest' for most recent version
    /// </summary>
    /// <remarks>
    /// maxLength 64, minLength 1
    /// </remarks>
    function Version(const Value: string): TSkillParams;

    class function New(const SkillType: TSkillType): TSkillParams; overload;
    class function New(const SkillType: string): TSkillParams; overload;
  end;

  {$ENDREGION}

  {$REGION '[beta] ContainerParams'}

  TContainerParams = class(TJSONParam)
    /// <summary>
    /// Container id
    /// </summary>
    function Id(const Value: string): TContainerParams;

    /// <summary>
    /// List of skills to load in the container
    /// </summary>
    function Skills(const Value: TArray<TSkillParams>): TContainerParams;

    class function New: TContainerParams;
  end;

  {$ENDREGION}

  {$REGION '[beta] ContextManagementConfig'}

    TEditsParams = class abstract(TJSONParam);

    {$REGION '[beta] TEditsParams'}

      {$REGION '[beta] ClearToolUses20250919Edit'}

        {$REGION '[beta] sub ClearToolUses20250919Edit'}

        TInputTokensClearAtLeast = class(TJSONParam)
          function &Type(const Value: string = 'input_tokens'): TInputTokensClearAtLeast;
          function Value(const Value: Integer): TInputTokensClearAtLeast;

          class function New: TInputTokensClearAtLeast;
        end;

        TToolUsesKeep = class(TJSONParam)
          function &Type(const Value: string = 'tool_uses'): TToolUsesKeep;
          function Value(const Value: Integer): TToolUsesKeep;

          class function New: TToolUsesKeep;
        end;

        TInputTokensTrigger = class(TJSONParam)
          function &Type(const Value: string = 'input_tokens'): TInputTokensTrigger;
          function Value(const Value: Integer): TInputTokensTrigger;

          class function New: TInputTokensTrigger;
        end;

        TToolUsesTrigger = class(TJSONParam)
          function &Type(const Value: string = 'tool_uses'): TToolUsesTrigger;
          function Value(const Value: Integer): TToolUsesTrigger;

          class function New: TToolUsesTrigger;
        end;

        {$ENDREGION}

      TClearToolUses20250919Edit = class(TEditsParams)
        function &Type(const Value: string = 'clear_tool_uses_20250919'): TClearToolUses20250919Edit;

        /// <summary>
        /// Minimum number of tokens that must be cleared when triggered. Context will only be modified
        /// if at least this many tokens can be removed.
        /// </summary>
        function ClearAtLeast(const Value: TInputTokensClearAtLeast): TClearToolUses20250919Edit;

        /// <summary>
        /// Whether to clear all tool inputs (bool) or specific tool inputs to clear (list)
        /// </summary>
        function ClearToolInputs(const Value: Boolean): TClearToolUses20250919Edit; overload;

        /// <summary>
        /// Whether to clear all tool inputs (bool) or specific tool inputs to clear (list)
        /// </summary>
        function ClearToolInputs(const Value: TArray<string>): TClearToolUses20250919Edit; overload;

        /// <summary>
        /// Tool names whose uses are preserved from clearing
        /// </summary>
        function ExcludeTools(const Value: TArray<string>): TClearToolUses20250919Edit; overload;

        /// <summary>
        /// Number of tool uses to retain in the conversation
        /// </summary>
        function Keep(const Value: TToolUsesKeep): TClearToolUses20250919Edit;

        /// <summary>
        /// Condition that triggers the context management strategy
        /// </summary>
        function Trigger(const Value: TInputTokensTrigger): TClearToolUses20250919Edit; overload;

        /// <summary>
        /// Condition that triggers the context management strategy
        /// </summary>
        function Trigger(const Value: TToolUsesTrigger): TClearToolUses20250919Edit; overload;


        class function New: TClearToolUses20250919Edit;
      end;

      {$ENDREGION}

      {$REGION '[beta] ClearThinking20251015Edit'}

        {$REGION '[beta] sub ClearThinking20251015Edit'}

        TThinkingTurns = class(TJSONParam)
          function &Type(const Value: string = 'thinking_turns'): TThinkingTurns;
          function Value(const Value: Integer): TThinkingTurns;

          class function New: TThinkingTurns;
        end;

        TAllThinkingTurns = class(TJSONParam)
          function &Type(const Value: string = 'all'): TAllThinkingTurns;

          class function New: TAllThinkingTurns;
        end;

        {$ENDREGION}

      TClearThinking20251015Edit = class(TEditsParams)
        function &Type(const Value: string = 'clear_thinking_20251015'): TClearThinking20251015Edit;

        /// <summary>
        /// Number of most recent assistant turns to keep thinking blocks for. Older turns will have their
        /// thinking blocks removed.
        /// </summary>
        function Keep(const Value: TThinkingTurns): TClearThinking20251015Edit; overload;

        /// <summary>
        /// Number of most recent assistant turns to keep thinking blocks for. Older turns will have their
        /// thinking blocks removed.
        /// </summary>
        function Keep(const Value: TAllThinkingTurns): TClearThinking20251015Edit; overload;

        /// <summary>
        /// Number of most recent assistant turns to keep thinking blocks for. Older turns will have their
        /// thinking blocks removed.
        /// </summary>
        function Keep(const Value: string = 'all'): TClearThinking20251015Edit; overload;

        class function New: TClearThinking20251015Edit;
      end;

      {$ENDREGION}

    {$ENDREGION}

  TContextManagementConfig = class(TJSONParam)
    /// <summary>
    /// List of context management edits to apply
    /// </summary>
    function Edits(const Value: TArray<TEditsParams>): TContextManagementConfig;
  end;

  {$ENDREGION}

  {$REGION '[beta] RequestMCPServerURLDefinition'}

    {$REGION '[beta] RequestMCPServerToolConfiguration'}

    TRequestMCPServerToolConfiguration = class(TJSONParam)
      function AllowedTools(const Value: TArray<string>): TRequestMCPServerToolConfiguration;
      function Enabled(const Value: Boolean): TRequestMCPServerToolConfiguration;

      class function New: TRequestMCPServerToolConfiguration;
    end;

    {$ENDREGION}

  TRequestMCPServerURLDefinition = class(TJSONParam)
    function &Type(const Value: string = 'url'): TRequestMCPServerURLDefinition;
    function Name(const Value: string): TRequestMCPServerURLDefinition;
    function Url(const Value: string): TRequestMCPServerURLDefinition;
    function AuthorizationToken(const Value: string): TRequestMCPServerURLDefinition;
    function ToolConfiguration(const Value: TRequestMCPServerToolConfiguration): TRequestMCPServerURLDefinition;

    class function New: TRequestMCPServerURLDefinition;
  end;


  {$ENDREGION}

  {$REGION '[beta] OutputConfig'}

  TOutputConfigFormat = class(TJSONParam)
    function &Type(const Value: string = 'json_schema'): TOutputConfigFormat;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: TJSONObject): TOutputConfigFormat; overload;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: TSchemaParams): TOutputConfigFormat; overload;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: string): TOutputConfigFormat; overload;

    class function New: TOutputConfigFormat;
  end;

  TOutputConfig = class(TJSONParam)
    /// <summary>
    /// How much effort the model should put into its response. Higher effort levels may result in more
    /// thorough analysis but take longer.
    /// </summary>
    /// <param name="Value">
    /// Valid values are low, medium, or high.
    /// </param>
    function Effort(const Value: TEffortType): TOutputConfig; overload;

    /// <summary>
    /// How much effort the model should put into its response. Higher effort levels may result in more
    /// thorough analysis but take longer.
    /// </summary>
    /// <param name="Value">
    /// Valid values are low, medium, or high.
    /// </param>
    function Effort(const Value: string): TOutputConfig; overload;

    /// <summary>
    /// A schema to specify Claude's output format in responses.
    /// </summary>
    function Format(const Value: TOutputConfigFormat): TOutputConfig; overload;

    /// <summary>
    /// A schema to specify Claude's output format in responses.
    /// </summary>
    function Format(const Value: string): TOutputConfig; overload;

    class function New: TOutputConfig;
  end;

  {$ENDREGION}

  {$REGION '[beta] JSONOutputFormat'}

  TJSONOutputFormat = class(TJSONParam)
    function &Type(const Value: string = 'json_schema'): TJSONOutputFormat;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: TSchemaParams): TJSONOutputFormat; overload;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: TJSONObject): TJSONOutputFormat; overload;

    /// <summary>
    /// The JSON schema of the format
    /// </summary>
    function Schema(const Value: string): TJSONOutputFormat; overload;

    class function New: TJSONOutputFormat;
  end;

  {$ENDREGION}

{$ENDREGION}

  TChatParams = class(TJSONParam)
    /// <summary>
    /// Optional header to specify the beta version(s) you want to use.
    /// </summary>
    /// <remarks>
    /// Accepts one of the following:
    /// <para>
    /// • "message-batches-2024-09-24"
    /// </para>
    /// <para>
    /// • "prompt-caching-2024-07-31"
    /// </para>
    /// <para>
    /// • "computer-use-2024-10-22"
    /// </para>
    /// <para>
    /// • "computer-use-2025-01-24"
    /// </para>
    /// <para>
    /// • "pdfs-2024-09-25"
    /// </para>
    /// <para>
    /// • "token-counting-2024-11-01"
    /// </para>
    /// <para>
    /// • "token-efficient-tools-2025-02-19"
    /// </para>
    /// <para>
    /// • "output-128k-2025-02-19"
    /// </para>
    /// <para>
    /// • "files-api-2025-04-14"
    /// </para>
    /// <para>
    /// • "mcp-client-2025-04-04"
    /// </para>
    /// <para>
    /// • "mcp-client-2025-11-20"
    /// </para>
    /// <para>
    /// • "dev-full-thinking-2025-05-14"
    /// </para>
    /// <para>
    /// • "interleaved-thinking-2025-05-14"
    /// </para>
    /// <para>
    /// • "code-execution-2025-05-22"
    /// </para>
    /// <para>
    /// • "extended-cache-ttl-2025-04-11"
    /// </para>
    /// <para>
    /// • "context-1m-2025-08-07"
    /// </para>
    /// <para>
    /// • "context-management-2025-06-27"
    /// </para>
    /// <para>
    /// • "model-context-window-exceeded-2025-08-26"
    /// </para>
    /// <para>
    /// • "skills-2025-10-02"
    /// </para>
    /// <para>
    /// • "compact-2026-01-12"
    /// </para>
    /// </remarks>
    function Beta(const Value: TArray<string>): TChatParams; overload;

    /// <summary>
    /// Optional header to specify the beta version(s) you want to use.
    /// </summary>
    /// <remarks>
    /// Accepts one of the following:
    /// <para>
    /// • "message-batches-2024-09-24"
    /// </para>
    /// <para>
    /// • "prompt-caching-2024-07-31"
    /// </para>
    /// <para>
    /// • "computer-use-2024-10-22"
    /// </para>
    /// <para>
    /// • "computer-use-2025-01-24"
    /// </para>
    /// <para>
    /// • "pdfs-2024-09-25"
    /// </para>
    /// <para>
    /// • "token-counting-2024-11-01"
    /// </para>
    /// <para>
    /// • "token-efficient-tools-2025-02-19"
    /// </para>
    /// <para>
    /// • "output-128k-2025-02-19"
    /// </para>
    /// <para>
    /// • "files-api-2025-04-14"
    /// </para>
    /// <para>
    /// • "mcp-client-2025-04-04"
    /// </para>
    /// <para>
    /// • "mcp-client-2025-11-20"
    /// </para>
    /// <para>
    /// • "dev-full-thinking-2025-05-14"
    /// </para>
    /// <para>
    /// • "interleaved-thinking-2025-05-14"
    /// </para>
    /// <para>
    /// • "code-execution-2025-05-22"
    /// </para>
    /// <para>
    /// • "extended-cache-ttl-2025-04-11"
    /// </para>
    /// <para>
    /// • "context-1m-2025-08-07"
    /// </para>
    /// <para>
    /// • "context-management-2025-06-27"
    /// </para>
    /// <para>
    /// • "model-context-window-exceeded-2025-08-26"
    /// </para>
    /// <para>
    /// • "skills-2025-10-02"
    /// </para>
    /// </remarks>
    function Beta(const Value: TArray<TBeta>): TChatParams; overload;

    /// <summary>
    /// The maximum number of tokens to generate before stopping.
    /// </summary>
    /// <remarks>
    /// Note that our models may stop before reaching this maximum. This parameter only specifies
    /// the absolute maximum number of tokens to generate.
    /// <para>
    /// • Different models have different maximum values for this parameter.
    /// </para>
    /// <para>
    /// • minimum 1
    /// </para>
    /// </remarks>
    function MaxTokens(const Value: Integer): TChatParams;

    /// <summary>
    /// Input messages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • Models are trained to operate on alternating user and assistant conversational turns. When creating
    /// a new Message, specify the prior conversational turns with the messages parameter, and the model then
    /// generates the next Message in the conversation. Consecutive user or assistant turns in your request
    /// will be combined into a single turn.
    /// </para>
    /// <para>
    /// • Each input message must be an object with a role and content. You can specify a single user-role
    /// message, or you can include multiple user and assistant messages.
    /// </para>
    /// <para>
    /// • If the final message uses the assistant role, the response content will continue immediately from
    /// the content in that message. This can be used to constrain part of the model's response.
    /// </para>
    /// </para>
    /// <para>
    /// • Each input message content may be either a single string or an array of content blocks, where each
    /// block has a specific type. Using a string for content is shorthand for an array of one content block
    /// of type "text".
    /// </para>
    /// <para>
    /// • Note that if you want to include a system prompt, you can use the top-level system parameter —
    /// there is no "system" role for input messages in the Messages API.
    /// </para>
    /// <para>
    /// • There is a limit of 100,000 messages in a single request.
    /// </para>
    /// </remarks>
    function Messages(const Value: TArray<TMessageParam>): TChatParams; overload;

    /// <summary>
    /// Input messages.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • Models are trained to operate on alternating user and assistant conversational turns. When creating
    /// a new Message, specify the prior conversational turns with the messages parameter, and the model then
    /// generates the next Message in the conversation. Consecutive user or assistant turns in your request
    /// will be combined into a single turn.
    /// </para>
    /// <para>
    /// • Each input message must be an object with a role and content. You can specify a single user-role
    /// message, or you can include multiple user and assistant messages.
    /// </para>
    /// <para>
    /// • If the final message uses the assistant role, the response content will continue immediately from
    /// the content in that message. This can be used to constrain part of the model's response.
    /// </para>
    /// </para>
    /// <para>
    /// • Each input message content may be either a single string or an array of content blocks, where each
    /// block has a specific type. Using a string for content is shorthand for an array of one content block
    /// of type "text".
    /// </para>
    /// <para>
    /// • Note that if you want to include a system prompt, you can use the top-level system parameter —
    /// there is no "system" role for input messages in the Messages API.
    /// </para>
    /// <para>
    /// • There is a limit of 100,000 messages in a single request.
    /// </para>
    /// </remarks>
    function Messages(const Value: string): TChatParams; overload;

    /// <summary>
    /// The model that will complete your prompt.
    /// </summary>
    function Model(const Value: string): TChatParams;

    /// <summary>
    /// Container identifier for reuse across requests.
    /// </summary>
    function Container(const Value: string): TChatParams; overload;

    /// <summary>
    /// Container identifier for reuse across requests.
    /// </summary>
    function Container(const Value: TContainerParams): TChatParams; overload;

    /// <summary>
    /// Context management configuration.
    /// </summary>
    /// <remarks>
    /// This allows you to control how Claude manages context across multiple requests, such as whether
    /// to clear function results or not.
    /// </remarks>
    function ContextManagement(const Value: TContextManagementConfig): TChatParams; overload;

    /// <summary>
    /// Context management configuration.
    /// </summary>
    /// <remarks>
    /// This allows you to control how Claude manages context across multiple requests, such as whether
    /// to clear function results or not.
    /// </remarks>
    function ContextManagement(const Value: string): TChatParams; overload;

    /// <summary>
    /// Specifies the geographic region for inference processing. If not specified, the workspace's
    /// default_inference_geo is used.
    /// </summary>
    function InferenceGeo(const Value: string): TChatParams;

    /// <summary>
    /// MCP servers to be utilized in this request
    /// </summary>
    function McpServers(const Value: TArray<TRequestMCPServerURLDefinition>): TChatParams; overload;

    /// <summary>
    /// MCP servers to be utilized in this request
    /// </summary>
    function McpServers(const Value: string): TChatParams; overload;

    /// <summary>
    /// An external identifier for the user who is associated with the request.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This should be a uuid, hash value, or other opaque identifier. Anthropic may use this id to help
    /// detect abuse. Do not include any identifying information such as name, email address, or phone
    /// number.
    /// </para>
    /// <para>
    /// • maxLength 256
    /// </para>
    /// </remarks>
    function Metadata(const Value: TJSONObject): TChatParams; overload;

    /// <summary>
    /// An external identifier for the user who is associated with the request.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • This should be a uuid, hash value, or other opaque identifier. Anthropic may use this id to help
    /// detect abuse. Do not include any identifying information such as name, email address, or phone
    /// number.
    /// </para>
    /// <para>
    /// • maxLength 256
    /// </para>
    /// </remarks>
    function Metadata(const Value: string): TChatParams; overload;

    /// <summary>
    /// Configuration options for the model's output, such as the output format.
    /// </summary>
    function OutputConfig(const Value: TOutputConfig): TChatParams; overload;

    /// <summary>
    /// Configuration options for the model's output, such as the output format.
    /// </summary>
    function OutputConfig(const Value: string): TChatParams; overload;

    /// <summary>
    /// Determines whether to use priority capacity (if available) or standard capacity for this request.
    /// </summary>
    /// <remarks>
    /// Anthropic offers different levels of service for your API requests.
    /// </remarks>
    function ServiceTier(const Value: TServiceTierType): TChatParams; overload;

    /// <summary>
    /// Determines whether to use priority capacity (if available) or standard capacity for this request.
    /// </summary>
    /// <remarks>
    /// Anthropic offers different levels of service for your API requests.
    /// </remarks>
    function ServiceTier(const Value: string): TChatParams; overload;

    /// <summary>
    /// Body param: The inference speed mode for this request. "fast" enables high
    /// output-tokens-per-second inference.
    /// </summary>
    /// <remarks>
    /// Only with Claude Opus 4.6
    /// <para>
    /// • Fast mode is priced at 6x standard Opus rates for prompts <= 200K tokens, and 12x standard Opus
    /// rates for prompts > 200K tokens.
    /// </para>
    /// </remarks>
    function Speed(const Value: TSpeedType): TChatParams; overload;

    /// <summary>
    /// Body param: The inference speed mode for this request. "fast" enables high
    /// output-tokens-per-second inference.
    /// </summary>
    /// <remarks>
    /// Only with Claude Opus 4.6
    /// <para>
    /// • Fast mode is priced at 6x standard Opus rates for prompts <= 200K tokens, and 12x standard Opus
    /// rates for prompts > 200K tokens.
    /// </para>
    /// </remarks>
    function Speed(const Value: string): TChatParams; overload;

    /// <summary>
    /// Custom text sequences that will cause the model to stop generating.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • Models will normally stop when they have naturally completed their turn, which will result
    /// in a response <c>stop_reason</c> of <c>end_turn</c>.
    /// </para>
    /// <para>
    /// • If you want the model to stop generating when it encounters custom strings of text, you can
    /// use the <c>stop_sequences</c> parameter. If the model encounters one of the custom sequences,
    /// response <c>stop_reason</c> value will be <c>stop_sequence</c> and the response stop_sequence
    /// the value will contain the matched stop sequence.
    /// </para>
    /// </remarks>
    function StopSequences(const Value: TArray<string>): TChatParams;

    /// <summary>
    /// Whether to incrementally stream the response using server-sent events.
    /// </summary>
    function Stream(const Value: Boolean = True): TChatParams;

    /// <summary>
    /// System prompt.
    /// </summary>
    /// <remarks>
    /// A system prompt is a way of providing context and instructions to Claude, such as specifying
    /// a particular goal or role.
    /// </remarks>
    function System(const Value: string): TChatParams; overload;

    /// <summary>
    /// System prompt.
    /// </summary>
    /// <remarks>
    /// A system prompt is a way of providing context and instructions to Claude, such as specifying
    /// a particular goal or role.
    /// </remarks>
    function System(const Value: TArray<TTextBlockParam>): TChatParams; overload;

    /// <summary>
    /// Amount of randomness injected into the response.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • Defaults to 1.0. Ranges from 0.0 to 1.0. Use temperature closer to 0.0 for analytical / multiple
    /// choice, and closer to 1.0 for creative and generative tasks.
    /// </para>
    /// <para>
    /// • Note that even with temperature of 0.0, the results will not be fully deterministic.
    /// </para>
    /// <para>
    /// • maximum 1 ; minimum 0
    /// </para>
    /// </remarks>
    function Temperature(const Value: Double): TChatParams;

    /// <summary>
    /// Configuration for enabling Claude's extended thinking.
    /// </summary>
    /// <remarks>
    /// When enabled, responses include thinking content blocks showing Claude's thinking process before
    /// the final answer. Requires a minimum budget of 1,024 tokens and counts towards your max_tokens limit.
    /// </remarks>
    function Thinking(const Value: TThinkingConfigParam): TChatParams;

    /// <summary>
    /// How the model should use the provided tools. The model can use a specific tool, any available tool,
    /// decide by itself, or not use tools at all.
    /// </summary>
    function ToolChoice(const Value: TToolChoice): TChatParams;

    /// <summary>
    /// Definitions of tools that the model may use.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • If you include tools in your API request, the model may return tool_use content blocks that
    /// represent the model's use of those tools. You can then run those tools using the tool input
    /// generated by the model and then optionally return results back to the model using tool_result
    /// content blocks.
    /// </para>
    /// <para>
    /// • There are two types of tools: client tools and server tools.
    /// See https://platform.claude.com/docs/en/api/messages/create
    /// </para>
    /// </remarks>
    function Tools(const Value: TArray<TToolUnion>): TChatParams; overload;

    /// <summary>
    /// Definitions of tools that the model may use.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • If you include tools in your API request, the model may return tool_use content blocks that
    /// represent the model's use of those tools. You can then run those tools using the tool input
    /// generated by the model and then optionally return results back to the model using tool_result
    /// content blocks.
    /// </para>
    /// <para>
    /// • There are two types of tools: client tools and server tools.
    /// See https://platform.claude.com/docs/en/api/messages/create
    /// </para>
    /// </remarks>
    function Tools(const Value: string): TChatParams; overload;

    /// <summary>
    /// Only sample from the top K options for each subsequent token.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • Used to remove "long tail" low probability responses.
    /// </para>
    /// <para>
    /// • Recommended for advanced use cases only. You usually only need to use temperature.
    /// </para>
    /// <para>
    /// • minimum 0
    /// </para>
    /// </remarks>
    function TopK(const Value: Single): TChatParams;

    /// <summary>
    /// Use nucleus sampling.
    /// </summary>
    /// <remarks>
    /// <para>
    /// • In nucleus sampling, we compute the cumulative distribution over all the options for each
    /// subsequent token in decreasing probability order and cut it off once it reaches a particular
    /// probability specified by top_p. You should either alter temperature or top_p, but not both.
    /// </para>
    /// <para>
    /// • Recommended for advanced use cases only. You usually only need to use temperature.
    /// </para>
    /// <para>
    /// • maximum 1 , minimum 0
    /// </para>
    /// </remarks>
    function TopP(const Value: Single): TChatParams;
  end;

  TChatParamProc = TProc<TChatParams>;

implementation

{ TChatParams }

function TChatParams.Beta(const Value: TArray<string>): TChatParams;
begin
  Result := TChatParams(Add('beta', Value));
end;

function TChatParams.Beta(const Value: TArray<TBeta>): TChatParams;
begin
  var BetaValues: TArray<string>;
  for var Item in Value do
    BetaValues := BetaValues + [Item.ToString];

  Result := TChatParams(Add('beta', BetaValues));
end;

function TChatParams.Container(const Value: TContainerParams): TChatParams;
begin
  Result := TChatParams(Add('container', Value.Detach));
end;

function TChatParams.ContextManagement(const Value: string): TChatParams;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TChatParams(Add('context_management', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TChatParams.ContextManagement(
  const Value: TContextManagementConfig): TChatParams;
begin
  Result := TChatParams(Add('context_management', Value.Detach));
end;

function TChatParams.InferenceGeo(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('inference_geo', Value));
end;

function TChatParams.Container(const Value: string): TChatParams;
var
  JSONObject: TJSONObject;
begin
  if not ( Value.Trim.StartsWith('{') and Value.Trim.EndsWith('}') ) then
    Exit( TChatParams(Add('container', Value)) );

  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit( TChatParams(Add('container', JSONObject)) );

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TChatParams.MaxTokens(const Value: Integer): TChatParams;
begin
  Result := TChatParams(Add('max_tokens', Value));
end;

function TChatParams.McpServers(const Value: string): TChatParams;
var
  JSONArray: TJSONArray;
begin
  if TJSONHelper.TryGetArray(Value, JSONArray) then
    Exit(TChatParams(Add('mcp_servers', JSONArray)));

  raise EAnthropicException.Create('Invalid JSON Array');
end;

function TChatParams.McpServers(
  const Value: TArray<TRequestMCPServerURLDefinition>): TChatParams;
begin
  Result := TChatParams(Add('mcp_servers',
    TJSONHelper.ToJsonArray<TRequestMCPServerURLDefinition>(Value)));
end;

function TChatParams.Messages(const Value: TArray<TMessageParam>): TChatParams;
begin
  Result := TChatParams(Add('messages',
    TJSONHelper.ToJsonArray<TMessageParam>(Value)));
end;

function TChatParams.Messages(const Value: string): TChatParams;
var
  JSONArray: TJSONArray;
begin
  if TJSONHelper.TryGetArray(Value, JSONArray) then
    Exit(TChatParams(Add('messages', JSONArray)));

  raise EAnthropicException.Create('Invalid JSON Array');
end;

function TChatParams.Metadata(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('metadata', TJSONObject.Create
    .AddPair('user_id', Value) )
  );
end;

function TChatParams.Metadata(const Value: TJSONObject): TChatParams;
begin
  Result := TChatParams(Add('metadata', Value));
end;

function TChatParams.Model(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('model', Value));
end;

function TChatParams.OutputConfig(const Value: TOutputConfig): TChatParams;
begin
  Result := TChatParams(Add('output_config', Value.Detach));
end;

function TChatParams.OutputConfig(const Value: string): TChatParams;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TChatParams(Add('output_config', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TChatParams.ServiceTier(const Value: TServiceTierType): TChatParams;
begin
  Result := Self.ServiceTier(Value.ToString);
end;

function TChatParams.ServiceTier(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('service_tier', Value));
end;

function TChatParams.Speed(const Value: string): TChatParams;
begin
  Result := TChatParams(Add('speed', Value));
end;

function TChatParams.Speed(const Value: TSpeedType): TChatParams;
begin
  Result := Speed(Value.ToString);
end;

function TChatParams.StopSequences(const Value: TArray<string>): TChatParams;
begin
  Result := TChatParams(Add('stop_sequences', Value));
end;

function TChatParams.Stream(const Value: Boolean): TChatParams;
begin
  Result := TChatParams(Add('stream', Value));
end;

function TChatParams.System(const Value: TArray<TTextBlockParam>): TChatParams;
begin
  Result := TChatParams(Add('system',
    TJSONHelper.ToJsonArray<TTextBlockParam>(Value)));
end;

function TChatParams.System(const Value: string): TChatParams;
var
  JSONArray: TJSONArray;
begin
  if not Value.StartsWith('[') then
    {--- We consider value is a string and not a JSON string }
    Exit(TChatParams(Add('system', Value)));

  {--- Value is a JSON array }
  if TJSONHelper.TryGetArray(Value, JSONArray) then
    Exit(TChatParams(Add('system', JSONArray)));

  raise EAnthropicException.Create('Invalid JSON Array');
end;

function TChatParams.Temperature(const Value: Double): TChatParams;
begin
  Result := TChatParams(Add('temperature', Value));
end;

function TChatParams.Thinking(const Value: TThinkingConfigParam): TChatParams;
begin
  Result := TChatParams(Add('thinking', Value.Detach));
end;

function TChatParams.ToolChoice(const Value: TToolChoice): TChatParams;
begin
  Result := TChatParams(Add('tool_choice', Value.Detach));
end;

function TChatParams.Tools(const Value: string): TChatParams;
var
  JSONArray: TJSONArray;
begin
  if TJSONHelper.TryGetArray(Value, JSONArray) then
    Exit(TChatParams(Add('tools', JSONArray)));

  raise EAnthropicException.Create('Invalid JSON Array');
end;

function TChatParams.Tools(const Value: TArray<TToolUnion>): TChatParams;
begin
  Result := TChatParams(Add('tools',
    TJSONHelper.ToJsonArray<TToolUnion>(Value)));
end;

function TChatParams.TopK(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_k', Value));
end;

function TChatParams.TopP(const Value: Single): TChatParams;
begin
  Result := TChatParams(Add('top_p', Value));
end;

{ TMessageParam }

function TMessageParam.Content(
  const Value: TArray<TContentBlockParam>): TMessageParam;
begin
  Result := TMessageParam(Add('content',
    TJSONHelper.ToJsonArray<TContentBlockParam>(Value)));
end;

function TMessageParam.Content(const Value: string): TMessageParam;
begin
  Result := TMessageParam(Add('content', Value));
end;

class function TMessageParam.New: TMessageParam;
begin
  Result := TMessageParam.Create;
end;

function TMessageParam.Role(const Value: TMessageRole): TMessageParam;
begin
  Result := TMessageParam(Add('role', Value.ToString));
end;

function TMessageParam.Role(const Value: string): TMessageParam;
begin
  Result := TMessageParam(Add('role', Value));
end;

{ TTextBlockParam }

function TTextBlockParam.&Type(const Value: string): TTextBlockParam;
begin
  Result := TTextBlockParam(Add('type', Value));
end;

function TTextBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TTextBlockParam;
begin
  Result := TTextBlockParam(Add('cache_control', Value.Detach));
end;

function TTextBlockParam.Citations(
  const Value: TArray<TTextCitationParam>): TTextBlockParam;
begin
  Result := TTextBlockParam(Add('citations',
    TJSONHelper.ToJsonArray<TTextCitationParam>(Value)));
end;

class function TTextBlockParam.New: TTextBlockParam;
begin
  Result := TTextBlockParam.Create.&Type();
end;

function TTextBlockParam.Text(const Value: string): TTextBlockParam;
begin
  Result := TTextBlockParam(Add('text', Value));
end;

{ TCacheControlEphemeral }

class function TCacheControlEphemeral.New: TCacheControlEphemeral;
begin
  Result := TCacheControlEphemeral.Create.&Type();
end;

function TCacheControlEphemeral.Ttl(
  const Value: string): TCacheControlEphemeral;
begin
  Result := TCacheControlEphemeral(Add('ttl', Value));
end;

function TCacheControlEphemeral.&Type(
  const Value: string): TCacheControlEphemeral;
begin
  Result := TCacheControlEphemeral(Add('type', Value));
end;

{ TCitationCharLocationParam }

function TCitationCharLocationParam.CitedText(
  const Value: string): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('cited_text', Value));
end;

function TCitationCharLocationParam.DocumentIndex(
  const Value: Double): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('document_index', Value));
end;

function TCitationCharLocationParam.DocumentTitle(
  const Value: string): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('document_title', Value));
end;

function TCitationCharLocationParam.EndCharIndex(
  const Value: Double): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('end_char_index', Value));
end;

class function TCitationCharLocationParam.New: TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam.Create.&Type();
end;

function TCitationCharLocationParam.StartCharIndex(
  const Value: Double): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('start_char_index', Value));
end;

function TCitationCharLocationParam.&Type(
  const Value: string): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam(Add('type', Value));
end;

{ TCitationPageLocationParam }

function TCitationPageLocationParam.CitedText(
  const Value: string): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('cited_text', Value));
end;

function TCitationPageLocationParam.DocumentIndex(
  const Value: Double): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('document_index', Value));
end;

function TCitationPageLocationParam.DocumentTitle(
  const Value: string): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('document_title', Value));
end;

function TCitationPageLocationParam.EndPageNumber(
  const Value: Double): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('end_page_number', Value));
end;

class function TCitationPageLocationParam.New: TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam.Create.&Type();
end;

function TCitationPageLocationParam.StartPageNCumber(
  const Value: Double): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('start_page_number', Value));
end;

function TCitationPageLocationParam.&Type(
  const Value: string): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam(Add('type', Value));
end;

{ TCitationContentBlockLocationParam }

function TCitationContentBlockLocationParam.CitedText(
  const Value: string): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('cited_text', Value));
end;

function TCitationContentBlockLocationParam.DocumentIndex(
  const Value: Double): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('document_index', Value));
end;

function TCitationContentBlockLocationParam.DocumentTitle(
  const Value: string): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('document_title', Value));
end;

function TCitationContentBlockLocationParam.EndBlockIndex(
  const Value: Double): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('end_block_index', Value));
end;

class function TCitationContentBlockLocationParam.New: TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam.Create.&Type();
end;

function TCitationContentBlockLocationParam.StartBlockIndex(
  const Value: Double): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('start_block_index', Value));
end;

function TCitationContentBlockLocationParam.&Type(
  const Value: string): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam(Add('type', Value));
end;

{ TCitationWebSearchResultLocationParam }

function TCitationWebSearchResultLocationParam.CitedText(
  const Value: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam(Add('cited_text', Value));
end;

function TCitationWebSearchResultLocationParam.EncryptedIndex(
  const Value: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam(Add('encrypted_index', Value));
end;

class function TCitationWebSearchResultLocationParam.New: TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam.Create.&Type();
end;

function TCitationWebSearchResultLocationParam.Title(
  const Value: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam(Add('title', Value));
end;

function TCitationWebSearchResultLocationParam.&Type(
  const Value: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam(Add('type', Value));
end;

function TCitationWebSearchResultLocationParam.Url(
  const Value: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam(Add('url', Value));
end;

{ TCitationSearchResultLocationParam }

function TCitationSearchResultLocationParam.CitedText(
  const Value: string): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('cited_text', Value));
end;

function TCitationSearchResultLocationParam.EndBlockIndex(
  const Value: Double): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('end_block_index', Value));
end;

class function TCitationSearchResultLocationParam.New: TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam.Create.&Type();
end;

function TCitationSearchResultLocationParam.SearchResultIndex(
  const Value: Double): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('search_result_index', Value));
end;

function TCitationSearchResultLocationParam.Source(
  const Value: string): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('source', Value));
end;

function TCitationSearchResultLocationParam.StartBlockIndex(
  const Value: Double): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('start_block_index', Value));
end;

function TCitationSearchResultLocationParam.Title(
  const Value: string): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('title', Value));
end;

function TCitationSearchResultLocationParam.&Type(
  const Value: string): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam(Add('type', Value));
end;

{ TImageBlockParam }

function TImageBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TImageBlockParam;
begin
  Result := TImageBlockParam(Add('cache_control', Value.Detach));
end;

class function TImageBlockParam.New: TImageBlockParam;
begin
  Result := TImageBlockParam.Create.&Type();
end;

function TImageBlockParam.Source(const Base64OrUri,
  MimeType: string): TImageBlockParam;
begin
  if Base64OrUri.ToLower.StartsWith('http') then
    begin
      var JSONParam := TURLImageSource.New.Url(Base64OrUri);
      Exit(TImageBlockParam(Add('source', JSONParam.Detach)));
    end;

  if Base64OrUri.ToLower.StartsWith('file_') then
    begin
      var JSONParam := TFileImageSource.New.FileId(Base64OrUri);
      Exit(TImageBlockParam(Add('source', JSONParam.Detach)));
    end;

  var JSONParam := TBase64ImageSource.New.Data(Base64OrUri).MediaType(MimeType);
  Result := TImageBlockParam(Add('source', JSONParam.Detach));
end;

function TImageBlockParam.Source(const Value: TImageSource): TImageBlockParam;
begin
  Result := TImageBlockParam(Add('source', Value.Detach));
end;

function TImageBlockParam.&Type(const Value: string): TImageBlockParam;
begin
  Result := TImageBlockParam(Add('type', Value));
end;

{ TBase64ImageSource }

function TBase64ImageSource.MediaType(const Value: string): TBase64ImageSource;
begin
  Result := TBase64ImageSource(Add('media_type', Value));
end;

class function TBase64ImageSource.New: TBase64ImageSource;
begin
  Result := TBase64ImageSource.Create.&Type();
end;

function TBase64ImageSource.&Type(const Value: string): TBase64ImageSource;
begin
  Result := TBase64ImageSource(Add('type', Value));
end;

function TBase64ImageSource.Data(const Value: string): TBase64ImageSource;
begin
  Result := TBase64ImageSource(Add('data', Value));
end;

{ TURLImageSource }

class function TURLImageSource.New: TURLImageSource;
begin
  Result := TURLImageSource.Create.&Type();
end;

function TURLImageSource.&Type(const Value: string): TURLImageSource;
begin
  Result := TURLImageSource(Add('type', Value));
end;

function TURLImageSource.Url(const Value: string): TURLImageSource;
begin
  Result := TURLImageSource(Add('url', Value));
end;

{ TBase64PDFSource }

function TBase64PDFSource.Data(const Value: string): TBase64PDFSource;
begin
  Result := TBase64PDFSource(Add('data', Value));
end;

function TBase64PDFSource.MediaType(const Value: string): TBase64PDFSource;
begin
  Result := TBase64PDFSource(Add('media_type', Value));
end;

class function TBase64PDFSource.New: TBase64PDFSource;
begin
  Result := TBase64PDFSource.Create.&Type().MediaType();
end;

function TBase64PDFSource.&Type(const Value: string): TBase64PDFSource;
begin
  Result := TBase64PDFSource(Add('type', Value));
end;

{ TPlainTextSource }

function TPlainTextSource.Data(const Value: string): TPlainTextSource;
begin
  Result := TPlainTextSource(Add('data', Value));
end;

function TPlainTextSource.MediaType(const Value: string): TPlainTextSource;
begin
  Result := TPlainTextSource(Add('media_type', Value));
end;

class function TPlainTextSource.New: TPlainTextSource;
begin
  Result := TPlainTextSource.Create.&Type().MediaType();
end;

function TPlainTextSource.&Type(const Value: string): TPlainTextSource;
begin
  Result := TPlainTextSource(Add('type', Value));
end;

{ TContentBlockSource }

function TContentBlockSource.Content(
  const Value: TArray<TContentBlockSourceContent>): TContentBlockSource;
begin
  Result := TContentBlockSource(Add('content',
    TJSONHelper.ToJsonArray<TContentBlockSourceContent>(Value)));
end;

function TContentBlockSource.Content(const Value: string): TContentBlockSource;
begin
  Result := TContentBlockSource(Add('content', Value));
end;

class function TContentBlockSource.New: TContentBlockSource;
begin
  Result := TContentBlockSource.Create.&Type();
end;

function TContentBlockSource.&Type(const Value: string): TContentBlockSource;
begin
  Result := TContentBlockSource(Add('type', Value));
end;

{ TURLPDFSource }

class function TURLPDFSource.New: TURLPDFSource;
begin
  Result := TURLPDFSource.Create.&Type();
end;

function TURLPDFSource.&Type(const Value: string): TURLPDFSource;
begin
  Result := TURLPDFSource(Add('type', Value));
end;

function TURLPDFSource.Url(const Value: string): TURLPDFSource;
begin
  Result := TURLPDFSource(Add('url', Value));
end;

{ TDocumentBlockParam }

function TDocumentBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('cache_control', Value.Detach));
end;

function TDocumentBlockParam.Citations(
  const Value: TCitationsConfigParam): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('citations', Value.Detach));
end;

function TDocumentBlockParam.Citations(
  const Value: Boolean): TDocumentBlockParam;
begin
  var ConfigParam := TCitationsConfigParam.Create
          .Enabled(Value);

  Result := TDocumentBlockParam(Add('citations', ConfigParam.Detach));
end;

function TDocumentBlockParam.Context(const Value: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('context', Value));
end;

class function TDocumentBlockParam.New: TDocumentBlockParam;
begin
  Result := TDocumentBlockParam.Create.&Type();
end;

function TDocumentBlockParam.Source(
  const Value: TArray<TContentBlockParam>): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('source',
    TJSONHelper.ToJsonArray<TContentBlockParam>(Value)));
end;

function TDocumentBlockParam.Source(
  const Value: TDocumentSourceParam): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('source', Value.Detach));
end;

function TDocumentBlockParam.Title(const Value: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('title', Value));
end;

function TDocumentBlockParam.&Type(const Value: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam(Add('type', Value));
end;

{ TCitationsConfigParam }

function TCitationsConfigParam.Enabled(
  const Value: Boolean): TCitationsConfigParam;
begin
  Result :=TCitationsConfigParam(Add('enabled', Value));
end;

{ TSearchResultBlockParam }

function TSearchResultBlockParam.Title(
  const Value: string): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('title', Value));
end;

function TSearchResultBlockParam.&Type(
  const Value: string): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('type', Value));
end;

function TSearchResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('cache_control', Value.Detach));
end;

function TSearchResultBlockParam.Citations(
  const Value: TCitationsConfigParam): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('citations', Value.Detach));
end;

function TSearchResultBlockParam.Content(
  const Value: TArray<TTextBlockParam>): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TTextBlockParam>(Value)));
end;

class function TSearchResultBlockParam.New: TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam.Create.&Type();
end;

function TSearchResultBlockParam.Source(
  const Value: string): TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam(Add('source', Value));
end;

{ TThinkingBlockParam }

class function TThinkingBlockParam.New: TThinkingBlockParam;
begin
  Result := TThinkingBlockParam.Create.&Type();
end;

function TThinkingBlockParam.Signature(
  const Value: string): TThinkingBlockParam;
begin
  Result := TThinkingBlockParam(Add('signature', Value));
end;

function TThinkingBlockParam.Thinking(const Value: string): TThinkingBlockParam;
begin
  Result := TThinkingBlockParam(Add('thinking', Value));
end;

function TThinkingBlockParam.&Type(const Value: string): TThinkingBlockParam;
begin
  Result := TThinkingBlockParam(Add('type', Value));
end;

{ TRedactedThinkingBlockParam }

function TRedactedThinkingBlockParam.Data(
  const Value: string): TRedactedThinkingBlockParam;
begin
  Result := TRedactedThinkingBlockParam(Add('data', Value));
end;

class function TRedactedThinkingBlockParam.New: TRedactedThinkingBlockParam;
begin
  Result := TRedactedThinkingBlockParam.Create.&Type();
end;

function TRedactedThinkingBlockParam.&Type(
  const Value: string): TRedactedThinkingBlockParam;
begin
  Result := TRedactedThinkingBlockParam(Add('type', Value));
end;

{ TToolUseBlockParam }

function TToolUseBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('cache_control', Value.Detach));
end;

function TToolUseBlockParam.Caller(
  const Value: TDirectCaller): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('caller', Value.Detach));
end;

function TToolUseBlockParam.Caller(
  const Value: TServerToolCaller): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('caller', Value.Detach));
end;

function TToolUseBlockParam.Id(const Value: string): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('id', Value));
end;

function TToolUseBlockParam.Input(const Value: TJSONObject): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('input', Value));
end;

function TToolUseBlockParam.Input(const Value: string): TToolUseBlockParam;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TToolUseBlockParam(Add('input', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TToolUseBlockParam.Name(const Value: string): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('name', Value));
end;

class function TToolUseBlockParam.New: TToolUseBlockParam;
begin
  Result := TToolUseBlockParam.Create.&Type();
end;

function TToolUseBlockParam.&Type(const Value: string): TToolUseBlockParam;
begin
  Result := TToolUseBlockParam(Add('type', Value));
end;

{ TToolResultBlockParam }

function TToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TToolResultBlockParam.Content(
  const Value: TArray<TContentBlockParam>): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TContentBlockParam>(Value)));
end;

function TToolResultBlockParam.Content(
  const Value: string): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('content', Value));
end;

function TToolResultBlockParam.IsError(
  const Value: Boolean): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('is_error', Value));
end;

class function TToolResultBlockParam.New: TToolResultBlockParam;
begin
  Result := TToolResultBlockParam.Create.&Type();
end;

function TToolResultBlockParam.ToolUseId(
  const Value: string): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('tool_use_id', Value));
end;

function TToolResultBlockParam.&Type(
  const Value: string): TToolResultBlockParam;
begin
  Result := TToolResultBlockParam(Add('type', Value));
end;

{ TServerToolUseBlockParam }

function TServerToolUseBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('cache_control', Value.Detach));
end;

function TServerToolUseBlockParam.Caller(
  const Value: TServerToolCaller): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('caller', Value.Detach));
end;

function TServerToolUseBlockParam.Caller(
  const Value: TDirectCaller): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('caller', Value.Detach));
end;

function TServerToolUseBlockParam.Id(
  const Value: string): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('id', Value));
end;

function TServerToolUseBlockParam.Input(
  const Value: TJSONObject): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('input', Value));
end;

function TServerToolUseBlockParam.Input(
  const Value: string): TServerToolUseBlockParam;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TServerToolUseBlockParam(Add('input', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TServerToolUseBlockParam.Name(
  const Value: string): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('name', Value));
end;

function TServerToolUseBlockParam.Name(
  const Value: TServerToolUseName): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('name', Value.ToString));
end;

class function TServerToolUseBlockParam.New: TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam.Create.&Type();
end;

function TServerToolUseBlockParam.&Type(
  const Value: string): TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam(Add('type', Value));
end;

{ TWebSearchToolResultBlockParam }

function TWebSearchToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TWebSearchToolResultBlockParam.Content(
  const Value: TWebSearchToolRequestError): TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam(Add('content', Value.Detach));
end;

class function TWebSearchToolResultBlockParam.New: TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam.Create.&Type();
end;

function TWebSearchToolResultBlockParam.Content(
  const Value: TArray<TWebSearchToolResultBlockItem>): TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TWebSearchToolResultBlockItem>(Value)));
end;

function TWebSearchToolResultBlockParam.ToolUseId(
  const Value: string): TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam(Add('tool_use_id', Value));
end;

function TWebSearchToolResultBlockParam.&Type(
  const Value: string): TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam(Add('type', Value));
end;

{ TWebSearchToolResultBlockItem }

function TWebSearchToolResultBlockItem.EncryptedContent(
  const Value: string): TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem(Add('encrypted_content', Value));
end;

class function TWebSearchToolResultBlockItem.New: TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem.Create.&Type();
end;

function TWebSearchToolResultBlockItem.PageAge(
  const Value: string): TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem(Add('page_age', Value));
end;

function TWebSearchToolResultBlockItem.Title(
  const Value: string): TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem(Add('title', Value));
end;

function TWebSearchToolResultBlockItem.&Type(
  const Value: string): TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem(Add('type', Value));
end;

function TWebSearchToolResultBlockItem.Url(
  const Value: string): TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem(Add('url', Value));
end;

{ TWebSearchToolRequestError }

function TWebSearchToolRequestError.ErrorCode(
  const Value: TWebSearchError): TWebSearchToolRequestError;
begin
  Result := TWebSearchToolRequestError(Add('error_code', Value.ToString));
end;

function TWebSearchToolRequestError.ErrorCode(
  const Value: string): TWebSearchToolRequestError;
begin
  Result := TWebSearchToolRequestError(Add('error_code', Value));
end;

class function TWebSearchToolRequestError.New: TWebSearchToolRequestError;
begin
  Result := TWebSearchToolRequestError.Create.&Type();
end;

function TWebSearchToolRequestError.&Type(
  const Value: string): TWebSearchToolRequestError;
begin
  Result := TWebSearchToolRequestError(Add('type', Value));
end;

{ TThinkingConfigParam }

function TThinkingConfigParam.BudgetTokens(
  const Value: Integer): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam(Add('budget_tokens', Value));
end;

class function TThinkingConfigParam.New(
  const State: TThinkingType): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam.Create.&Type(State);
end;

class function TThinkingConfigParam.New(
  const Value: string): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam.Create.&Type(Value);
end;

function TThinkingConfigParam.&Type(
  const Value: TThinkingType): TThinkingConfigParam;
begin
  Result := &Type(Value.ToString);
end;

function TThinkingConfigParam.&Type(const Value: string): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam(Add('type', Value));
end;

{ TToolChoice }

function TToolChoice.&Type(const Value: TToolChoiceType): TToolChoice;
begin
  Result := TToolChoice(Add('type', Value.ToString));
end;

function TToolChoice.&Type(const Value: string): TToolChoice;
begin
  Result := TToolChoice(Add('type', Value));
end;

function TToolChoice.DisableParallelToolUse(const Value: Boolean): TToolChoice;
begin
  Result := TToolChoice(Add('disable_parallel_tool_use', Value));
end;

function TToolChoice.Name(const Value: string): TToolChoice;
begin
  Result := TToolChoice(Add('name', Value));
end;

class function TToolChoice.New(const Value: string): TToolChoice;
begin
  Result := TToolChoice.Create.&Type(Value);
end;

class function TToolChoice.New(const Value: TToolChoiceType): TToolChoice;
begin
  Result := TToolChoice.Create.&Type(Value);
end;

{ TTool }

function TTool.InputSchema(const Value: TSchemaParams): TTool;
begin
  Result := TTool(Add('input_schema', Value.Detach));
end;

function TTool.InputSchema(const Value: string): TTool;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TTool(Add('input_schema', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TTool.&Type(const Value: string): TTool;
begin
  Result := TTool(Add('type', Value));
end;

function TTool.AllowedCallers(const Value: TArray<TAllowedCallersType>): TTool;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TTool(Add('allowed_callers', JSONArray));
end;

function TTool.AllowedCallers(const Value: TArray<string>): TTool;
begin
  Result := TTool(Add('allowed_callers', Value));
end;

function TTool.CacheControl(const Value: TCacheControlEphemeral): TTool;
begin
  Result := TTool(Add('cache_control', Value.Detach));
end;

function TTool.DeferLoading(const Value: Boolean): TTool;
begin
  Result := TTool(Add('defer_loading', Value));
end;

function TTool.Description(const Value: string): TTool;
begin
  Result := TTool(Add('description', Value));
end;

function TTool.FunctionPlugin(const Value: IFunctionCore): TTool;
begin
  Result := InputSchema(Value.InputSchema)
    .Name(Value.Name)
    .Description(Value.Description);
end;

function TTool.InputSchema(const Value: TInputSchema): TTool;
begin
  Result := TTool(Add('input_schema', Value.Detach));
end;

function TTool.Name(const Value: string): TTool;
begin
  Result := TTool(Add('name', Value));
end;

class function TTool.New: TTool;
begin
  Result := TTool.Create.&Type();
end;

function TTool.Strict(const Value: Boolean): TTool;
begin
  Result := TTool(Add('strict', Value));
end;

{ TInputSchema }

function TInputSchema.Properties(const Value: TJSONObject): TInputSchema;
begin
  Result := TInputSchema(Add('properties', Value));
end;

class function TInputSchema.New: TInputSchema;
begin
  Result := TInputSchema.Create.&Type();
end;

function TInputSchema.Properties(const Value: string): TInputSchema;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TInputSchema(Add('properties', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TInputSchema.&Type(const Value: string): TInputSchema;
begin
  Result := TInputSchema(Add('type', Value));
end;

{ TToolBash20250124 }

function TToolBash20250124.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolBash20250124;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolBash20250124(Add('allowed_callers', JSONArray));
end;

function TToolBash20250124.AllowedCallers(const Value: TArray<string>): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('allowed_callers', Value));
end;

function TToolBash20250124.CacheControl(
  const Value: TCacheControlEphemeral): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('cache_control', Value.Detach));
end;

function TToolBash20250124.DeferLoading(
  const Value: Boolean): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('defer_loading', Value));
end;

function TToolBash20250124.Name(const Value: string): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('name', Value));
end;

class function TToolBash20250124.New: TToolBash20250124;
begin
  Result := TToolBash20250124.Create.&Type().Name();
end;

function TToolBash20250124.Strict(const Value: Boolean): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('strict', Value));
end;

function TToolBash20250124.&Type(const Value: string): TToolBash20250124;
begin
  Result := TToolBash20250124(Add('type', Value));
end;

{ TToolTextEditor20250124 }

function TToolTextEditor20250124.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolTextEditor20250124;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolTextEditor20250124(Add('allowed_callers', JSONArray));
end;

function TToolTextEditor20250124.AllowedCallers(
  const Value: TArray<string>): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('allowed_callers', Value));
end;

function TToolTextEditor20250124.CacheControl(
  const Value: TCacheControlEphemeral): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('cache_control', Value.Detach));
end;

function TToolTextEditor20250124.DeferLoading(
  const Value: Boolean): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('defer_loading', Value));
end;

function TToolTextEditor20250124.Name(
  const Value: string): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('name', Value));
end;

class function TToolTextEditor20250124.New: TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124.Create.&Type().Name();
end;

function TToolTextEditor20250124.Strict(
  const Value: Boolean): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('strict', Value));
end;

function TToolTextEditor20250124.&Type(
  const Value: string): TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124(Add('type', Value));
end;

{ TToolTextEditor20250429 }

function TToolTextEditor20250429.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolTextEditor20250429;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolTextEditor20250429(Add('allowed_callers', JSONArray));
end;

function TToolTextEditor20250429.AllowedCallers(
  const Value: TArray<string>): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('allowed_callers', Value));
end;

function TToolTextEditor20250429.CacheControl(
  const Value: TCacheControlEphemeral): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('cache_control', Value.Detach));
end;

function TToolTextEditor20250429.DeferLoading(
  const Value: Boolean): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('defer_loading', Value));
end;

function TToolTextEditor20250429.Name(
  const Value: string): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('name', Value));
end;

class function TToolTextEditor20250429.New: TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429.Create.&Type().Name();
end;

function TToolTextEditor20250429.Strict(
  const Value: Boolean): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('strict', Value));
end;

function TToolTextEditor20250429.&Type(
  const Value: string): TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429(Add('type', Value));
end;

{ TToolTextEditor20250728 }

function TToolTextEditor20250728.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolTextEditor20250728;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolTextEditor20250728(Add('allowed_callers', JSONArray));
end;

function TToolTextEditor20250728.AllowedCallers(
  const Value: TArray<string>): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('allowed_callers', Value));
end;

function TToolTextEditor20250728.CacheControl(
  const Value: TCacheControlEphemeral): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('cache_control', Value.Detach));
end;

function TToolTextEditor20250728.DeferLoading(
  const Value: Boolean): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('defer_loading', Value));
end;

function TToolTextEditor20250728.MaxCharacters(
  const Value: Integer): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('max_characters', Value));
end;

function TToolTextEditor20250728.Name(
  const Value: string): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('name', Value));
end;

class function TToolTextEditor20250728.New: TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728.Create.&Type().Name();
end;

function TToolTextEditor20250728.Strict(const Value: Boolean): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('strict', Value));
end;

function TToolTextEditor20250728.&Type(
  const Value: string): TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728(Add('type', Value));
end;

{ TWebSearchTool20250305 }

function TWebSearchTool20250305.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TWebSearchTool20250305;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TWebSearchTool20250305(Add('allowed_callers', JSONArray));
end;

function TWebSearchTool20250305.AllowedCallers(
  const Value: TArray<string>): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('allowed_callers', Value));
end;

function TWebSearchTool20250305.AllowedDomains(
  const Value: TArray<string>): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('allowed_domains', Value));
end;

function TWebSearchTool20250305.BlockedDomains(
  const Value: TArray<string>): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('blocked_domains', Value));
end;

function TWebSearchTool20250305.CacheControl(
  const Value: TCacheControlEphemeral): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('cache_control', Value).Detach);
end;

function TWebSearchTool20250305.DeferLoading(
  const Value: Boolean): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('defer_loading', Value));
end;

function TWebSearchTool20250305.MaxUses(
  const Value: Integer): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('max_uses', Value));
end;

function TWebSearchTool20250305.Name(
  const Value: string): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('name', Value));
end;

class function TWebSearchTool20250305.New: TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305.Create.&Type().Name();
end;

function TWebSearchTool20250305.Strict(
  const Value: Boolean): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('strict', Value));
end;

function TWebSearchTool20250305.&Type(
  const Value: string): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('type', Value));
end;

function TWebSearchTool20250305.UserLocation(
  const Value: TUserLocation): TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305(Add('user_location', Value.Detach));
end;

{ TUserLocation }

function TUserLocation.City(const Value: string): TUserLocation;
begin
  Result := TUserLocation(Add('city', Value));
end;

function TUserLocation.Country(const Value: string): TUserLocation;
begin
  Result := TUserLocation(Add('country', Value));
end;

class function TUserLocation.New: TUserLocation;
begin
  Result := TUserLocation.Create.&Type();
end;

function TUserLocation.Region(const Value: string): TUserLocation;
begin
  Result := TUserLocation(Add('region', Value));
end;

function TUserLocation.Timezone(const Value: string): TUserLocation;
begin
  Result := TUserLocation(Add('timezone', Value));
end;

function TUserLocation.&Type(const Value: string): TUserLocation;
begin
  Result := TUserLocation(Add('type', Value));
end;



{ TDirectCaller }

class function TDirectCaller.New: TDirectCaller;
begin
  Result := TDirectCaller.Create.&Type();
end;

function TDirectCaller.&Type(const Value: string): TDirectCaller;
begin
  Result := TDirectCaller(Add('type', Value));
end;

{ TServerToolCaller }

function TServerToolCaller.ToolId(const Value: string): TServerToolCaller;
begin
  Result := TServerToolCaller(Add('tool_id', Value));
end;

function TServerToolCaller.&Type(const Value: string): TServerToolCaller;
begin
  Result := TServerToolCaller(Add('type', Value));
end;

{ TFileImageSource }

function TFileImageSource.FileId(const Value: string): TFileImageSource;
begin
  Result := TFileImageSource(Add('file_id', Value));
end;

class function TFileImageSource.New: TFileImageSource;
begin
  Result := TFileImageSource.Create.&Type();
end;

function TFileImageSource.&Type(const Value: string): TFileImageSource;
begin
  Result := TFileImageSource(Add('type', Value));
end;

{ TImage }

class function TImage.Base64Source: TBase64ImageSource;
begin
  Result := TBase64ImageSource.New;
end;

class function TImage.FileSource: TFileImageSource;
begin
  Result := TFileImageSource.New;
end;

class function TImage.UrlSource: TURLImageSource;
begin
  Result := TURLImageSource.New;
end;

{ TFileDocumentSource }

function TFileDocumentSource.FileId(const Value: string): TFileDocumentSource;
begin
  Result := TFileDocumentSource(Add('file_id', Value));
end;

class function TFileDocumentSource.New: TFileDocumentSource;
begin
  Result := TFileDocumentSource.Create.&Type();
end;

function TFileDocumentSource.&Type(const Value: string): TFileDocumentSource;
begin
  Result := TFileDocumentSource(Add('type', Value));
end;

{ TDocument }

class function TDocument.Base64PDF: TBase64PDFSource;
begin
  Result := TBase64PDFSource.New;
end;

class function TDocument.ContentBlock: TContentBlockSource;
begin
  Result := TContentBlockSource.New;
end;

class function TDocument.FileDocument: TFileDocumentSource;
begin
  Result := TFileDocumentSource.New;
end;

class function TDocument.PlainText: TPlainTextSource;
begin
  Result := TPlainTextSource.New;
end;

class function TDocument.UrlPdf: TURLPDFSource;
begin
  Result := TURLPDFSource.New;
end;

{ TToolReferenceBlockParam }

function TToolReferenceBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TToolReferenceBlockParam;
begin
  Result := TToolReferenceBlockParam(Add('cache_control', Value.Detach));
end;

class function TToolReferenceBlockParam.New: TToolReferenceBlockParam;
begin
  Result := TToolReferenceBlockParam.Create.&Type();
end;

function TToolReferenceBlockParam.ToolName(
  const Value: string): TToolReferenceBlockParam;
begin
  Result := TToolReferenceBlockParam(Add('tool_name', Value));
end;

function TToolReferenceBlockParam.&Type(
  const Value: string): TToolReferenceBlockParam;
begin
  Result := TToolReferenceBlockParam(Add('type', Value));
end;

{ TWebFetchToolResultBlockParam }

function TWebFetchToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TWebFetchToolResultBlockParam.Content(
  const Value: TWebFetchToolResultErrorBlockParam): TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam(Add('content', Value.Detach));
end;

function TWebFetchToolResultBlockParam.Content(
  const Value: TWebFetchBlockParam): TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam(Add('content', Value.Detach));
end;

class function TWebFetchToolResultBlockParam.New: TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam.Create.&Type();
end;

function TWebFetchToolResultBlockParam.ToolUseId(
  const Value: string): TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam(Add('tool_use_id', Value));
end;

function TWebFetchToolResultBlockParam.&Type(
  const Value: string): TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam(Add('type', Value));
end;

{ TWebFetchToolResultErrorBlockParam }

function TWebFetchToolResultErrorBlockParam.ErrorCode(
  const Value: string): TWebFetchToolResultErrorBlockParam;
begin
  Result := TWebFetchToolResultErrorBlockParam(Add('error_code', Value));
end;

function TWebFetchToolResultErrorBlockParam.ErrorCode(
  const Value: TWebSearchError): TWebFetchToolResultErrorBlockParam;
begin
  Result := TWebFetchToolResultErrorBlockParam(Add('error_code', Value.ToString));
end;

class function TWebFetchToolResultErrorBlockParam.New: TWebFetchToolResultErrorBlockParam;
begin
  Result := TWebFetchToolResultErrorBlockParam.Create.&Type();
end;

function TWebFetchToolResultErrorBlockParam.&Type(
  const Value: string): TWebFetchToolResultErrorBlockParam;
begin
  Result := TWebFetchToolResultErrorBlockParam(Add('type', Value));
end;

{ TWebFetchBlockParam }

function TWebFetchBlockParam.Content(
  const Value: TDocumentBlockParam): TWebFetchBlockParam;
begin
  Result := TWebFetchBlockParam(Add('content', Value.Detach));
end;

class function TWebFetchBlockParam.New: TWebFetchBlockParam;
begin
  Result := TWebFetchBlockParam.Create.&Type();
end;

function TWebFetchBlockParam.RetrievedAt(
  const Value: string): TWebFetchBlockParam;
begin
  Result := TWebFetchBlockParam(Add('retrieved_at', Value));
end;

function TWebFetchBlockParam.&Type(const Value: string): TWebFetchBlockParam;
begin
  Result := TWebFetchBlockParam(Add('type', Value));
end;

function TWebFetchBlockParam.Url(const Value: string): TWebFetchBlockParam;
begin
  Result := TWebFetchBlockParam(Add('url', Value));
end;

{ TCodeExecutionToolResultBlockParam }

function TCodeExecutionToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TCodeExecutionToolResultBlockParam.Content(
  const Value: TCodeExecutionToolResultErrorParam): TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

function TCodeExecutionToolResultBlockParam.Content(
  const Value: TCodeExecutionResultBlockParam): TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

class function TCodeExecutionToolResultBlockParam.New: TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam.Create.&Type();
end;

function TCodeExecutionToolResultBlockParam.ToolUseId(
  const Value: string): TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam(Add('tool_use_id', Value));
end;

function TCodeExecutionToolResultBlockParam.&Type(
  const Value: string): TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam(Add('type', Value));
end;

{ TCodeExecutionToolResultErrorParam }

function TCodeExecutionToolResultErrorParam.ErrorCode(
  const Value: TWebSearchError): TCodeExecutionToolResultErrorParam;
begin
  Result := TCodeExecutionToolResultErrorParam(Add('error_code', Value.ToString));
end;

function TCodeExecutionToolResultErrorParam.ErrorCode(
  const Value: string): TCodeExecutionToolResultErrorParam;
begin
  Result := TCodeExecutionToolResultErrorParam(Add('error_code', Value));
end;

class function TCodeExecutionToolResultErrorParam.New: TCodeExecutionToolResultErrorParam;
begin
  Result := TCodeExecutionToolResultErrorParam.Create.&Type();
end;

function TCodeExecutionToolResultErrorParam.&Type(
  const Value: string): TCodeExecutionToolResultErrorParam;
begin
  Result := TCodeExecutionToolResultErrorParam(Add('type', Value));
end;

{ TCodeExecutionResultBlockParam }

function TCodeExecutionResultBlockParam.Content(
  const Value: TArray<TCodeExecutionOutputBlockParam>): TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TCodeExecutionOutputBlockParam>(Value)));
end;

class function TCodeExecutionResultBlockParam.New: TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam.Create.&Type();
end;

function TCodeExecutionResultBlockParam.ReturnCode(
  const Value: Integer): TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam(Add('return_code', Value));
end;

function TCodeExecutionResultBlockParam.Stderr(
  const Value: string): TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam(Add('stderr', Value));
end;

function TCodeExecutionResultBlockParam.Stdout(
  const Value: string): TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam(Add('stdout', Value));
end;

function TCodeExecutionResultBlockParam.&Type(
  const Value: string): TCodeExecutionResultBlockParam;
begin
  Result := TCodeExecutionResultBlockParam(Add('type', Value));
end;

{ TCodeExecutionOutputBlockParam }

function TCodeExecutionOutputBlockParam.FileId(
  const Value: string): TCodeExecutionOutputBlockParam;
begin
  Result := TCodeExecutionOutputBlockParam(Add('file_id', Value));
end;

class function TCodeExecutionOutputBlockParam.New: TCodeExecutionOutputBlockParam;
begin
  Result := TCodeExecutionOutputBlockParam.Create.&Type();
end;

function TCodeExecutionOutputBlockParam.&Type(
  const Value: string): TCodeExecutionOutputBlockParam;
begin
  Result := TCodeExecutionOutputBlockParam(Add('type', Value));
end;

{ TBashCodeExecutionToolResultBlockParam }

function TBashCodeExecutionToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TBashCodeExecutionToolResultBlockParam.Content(
  const Value: TBashCodeExecutionToolResultErrorPara): TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

function TBashCodeExecutionToolResultBlockParam.Content(
  const Value: TBashCodeExecutionResultBlockParam): TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

class function TBashCodeExecutionToolResultBlockParam.New: TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam.Create.&Type();
end;

function TBashCodeExecutionToolResultBlockParam.ToolUseId(
  const Value: string): TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam(Add('tool_use_id', Value));
end;

function TBashCodeExecutionToolResultBlockParam.&Type(
  const Value: string): TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam(Add('type', Value));
end;

{ TBashCodeExecutionToolResultErrorPara }

function TBashCodeExecutionToolResultErrorPara.ErrorCode(
  const Value: TWebSearchError): TBashCodeExecutionToolResultErrorPara;
begin
  Result := TBashCodeExecutionToolResultErrorPara(Add('error_code', Value.ToString));
end;

function TBashCodeExecutionToolResultErrorPara.ErrorCode(
  const Value: string): TBashCodeExecutionToolResultErrorPara;
begin
  Result := TBashCodeExecutionToolResultErrorPara(Add('error_code', Value));
end;

class function TBashCodeExecutionToolResultErrorPara.New: TBashCodeExecutionToolResultErrorPara;
begin
  Result := TBashCodeExecutionToolResultErrorPara.Create.&Type();
end;

function TBashCodeExecutionToolResultErrorPara.&Type(
  const Value: string): TBashCodeExecutionToolResultErrorPara;
begin
  Result := TBashCodeExecutionToolResultErrorPara(Add('type', Value));
end;

{ TBashCodeExecutionResultBlockParam }

function TBashCodeExecutionResultBlockParam.Content(
  const Value: TArray<TBashCodeExecutionOutputBlockParam>): TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TBashCodeExecutionOutputBlockParam>(Value)));
end;

class function TBashCodeExecutionResultBlockParam.New: TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam.Create.&Type();
end;

function TBashCodeExecutionResultBlockParam.ReturnCode(
  const Value: Integer): TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam(Add('return_code', Value));
end;

function TBashCodeExecutionResultBlockParam.Stderr(
  const Value: string): TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam(Add('stderr', Value));
end;

function TBashCodeExecutionResultBlockParam.Stdout(
  const Value: string): TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam(Add('stdout', Value));
end;

function TBashCodeExecutionResultBlockParam.&Type(
  const Value: string): TBashCodeExecutionResultBlockParam;
begin
  Result := TBashCodeExecutionResultBlockParam(Add('type', Value));
end;

{ TBashCodeExecutionOutputBlockParam }

function TBashCodeExecutionOutputBlockParam.FileId(
  const Value: string): TBashCodeExecutionOutputBlockParam;
begin
  Result := TBashCodeExecutionOutputBlockParam(Add('file_id', Value));
end;

class function TBashCodeExecutionOutputBlockParam.New: TBashCodeExecutionOutputBlockParam;
begin
  Result := TBashCodeExecutionOutputBlockParam.Create.&Type();
end;

function TBashCodeExecutionOutputBlockParam.&Type(
  const Value: string): TBashCodeExecutionOutputBlockParam;
begin
  Result := TBashCodeExecutionOutputBlockParam(Add('type', Value));
end;

{ TTextEditorCodeExecutionToolResultBlockParam }

function TTextEditorCodeExecutionToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TTextEditorCodeExecutionToolResultBlockParam.Content(
  const Value: TTextEditorCodeExecutionToolResultErrorParam): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

function TTextEditorCodeExecutionToolResultBlockParam.Content(
  const Value: TTextEditorCodeExecutionViewResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

function TTextEditorCodeExecutionToolResultBlockParam.Content(
  const Value: TTextEditorCodeExecutionCreateResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

function TTextEditorCodeExecutionToolResultBlockParam.Content(
  const Value: TTextEditorCodeExecutionStrReplaceResultBlockParam): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('content', Value.Detach));
end;

class function TTextEditorCodeExecutionToolResultBlockParam.New: TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam.Create.&Type();
end;

function TTextEditorCodeExecutionToolResultBlockParam.ToolUseId(
  const Value: string): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('tool_use_id', Value));
end;

function TTextEditorCodeExecutionToolResultBlockParam.&Type(
  const Value: string): TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam(Add('type', Value));
end;

{ TTextEditorCodeExecutionToolResultErrorParam }

function TTextEditorCodeExecutionToolResultErrorParam.ErrorCode(
  const Value: TWebSearchError): TTextEditorCodeExecutionToolResultErrorParam;
begin
  Result := TTextEditorCodeExecutionToolResultErrorParam(Add('error_code', Value.ToString));
end;

function TTextEditorCodeExecutionToolResultErrorParam.ErrorCode(
  const Value: string): TTextEditorCodeExecutionToolResultErrorParam;
begin
  Result := TTextEditorCodeExecutionToolResultErrorParam(Add('error_code', Value));
end;

function TTextEditorCodeExecutionToolResultErrorParam.ErrorMessage(
  const Value: string): TTextEditorCodeExecutionToolResultErrorParam;
begin
  Result := TTextEditorCodeExecutionToolResultErrorParam(Add('error_message', Value));
end;

class function TTextEditorCodeExecutionToolResultErrorParam.New: TTextEditorCodeExecutionToolResultErrorParam;
begin
  Result := TTextEditorCodeExecutionToolResultErrorParam.Create.&Type();
end;

function TTextEditorCodeExecutionToolResultErrorParam.&Type(
  const Value: string): TTextEditorCodeExecutionToolResultErrorParam;
begin
  Result := TTextEditorCodeExecutionToolResultErrorParam(Add('type', Value));
end;

{ TTextEditorCodeExecutionViewResultBlockParam }

function TTextEditorCodeExecutionViewResultBlockParam.Content(
  const Value: string): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('content', Value));
end;

function TTextEditorCodeExecutionViewResultBlockParam.FileType(
  const Value: string): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('file_type', Value));
end;

function TTextEditorCodeExecutionViewResultBlockParam.FileType(
  const Value: TFileType): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('file_type', Value.ToString));
end;

class function TTextEditorCodeExecutionViewResultBlockParam.New: TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam.Create.&Type();
end;

function TTextEditorCodeExecutionViewResultBlockParam.NumLines(
  const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('num_lines', Value));
end;

function TTextEditorCodeExecutionViewResultBlockParam.StartLine(
  const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('start_line', Value));
end;

function TTextEditorCodeExecutionViewResultBlockParam.TotalLines(
  const Value: Integer): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('total_lines', Value));
end;

function TTextEditorCodeExecutionViewResultBlockParam.&Type(
  const Value: string): TTextEditorCodeExecutionViewResultBlockParam;
begin
  Result := TTextEditorCodeExecutionViewResultBlockParam(Add('type', Value));
end;

{ TTextEditorCodeExecutionCreateResultBlockParam }

function TTextEditorCodeExecutionCreateResultBlockParam.IsFileUpdate(
  const Value: Boolean): TTextEditorCodeExecutionCreateResultBlockParam;
begin
  Result := TTextEditorCodeExecutionCreateResultBlockParam(Add('is_file_update', Value));
end;

class function TTextEditorCodeExecutionCreateResultBlockParam.New: TTextEditorCodeExecutionCreateResultBlockParam;
begin
  Result := TTextEditorCodeExecutionCreateResultBlockParam.Create.&Type();
end;

function TTextEditorCodeExecutionCreateResultBlockParam.&Type(
  const Value: string): TTextEditorCodeExecutionCreateResultBlockParam;
begin
  Result := TTextEditorCodeExecutionCreateResultBlockParam(Add('type', Value));
end;

{ TTextEditorCodeExecutionStrReplaceResultBlockParam }

function TTextEditorCodeExecutionStrReplaceResultBlockParam.Lines(
  const Value: TArray<Integer>): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('lines', Value));
end;

class function TTextEditorCodeExecutionStrReplaceResultBlockParam.New: TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam.Create.&Type();
end;

function TTextEditorCodeExecutionStrReplaceResultBlockParam.NewLines(
  const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('new_lines', Value));
end;

function TTextEditorCodeExecutionStrReplaceResultBlockParam.NewStart(
  const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('new_start', Value));
end;

function TTextEditorCodeExecutionStrReplaceResultBlockParam.OldLines(
  const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('old_lines', Value));
end;

function TTextEditorCodeExecutionStrReplaceResultBlockParam.OldStart(
  const Value: Integer): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('old_start', Value));
end;

function TTextEditorCodeExecutionStrReplaceResultBlockParam.&Type(
  const Value: string): TTextEditorCodeExecutionStrReplaceResultBlockParam;
begin
  Result := TTextEditorCodeExecutionStrReplaceResultBlockParam(Add('type', Value));
end;

{ TToolSearchToolResultBlockParam }

function TToolSearchToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam(Add('cache_control', Value));
end;

function TToolSearchToolResultBlockParam.Content(
  const Value: TToolSearchToolResultErrorParam): TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam(Add('content', Value.Detach));
end;

function TToolSearchToolResultBlockParam.Content(
  const Value: TToolSearchToolSearchResultBlockParam): TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam(Add('content', Value.Detach));
end;

class function TToolSearchToolResultBlockParam.New: TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam.Create.&Type();
end;

function TToolSearchToolResultBlockParam.ToolUseId(
  const Value: string): TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam(Add('tool_use_id', Value));
end;

function TToolSearchToolResultBlockParam.&Type(
  const Value: string): TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam(Add('type', Value));
end;

{ TToolSearchToolResultErrorParam }

function TToolSearchToolResultErrorParam.ErrorCode(
  const Value: string): TToolSearchToolResultErrorParam;
begin
  Result := TToolSearchToolResultErrorParam(Add('error_code', Value));
end;

function TToolSearchToolResultErrorParam.ErrorCode(
  const Value: TWebSearchError): TToolSearchToolResultErrorParam;
begin
  Result := TToolSearchToolResultErrorParam(Add('error_code', Value.ToString));
end;

class function TToolSearchToolResultErrorParam.New: TToolSearchToolResultErrorParam;
begin
  Result := TToolSearchToolResultErrorParam.Create.&Type();
end;

function TToolSearchToolResultErrorParam.&Type(
  const Value: string): TToolSearchToolResultErrorParam;
begin
  Result := TToolSearchToolResultErrorParam(Add('type', Value));
end;

{ TToolSearchToolSearchResultBlockParam }

class function TToolSearchToolSearchResultBlockParam.New: TToolSearchToolSearchResultBlockParam;
begin
  Result := TToolSearchToolSearchResultBlockParam.Create.&Type();
end;

function TToolSearchToolSearchResultBlockParam.ToolReferences(
  const Value: TArray<TToolReferenceBlockParam>): TToolSearchToolSearchResultBlockParam;
begin
  Result := TToolSearchToolSearchResultBlockParam(Add('tool_references',
    TJSONHelper.ToJsonArray<TToolReferenceBlockParam>(Value)));
end;

function TToolSearchToolSearchResultBlockParam.&Type(
  const Value: string): TToolSearchToolSearchResultBlockParam;
begin
  Result := TToolSearchToolSearchResultBlockParam(Add('type', Value));
end;

{ TMCPToolUseBlockParam }

function TMCPToolUseBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('cache_control', Value.Detach));
end;

function TMCPToolUseBlockParam.Id(const Value: string): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('id', Value));
end;

function TMCPToolUseBlockParam.Input(
  const Value: TJSONObject): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('input', Value));
end;

function TMCPToolUseBlockParam.Input(
  const Value: string): TMCPToolUseBlockParam;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TMCPToolUseBlockParam(Add('input', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TMCPToolUseBlockParam.Name(const Value: string): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('name', Value));
end;

class function TMCPToolUseBlockParam.New: TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam.Create.&Type();
end;

function TMCPToolUseBlockParam.ServerName(
  const Value: string): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('server_name', Value));
end;

function TMCPToolUseBlockParam.&Type(
  const Value: string): TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam(Add('type', Value));
end;

{ TMCPToolResultBlockParam }

function TMCPToolResultBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('cache_control', Value.Detach));
end;

function TMCPToolResultBlockParam.Content(
  const Value: TArray<TTextBlockParam>): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('content',
    TJSONHelper.ToJsonArray<TTextBlockParam>(Value)));
end;

function TMCPToolResultBlockParam.Content(
  const Value: string): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('content', Value));
end;

function TMCPToolResultBlockParam.IsError(
  const Value: Boolean): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('is_error', Value));
end;

class function TMCPToolResultBlockParam.New: TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam.Create.&Type();
end;

function TMCPToolResultBlockParam.ToolUseId(
  const Value: string): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('tool_use_id', Value));
end;

function TMCPToolResultBlockParam.&Type(
  const Value: string): TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam(Add('type', Value));
end;

{ TContainerUploadBlockParam }

function TContainerUploadBlockParam.CacheControl(
  const Value: TCacheControlEphemeral): TContainerUploadBlockParam;
begin
  Result := TContainerUploadBlockParam(Add('cache_control', Value.Detach));
end;

function TContainerUploadBlockParam.FileId(
  const Value: string): TContainerUploadBlockParam;
begin
  Result := TContainerUploadBlockParam(Add('file_id', Value));
end;

class function TContainerUploadBlockParam.New: TContainerUploadBlockParam;
begin
  Result := TContainerUploadBlockParam.Create.&Type();
end;

function TContainerUploadBlockParam.&Type(
  const Value: string): TContainerUploadBlockParam;
begin
  Result := TContainerUploadBlockParam(Add('type', Value));
end;

{ TSkillParams }

function TSkillParams.&Type(const Value: TSkillType): TSkillParams;
begin
  Result := Self.&Type(Value.ToString);
end;

function TSkillParams.&Type(const Value: string): TSkillParams;
begin
  Result := TSkillParams(Add('type', Value));
end;

function TSkillParams.Version(const Value: string): TSkillParams;
begin
  Result := TSkillParams(Add('version', Value));
end;

class function TSkillParams.New(const SkillType: string): TSkillParams;
begin
  Result := TSkillParams.Create.&Type(SkillType);
end;

class function TSkillParams.New(const SkillType: TSkillType): TSkillParams;
begin
  Result := TSkillParams.Create.&Type(SkillType);
end;

function TSkillParams.SkillId(const Value: string): TSkillParams;
begin
  Result := TSkillParams(Add('skill_id', Value));
end;

{ TContainerParams }

function TContainerParams.Id(const Value: string): TContainerParams;
begin
  Result := TContainerParams(Add('id', Value));
end;

class function TContainerParams.New: TContainerParams;
begin
  Result := TContainerParams.Create;
end;

function TContainerParams.Skills(
  const Value: TArray<TSkillParams>): TContainerParams;
begin
  Result := TContainerParams(Add('skills',
    TJSONHelper.ToJsonArray<TSkillParams>(Value)));
end;

{ TClearToolUses20250919Edit }

function TClearToolUses20250919Edit.ClearAtLeast(
  const Value: TInputTokensClearAtLeast): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('clear_at_least', Value.Detach));
end;

function TClearToolUses20250919Edit.ClearToolInputs(
  const Value: Boolean): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('clear_tool_inputs', Value));
end;

function TClearToolUses20250919Edit.ClearToolInputs(
  const Value: TArray<string>): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('clear_tool_inputs', Value));
end;

function TClearToolUses20250919Edit.ExcludeTools(
  const Value: TArray<string>): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('exclude_tool', Value));
end;

function TClearToolUses20250919Edit.Keep(
  const Value: TToolUsesKeep): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('keep', Value.Detach));
end;

class function TClearToolUses20250919Edit.New: TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit.Create.&Type();
end;

function TClearToolUses20250919Edit.Trigger(
  const Value: TInputTokensTrigger): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('trigger', Value.Detach));
end;

function TClearToolUses20250919Edit.Trigger(
  const Value: TToolUsesTrigger): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('trigger', Value.Detach));
end;

function TClearToolUses20250919Edit.&Type(
  const Value: string): TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit(Add('type', Value));
end;

{ TInputTokensClearAtLeast }

class function TInputTokensClearAtLeast.New: TInputTokensClearAtLeast;
begin
  Result := TInputTokensClearAtLeast.Create.&Type();
end;

function TInputTokensClearAtLeast.&Type(
  const Value: string): TInputTokensClearAtLeast;
begin
  Result := TInputTokensClearAtLeast(Add('type', Value));
end;

function TInputTokensClearAtLeast.Value(
  const Value: Integer): TInputTokensClearAtLeast;
begin
  Result := TInputTokensClearAtLeast(Add('value', Value));
end;

{ TToolUsesKeep }

class function TToolUsesKeep.New: TToolUsesKeep;
begin
  Result := TToolUsesKeep.Create.&Type();
end;

function TToolUsesKeep.&Type(const Value: string): TToolUsesKeep;
begin
  Result := TToolUsesKeep(Add('type', Value));
end;

function TToolUsesKeep.Value(const Value: Integer): TToolUsesKeep;
begin
  Result := TToolUsesKeep(Add('value', Value));
end;

{ TInputTokensTrigger }

class function TInputTokensTrigger.New: TInputTokensTrigger;
begin
  Result := TInputTokensTrigger.Create.&Type();
end;

function TInputTokensTrigger.&Type(const Value: string): TInputTokensTrigger;
begin
  Result := TInputTokensTrigger(Add('type', Value));
end;

function TInputTokensTrigger.Value(const Value: Integer): TInputTokensTrigger;
begin
  Result := TInputTokensTrigger(Add('value', Value));
end;

{ TToolUsesTrigger }

class function TToolUsesTrigger.New: TToolUsesTrigger;
begin
  Result := TToolUsesTrigger.Create.&Type();
end;

function TToolUsesTrigger.&Type(const Value: string): TToolUsesTrigger;
begin
  Result := TToolUsesTrigger(Add('type', Value));
end;

function TToolUsesTrigger.Value(const Value: Integer): TToolUsesTrigger;
begin
  Result := TToolUsesTrigger(Add('value', Value));
end;

{ TClearThinking20251015Edit }

function TClearThinking20251015Edit.Keep(
  const Value: TThinkingTurns): TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit(Add('keep', Value.Detach));
end;

function TClearThinking20251015Edit.Keep(
  const Value: TAllThinkingTurns): TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit(Add('keep', Value.Detach));
end;

function TClearThinking20251015Edit.Keep(
  const Value: string): TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit(Add('keep', Value));
end;

class function TClearThinking20251015Edit.New: TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit.Create.&Type();
end;

function TClearThinking20251015Edit.&Type(
  const Value: string): TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit(Add('type', Value));
end;

{ TThinkingTurns }

class function TThinkingTurns.New: TThinkingTurns;
begin
  Result := TThinkingTurns.Create.&Type();
end;

function TThinkingTurns.&Type(const Value: string): TThinkingTurns;
begin
  Result := TThinkingTurns(Add('type', Value));
end;

function TThinkingTurns.Value(const Value: Integer): TThinkingTurns;
begin
  Result := TThinkingTurns(Add('value', Value));
end;

{ TAllThinkingTurns }

class function TAllThinkingTurns.New: TAllThinkingTurns;
begin
  Result := TAllThinkingTurns.Create.&Type();
end;

function TAllThinkingTurns.&Type(const Value: string): TAllThinkingTurns;
begin
  Result := TAllThinkingTurns(Add('type', Value));
end;

{ TContextManagementConfig }

function TContextManagementConfig.Edits(
  const Value: TArray<TEditsParams>): TContextManagementConfig;
begin
  Result := TContextManagementConfig(Add('edits',
    TJSONHelper.ToJsonArray<TEditsParams>(Value)));
end;

{ TRequestMCPServerToolConfiguration }

function TRequestMCPServerToolConfiguration.AllowedTools(
  const Value: TArray<string>): TRequestMCPServerToolConfiguration;
begin
  Result := TRequestMCPServerToolConfiguration(Add('allowed_tools', Value));
end;

function TRequestMCPServerToolConfiguration.Enabled(
  const Value: Boolean): TRequestMCPServerToolConfiguration;
begin
  Result := TRequestMCPServerToolConfiguration(Add('enabled', Value));
end;

class function TRequestMCPServerToolConfiguration.New: TRequestMCPServerToolConfiguration;
begin
  Result := TRequestMCPServerToolConfiguration.Create;
end;

{ TRequestMCPServerURLDefinition }

function TRequestMCPServerURLDefinition.AuthorizationToken(
  const Value: string): TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition(Add('authorization_token', Value));
end;

function TRequestMCPServerURLDefinition.Name(
  const Value: string): TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition(Add('name', Value));
end;

class function TRequestMCPServerURLDefinition.New: TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition.Create.&Type();
end;

function TRequestMCPServerURLDefinition.ToolConfiguration(
  const Value: TRequestMCPServerToolConfiguration): TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition(Add('tool_configuration', Value.Detach));
end;

function TRequestMCPServerURLDefinition.&Type(
  const Value: string): TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition(Add('type', Value));
end;

function TRequestMCPServerURLDefinition.Url(
  const Value: string): TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition(Add('url', Value));
end;

{ TOutputConfig }

function TOutputConfig.Effort(const Value: TEffortType): TOutputConfig;
begin
  Result := Self.Effort(Value.ToString);
end;

function TOutputConfig.Effort(const Value: string): TOutputConfig;
begin
  Result := TOutputConfig(Add('effort', Value));
end;

function TOutputConfig.Format(const Value: string): TOutputConfig;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TOutputConfig(Add('format', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TOutputConfig.Format(const Value: TOutputConfigFormat): TOutputConfig;
begin
  Result := TOutputConfig(Add('format', Value.Detach));
end;

class function TOutputConfig.New: TOutputConfig;
begin
  Result := TOutputConfig.Create;
end;

{ TJSONOutputFormat }

class function TJSONOutputFormat.New: TJSONOutputFormat;
begin
  Result := TJSONOutputFormat.Create.&Type();
end;

function TJSONOutputFormat.Schema(
  const Value: TSchemaParams): TJSONOutputFormat;
begin
  Result := TJSONOutputFormat(Add('schema', Value.Detach));
end;

function TJSONOutputFormat.Schema(const Value: TJSONObject): TJSONOutputFormat;
begin
  Result := TJSONOutputFormat(Add('schema', Value));
end;

function TJSONOutputFormat.Schema(const Value: string): TJSONOutputFormat;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TJSONOutputFormat(Add('schema', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TJSONOutputFormat.&Type(const Value: string): TJSONOutputFormat;
begin
  Result := TJSONOutputFormat(Add('type', Value));
end;

{ TToolBash20241022 }

function TToolBash20241022.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolBash20241022;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolBash20241022(Add('allowed_callers', JSONArray));
end;

function TToolBash20241022.AllowedCallers(
  const Value: TArray<string>): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('allowed_callers', Value));
end;

function TToolBash20241022.CacheControl(
  const Value: TCacheControlEphemeral): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('cache_control', Value.Detach));
end;

function TToolBash20241022.DeferLoading(
  const Value: Boolean): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('defer_loading', Value));
end;

function TToolBash20241022.Name(const Value: string): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('name', Value));
end;

class function TToolBash20241022.New: TToolBash20241022;
begin
  Result := TToolBash20241022.Create.&Type().Name();
end;

function TToolBash20241022.Strict(const Value: Boolean): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('strict', Value));
end;

function TToolBash20241022.&Type(const Value: string): TToolBash20241022;
begin
  Result := TToolBash20241022(Add('type', Value));
end;

{ TCodeExecutionTool20250522 }

function TCodeExecutionTool20250522.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TCodeExecutionTool20250522;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TCodeExecutionTool20250522(Add('allowed_callers', JSONArray));
end;

function TCodeExecutionTool20250522.AllowedCallers(
  const Value: TArray<string>): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('allowed_callers', Value));
end;

function TCodeExecutionTool20250522.CacheControl(
  const Value: TCacheControlEphemeral): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('cache_control', Value.Detach));
end;

function TCodeExecutionTool20250522.DeferLoading(
  const Value: Boolean): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('defer_loading', Value));
end;

function TCodeExecutionTool20250522.Name(
  const Value: string): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('name', Value));
end;

class function TCodeExecutionTool20250522.New: TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522.Create.&Type();
end;

function TCodeExecutionTool20250522.Strict(
  const Value: Boolean): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('strict', Value));
end;

function TCodeExecutionTool20250522.&Type(
  const Value: string): TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522(Add('type', Value));
end;

{ TCodeExecutionTool20250825 }

function TCodeExecutionTool20250825.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TCodeExecutionTool20250825;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TCodeExecutionTool20250825(Add('allowed_callers', JSONArray));
end;

function TCodeExecutionTool20250825.AllowedCallers(
  const Value: TArray<string>): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('allowed_callers', Value));
end;

function TCodeExecutionTool20250825.CacheControl(
  const Value: TCacheControlEphemeral): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('cache_control', Value.Detach));
end;

function TCodeExecutionTool20250825.DeferLoading(
  const Value: Boolean): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('defer_loading', Value));
end;

function TCodeExecutionTool20250825.Name(
  const Value: string): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('name', Value));
end;

class function TCodeExecutionTool20250825.New: TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825.Create.&Type().Name();
end;

function TCodeExecutionTool20250825.Strict(
  const Value: Boolean): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('strict', Value));
end;

function TCodeExecutionTool20250825.&Type(
  const Value: string): TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825(Add('type', Value));
end;

{ TToolComputerUse20241022 }

function TToolComputerUse20241022.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolComputerUse20241022;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolComputerUse20241022(Add('allowed_callers', JSONArray));
end;

function TToolComputerUse20241022.AllowedCallers(
  const Value: TArray<string>): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('allowed_callers', Value));
end;

function TToolComputerUse20241022.CacheControl(
  const Value: TCacheControlEphemeral): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('cache_control', Value.Detach));
end;

function TToolComputerUse20241022.DeferLoading(
  const Value: Boolean): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('defer_loading', Value));
end;

function TToolComputerUse20241022.DisplayHeightPx(
  const Value: Integer): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('display_height_px', Value));
end;

function TToolComputerUse20241022.DisplayNumber(
  const Value: Integer): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('display_number', Value));
end;

function TToolComputerUse20241022.DisplayWidthPx(
  const Value: Integer): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('display_width_px', Value));
end;

function TToolComputerUse20241022.Name(
  const Value: string): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('name', Value));
end;

class function TToolComputerUse20241022.New: TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022.Create.&Type().Name();
end;

function TToolComputerUse20241022.Strict(
  const Value: Boolean): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('strict', Value));
end;

function TToolComputerUse20241022.&Type(
  const Value: string): TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022(Add('type', Value));
end;

{ TMemoryTool20250818 }

function TMemoryTool20250818.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TMemoryTool20250818;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TMemoryTool20250818(Add('allowed_callers', JSONArray));
end;

function TMemoryTool20250818.AllowedCallers(
  const Value: TArray<string>): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('allowed_callers', Value));
end;

function TMemoryTool20250818.CacheControl(
  const Value: TCacheControlEphemeral): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('cache_control', Value.Detach));
end;

function TMemoryTool20250818.DeferLoading(
  const Value: Boolean): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('defer_loading', Value));
end;

function TMemoryTool20250818.Name(const Value: string): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('name', Value));
end;

class function TMemoryTool20250818.New: TMemoryTool20250818;
begin
  Result := TMemoryTool20250818.Create.&Type().Name();
end;

function TMemoryTool20250818.Strict(const Value: Boolean): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('strict', Value));
end;

function TMemoryTool20250818.&Type(const Value: string): TMemoryTool20250818;
begin
  Result := TMemoryTool20250818(Add('type', Value));
end;

{ TToolComputerUse20250124 }

function TToolComputerUse20250124.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolComputerUse20250124;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolComputerUse20250124(Add('allowed_callers', JSONArray));
end;

function TToolComputerUse20250124.AllowedCallers(
  const Value: TArray<string>): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('allowed_callers', Value));
end;

function TToolComputerUse20250124.CacheControl(
  const Value: TCacheControlEphemeral): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('cache_control', Value.Detach));
end;

function TToolComputerUse20250124.DeferLoading(
  const Value: Boolean): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('defer_loading', Value));
end;

function TToolComputerUse20250124.DisplayHeightPx(
  const Value: Integer): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('display_height_px', Value));
end;

function TToolComputerUse20250124.DisplayNumber(
  const Value: Integer): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('display_number', Value));
end;

function TToolComputerUse20250124.DisplayWidthPx(
  const Value: Integer): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('display_width_px', Value));
end;

function TToolComputerUse20250124.Name(
  const Value: string): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('name', Value));
end;

class function TToolComputerUse20250124.New: TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124.Create.&Type().Name();
end;

function TToolComputerUse20250124.Strict(
  const Value: Boolean): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('strict', Value));
end;

function TToolComputerUse20250124.&Type(
  const Value: string): TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124(Add('type', Value));
end;

{ TToolTextEditor20241022 }

function TToolTextEditor20241022.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolTextEditor20241022;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolTextEditor20241022(Add('allowed_callers', JSONArray));
end;

function TToolTextEditor20241022.AllowedCallers(
  const Value: TArray<string>): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('allowed_callers', Value));
end;

function TToolTextEditor20241022.CacheControl(
  const Value: TCacheControlEphemeral): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('cache_control', Value.Detach));
end;

function TToolTextEditor20241022.DeferLoading(
  const Value: Boolean): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('defer_loading', Value));
end;

function TToolTextEditor20241022.Name(
  const Value: string): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('name', Value));
end;

class function TToolTextEditor20241022.New: TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022.Create.&Type().Name();
end;

function TToolTextEditor20241022.Strict(
  const Value: Boolean): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('strict', Value));
end;

function TToolTextEditor20241022.&Type(
  const Value: string): TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022(Add('type', Value));
end;

{ TToolComputerUse20251124 }

function TToolComputerUse20251124.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolComputerUse20251124;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolComputerUse20251124(Add('allowed_callers', JSONArray));
end;

function TToolComputerUse20251124.AllowedCallers(
  const Value: TArray<string>): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('allowed_callers', Value));
end;

function TToolComputerUse20251124.CacheControl(
  const Value: TCacheControlEphemeral): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('cache_control', Value.Detach));
end;

function TToolComputerUse20251124.DeferLoading(
  const Value: Boolean): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('defer_loading', Value));
end;

function TToolComputerUse20251124.DisplayHeightPx(
  const Value: Integer): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('display_height_px', Value));
end;

function TToolComputerUse20251124.DisplayNumber(
  const Value: Integer): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('display_number', Value));
end;

function TToolComputerUse20251124.DisplayWidthPx(
  const Value: Integer): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('display_width_px', Value));
end;

function TToolComputerUse20251124.Name(
  const Value: string): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('name', Value));
end;

class function TToolComputerUse20251124.New: TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124.Create.&Type().Name();
end;

function TToolComputerUse20251124.Strict(
  const Value: Boolean): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('strict', Value));
end;

function TToolComputerUse20251124.&Type(
  const Value: string): TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124(Add('type', Value));
end;

{ TWebFetchTool20250910 }

function TWebFetchTool20250910.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TWebFetchTool20250910;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TWebFetchTool20250910(Add('allowed_callers', JSONArray));
end;

function TWebFetchTool20250910.AllowedCallers(
  const Value: TArray<string>): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('allowed_callers', Value));
end;

function TWebFetchTool20250910.AllowedDomains(
  const Value: TArray<string>): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('allowed_domains', Value));
end;

function TWebFetchTool20250910.BlockedDomains(
  const Value: TArray<string>): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('blocked_domains', Value));
end;

function TWebFetchTool20250910.CacheControl(
  const Value: TCacheControlEphemeral): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('cache_control', Value).Detach);
end;

function TWebFetchTool20250910.DeferLoading(
  const Value: Boolean): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('defer_loading', Value));
end;

function TWebFetchTool20250910.MaxUses(
  const Value: Integer): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('max_uses', Value));
end;

function TWebFetchTool20250910.Name(const Value: string): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('name', Value));
end;

class function TWebFetchTool20250910.New: TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910.Create.&Type().Name();
end;

function TWebFetchTool20250910.Strict(
  const Value: Boolean): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('strict', Value));
end;

function TWebFetchTool20250910.&Type(
  const Value: string): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('type', Value));
end;

function TWebFetchTool20250910.UserLocation(
  const Value: TUserLocation): TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910(Add('user_location', Value.Detach));
end;

{ TToolSearchToolBm25_20251119 }

function TToolSearchToolBm25_20251119.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolSearchToolBm25_20251119;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolSearchToolBm25_20251119(Add('allowed_callers', JSONArray));
end;

function TToolSearchToolBm25_20251119.AllowedCallers(
  const Value: TArray<string>): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('allowed_callers', Value));
end;

function TToolSearchToolBm25_20251119.CacheControl(
  const Value: TCacheControlEphemeral): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('cache_control', Value.Detach));
end;

function TToolSearchToolBm25_20251119.DeferLoading(
  const Value: Boolean): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('defer_loading', Value));
end;

function TToolSearchToolBm25_20251119.Name(
  const Value: string): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('name', Value));
end;

class function TToolSearchToolBm25_20251119.New: TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119.Create.&Type().Name();
end;

function TToolSearchToolBm25_20251119.Strict(
  const Value: Boolean): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('strict', Value));
end;

function TToolSearchToolBm25_20251119.&Type(
  const Value: string): TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119(Add('type', Value));
end;

{ TToolSearchToolRegex20251119 }

function TToolSearchToolRegex20251119.AllowedCallers(
  const Value: TArray<TAllowedCallersType>): TToolSearchToolRegex20251119;
begin
  var JSONArray := TJSONArray.Create;
  for var Item in Value do
    JSONArray.Add(Item.ToString);

  Result := TToolSearchToolRegex20251119(Add('allowed_callers', JSONArray));
end;

function TToolSearchToolRegex20251119.AllowedCallers(
  const Value: TArray<string>): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('allowed_callers', Value));
end;

function TToolSearchToolRegex20251119.CacheControl(
  const Value: TCacheControlEphemeral): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('cache_control', Value.Detach));
end;

function TToolSearchToolRegex20251119.DeferLoading(
  const Value: Boolean): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('defer_loading', Value));
end;

function TToolSearchToolRegex20251119.Name(
  const Value: string): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('name', Value));
end;

class function TToolSearchToolRegex20251119.New: TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119.Create.&Type().Name();
end;

function TToolSearchToolRegex20251119.Strict(
  const Value: Boolean): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('strict', Value));
end;

function TToolSearchToolRegex20251119.&Type(
  const Value: string): TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119(Add('type', Value));
end;

{ TMCPToolset }

function TMCPToolset.CacheControl(
  const Value: TCacheControlEphemeral): TMCPToolset;
begin
  Result := TMCPToolset(Add('cache_control', Value.Detach));
end;

function TMCPToolset.Configs(const Value: TMCPconfigs): TMCPToolset;
begin
  Result := TMCPToolset(Add('configs', Value.Detach));
end;

function TMCPToolset.DefaultConfig(const Value: TMCPconfigs): TMCPToolset;
begin
  Result := TMCPToolset(Add('default_config', Value.Detach));
end;

function TMCPToolset.McpServerName(const Value: string): TMCPToolset;
begin
  Result := TMCPToolset(Add('mcp_server_name', Value));
end;

class function TMCPToolset.New: TMCPToolset;
begin
  Result := TMCPToolset.Create.&Type();
end;

function TMCPToolset.&Type(const Value: string): TMCPToolset;
begin
  Result := TMCPToolset(Add('type', Value));
end;

{ TMCPconfigs }

function TMCPconfigs.DeferLoading(const Value: Boolean): TMCPconfigs;
begin
  Result := TMCPconfigs(Add('defer_loading', Value));
end;

function TMCPconfigs.Enabled(const Value: Boolean): TMCPconfigs;
begin
  Result := TMCPconfigs(Add('enabled', Value));
end;

{ TOutputConfigFormat }

function TOutputConfigFormat.Schema(
  const Value: TJSONObject): TOutputConfigFormat;
begin
  Result := TOutputConfigFormat(Add('schema', Value));
end;

function TOutputConfigFormat.Schema(
  const Value: TSchemaParams): TOutputConfigFormat;
begin
  Result := TOutputConfigFormat(Add('schema', Value.Detach));
end;

class function TOutputConfigFormat.New: TOutputConfigFormat;
begin
  Result := TOutputConfigFormat.Create.&Type();
end;

function TOutputConfigFormat.Schema(const Value: string): TOutputConfigFormat;
var
  JSONObject: TJSONObject;
begin
  if TJSONHelper.TryGetObject(Value, JSONObject) then
    Exit(TOutputConfigFormat(Add('schema', JSONObject)));

  raise EAnthropicException.Create('Invalid JSON Object');
end;

function TOutputConfigFormat.&Type(const Value: string): TOutputConfigFormat;
begin
  Result := TOutputConfigFormat(Add('type', Value));
end;

end.
