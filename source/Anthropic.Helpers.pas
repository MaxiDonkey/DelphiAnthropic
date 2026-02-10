unit Anthropic.Helpers;

{-------------------------------------------------------------------------------

      Github repository :  https://github.com/MaxiDonkey/DelphiAnthropic
      Visit the Github repository for the documentation and use examples

 ------------------------------------------------------------------------------}

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  Anthropic.API.Params, Anthropic.Types, Anthropic.API.ArrayBuilder,
  Anthropic.Chat.Request;

type
  TWebSearchTools = TArrayBuilder<TWebSearchToolResultBlockItem>;

{$REGION 'TextBlocks'}

  TTextBlocks = TArrayBuilder<TTextBlockParam>;

  TTextBlocksHelper = record Helper for TTextBlocks
    function AddText(const Value: TTextBlockParam): TTextBlocks; overload;
    function AddText(const Value: string): TTextBlocks; overload;
  end;

{$ENDREGION}

{$REGION 'Contents'}

  TContents = TArrayBuilder<TContentBlockParam>;

  TContentsHelper = record Helper for TContents
    function AddText(const Value: TTextBlockParam): TContents; overload;
    function AddText(const Value: string): TContents; overload;

    function AddImage(const Base64orUri: string; const MimeType: string = ''): TContents; overload;
    function AddImage(const Value: TImageBlockParam): TContents; overload;

    function AddPDF(const Base64orUri: string): TContents; overload;
    function AddPDF(const Value: TDocumentBlockParam): TContents; overload;

    function AddTextPlain(const Text: string): TContents; overload;
    function AddTextPlain(const Value: TDocumentBlockParam): TContents; overload;

    function AddBlockContent(const Value: string): TContents; overload;
    function AddBlockContent(const Value: TDocumentBlockParam): TContents; overload;

    function AddFileDocument(const FileId: string): TContents; overload;
  end;

{$ENDREGION}

{$REGION 'Messages'}

  TMessages = TArrayBuilder<TMessageParam>;

  TMessagesHelper = record Helper for TMessages
    function User(const Value: string): TMessages; overload;
    function User(const Value: TArray<TContentBlockParam>): TMessages; overload;
    function Assistant(const Value: string): TMessages; overload;
    function Assistant(const Value: TArray<TContentBlockParam>): TMessages; overload;
  end;

{$ENDREGION}

{$REGION 'Cache control'}

  TCacheControlManager = record
    class function AddCacheControl: TCacheControlEphemeral; overload; static;
    class function AddCacheControl(const ttl: string): TCacheControlEphemeral; overload; static;
  end;

{$ENDREGION}

{$REGION 'Citations'}

  TCitations = TArrayBuilder<TTextCitationParam>;

  TCitationManager = record
    class function AddCharLocation(const CitedText: string): TCitationCharLocationParam; static;
    class function AddPageLocation(const CitedText: string): TCitationPageLocationParam; static;
    class function AddContentBlockLocation(const CitedText: string): TCitationContentBlockLocationParam; static;
    class function AddWebSearchResultLocation(const CitedText: string): TCitationWebSearchResultLocationParam; static;
    class function AddSearchResultLocation(const CitedText: string): TCitationSearchResultLocationParam; static;
  end;

{$ENDREGION}

{$REGION 'Context'}

  TContextBetaManager = record
  private
    class function Empty: TContextBetaManager; static; inline;
  public
    class function CreateToolReference: TToolReferenceBlockParam; static;
    class function CreateWebFetchToolResult: TWebFetchToolResultBlockParam; static;
    class function CreateCodeExecutionToolResult: TCodeExecutionToolResultBlockParam; static;
    class function CreateBashCodeExecutionToolResult: TBashCodeExecutionToolResultBlockParam; static;
    class function CreateTextEditorCodeExecutionToolResult: TTextEditorCodeExecutionToolResultBlockParam; static;
    class function CreateToolSearchToolResult: TToolSearchToolResultBlockParam; static;
    class function CreateMCPToolUse: TMCPToolUseBlockParam; static;
    class function CreateMCPToolResult: TMCPToolResultBlockParam; static;
    class function CreateContainerUpload: TContainerUploadBlockParam; static;
    class function CreateContainer: TContainerParams; static;
    class function CreateSkill(const Value: TSkillType): TSkillParams; overload; static;
    class function CreateSkill(const Value: string): TSkillParams; overload; static;
  end;

  TContextManager = record
  private
    class var FContextBeta: TContextBetaManager;
    class var FCitation: TCitationManager;
    class function Empty: TContextManager; static; inline;
  public
    class function CreateSearchResult: TSearchResultBlockParam; static;
    class function CreateThinking: TThinkingBlockParam; static;
    class function CreateRedactedThinking: TRedactedThinkingBlockParam; static;
    class function CreateToolUse: TToolUseBlockParam; static;
    class function CreateToolResult: TToolResultBlockParam; static;
    class function CreateServerToolUse: TServerToolUseBlockParam; static;
    class function CreateWebSearchToolResult: TWebSearchToolResultBlockParam; static;
    class function CreateWebSearchToolResultItem: TWebSearchToolResultBlockItem; static;
    class function CreateWebSearchToolRequestError: TWebSearchToolRequestError; static;
    class property Beta: TContextBetaManager read FContextBeta;
    class property Citation: TCitationManager read FCitation;
  end;

{$ENDREGION}

{$REGION 'Tool choice'}

  TToolChoiceManager = record
  private
    class function Empty: TToolChoiceManager; static; inline;
  public
    class function Auto: TToolChoice; static;
    class function Any: TToolChoice; static;
    class function None: TToolChoice; static;
    class function Tool: TToolChoice; static;
  end;

{$ENDREGION}

{$REGION 'Tools'}

  TTools = TArrayBuilder<TToolUnion>;

  TToolBetaManager = record
  private
    class function Empty: TToolBetaManager; static; inline;
  public
    class function CreateCodeExecutionTool20250825: TCodeExecutionTool20250825; static;
    class function CreateCodeExecutionTool20250522: TCodeExecutionTool20250522; static;

    class function CreateMCPToolset: TMCPToolset; static;

    class function CreateMemoryTool20250818: TMemoryTool20250818; static;

    class function CreateToolBash20241022: TToolBash20241022; static;

    class function CreateToolComputerUse20251124: TToolComputerUse20251124; static;
    class function CreateToolComputerUse20250124: TToolComputerUse20250124; static;
    class function CreateToolComputerUse20241022: TToolComputerUse20241022; static;

    class function CreateToolSearchToolBm25_20251119: TToolSearchToolBm25_20251119; static;

    class function CreateToolSearchToolRegex20251119: TToolSearchToolRegex20251119; static;

    class function CreateToolTextEditor20241022: TToolTextEditor20241022; static;

    class function CreateWebFetchTool20250910: TWebFetchTool20250910; static;
  end;

  TToolManager = record
  private
    class var FToolBeta: TToolBetaManager;
    class function Empty: TToolManager; static; inline;
  public
    class function CreateToolCustom: TTool; static;
    class function CreateToolBash20250124: TToolBash20250124; static;
    class function CreateToolTextEditor20250728: TToolTextEditor20250728; static;
    class function CreateToolTextEditor20250429: TToolTextEditor20250429; static;
    class function CreateToolTextEditor20250124: TToolTextEditor20250124; static;
    class function CreateWebSearchTool20250305: TWebSearchTool20250305; static;
    class property Beta: TToolBetaManager read FToolBeta;
  end;

{$ENDREGION}

{$REGION 'Skill'}

  TSkills = TArrayBuilder<TSkillParams>;

  TSkillManager = record
  private
    class function Empty: TSkillManager; static; inline;
  public
    class function CreateSkill(const SkillType: TSkillType): TSkillParams; overload; static;
    class function CreateSkill(const SkillType: string): TSkillParams; overload; static;
  end;

{$ENDREGION}

{$REGION 'Edit'}

  TContextEdits = TArrayBuilder<TEditsParams>;

  TEditManager = record
  private
    class function Empty: TEditManager; static; inline;
  public
    class function CreateClearToolUses20250919Edit: TClearToolUses20250919Edit; static;
    class function CreateClearThinking20251015Edit: TClearThinking20251015Edit; static;

    /// <summary>
    /// Minimum number of tokens that must be cleared when triggered. Context will only be modified
    /// if at least this many tokens can be removed.
    /// </summary>
    class function InputTokensClearAtLeast(const Value: Integer): TInputTokensClearAtLeast; static;

    /// <summary>
    /// Number of tool uses to retain in the conversation
    /// </summary>
    class function ToolUsesKeep(const Value: Integer): TToolUsesKeep; static;

    /// <summary>
    /// Number of tool uses to retain in the conversation
    /// </summary>
    class function InputTokensTrigger(const Value: Integer): TInputTokensTrigger; static;

    /// <summary>
    /// Condition that triggers the context management strategy
    /// </summary>
    class function ToolUsesTrigger(const Value: Integer): TToolUsesTrigger; static;

    /// <summary>
    /// Number of most recent assistant turns to keep thinking blocks for. Older turns will have
    /// their thinking blocks removed.
    /// </summary>
    class function ThinkingTurns(const Value: Integer): TThinkingTurns; static;

    class function AllThinkingTurns: TAllThinkingTurns; static;
  end;

{$ENDREGION}

{$REGION 'MCP Server'}

  TMCPServerURLDefinitions = TArrayBuilder<TRequestMCPServerURLDefinition>;

  TMCPServerManager = record
  private
    class function Empty: TMCPServerManager; static; inline;
  public
    class function CreateMCPServer: TRequestMCPServerURLDefinition; static;
    class function CreateMCPServerToolConfiguration: TRequestMCPServerToolConfiguration; static;
  end;

{$ENDREGION}

{$REGION 'Content & Payload'}

  TContentManager = record
    class function AddText(const Text: string): TTextBlockParam; static;
    class function AddImage(const Base64orUri: string; const MimeType: string = ''): TImageBlockParam; static;
    class function AddPDF(const Base64orUri: string): TDocumentBlockParam; static;
    class function AddTextPlain(const Text: string): TDocumentBlockParam; static;
    class function AddBlockContent(const Value: string): TDocumentBlockParam; overload; static;
    class function AddBlockContent(const Value: TDocumentBlockParam): TDocumentBlockParam; overload; static;
    class function AddFileDocument(const FileId: string): TDocumentBlockParam; static;
  end;

  TPayload = record
    class function Assistant(const Value: string): TMessageParam; overload; static;
    class function Assistant(const Value: TArray<TContentBlockParam>): TMessageParam; overload; static;
    class function User(const Value: string): TMessageParam; overload; static;
    class function User(const Value: TArray<TContentBlockParam>): TMessageParam; overload; static;
  end;

{$ENDREGION}

  TGenerationManager = record
  private
    class var FCacheControlManager: TCacheControlManager;
    class var FContentManager: TContentManager;
    class var FContextManager: TContextManager;
    class var FDocument: TDocument;
    class var FEditManager: TEditManager;
    class var FMCPServerManager: TMCPServerManager;
    class var FToolChoiceManager: TToolChoiceManager;
    class var FSkillManager: TSkillManager;
    class var FToolManager: TToolManager;

    class function Empty: TGenerationManager; static; inline;
  public
    class function CitationParts: TCitations; static;
    class function ContentParts: TContents; static;
    class function EditParts: TContextEdits; static;
    class function MessageParts: TMessages; static;
    class function MCPServerParts: TMCPServerURLDefinitions; static;
    class function SkillParts: TSkills; static;
    class function TextBlockParts: TTextBlocks; static;
    class function ToolParts: TTools; static;
    class function WebSearchToolParts: TWebSearchTools; static;

    class function CreateCitationsConfig: TCitationsConfigParam; static;
    class function CreateContextManagement: TContextManagementConfig; static;
    class function CreateContainer: TContainerParams; static;
    class function CreateDocumentBlock: TDocumentBlockParam; static;
    class function CreateFormat: TOutputConfigFormat; static;
    class function CreateImageBlock: TImageBlockParam; static;
    class function CreateOutputConfig: TOutputConfig; static;
    class function CreateTextBlock: TTextBlockParam; static;
    class function CreateThinkingConfig(const State: TThinkingType): TThinkingConfigParam; overload; static;
    class function CreateThinkingConfig(const Value: string): TThinkingConfigParam; overload; static;


    class property BlockContent: TContentManager read FContentManager;
    class property Cache: TCacheControlManager read FCacheControlManager;
    class property Context: TContextManager read FContextManager;
    class property Document: TDocument read FDocument;
    class property Edit: TEditManager read FEditManager;
    class property MCPServer: TMCPServerManager read FMCPServerManager;
    class property Skill: TSkillManager read FSkillManager;
    class property Tool: TToolManager read FToolManager;
    class property ToolChoice: TToolChoiceManager read FToolChoiceManager;
  end;

function Generation: TGenerationManager;

implementation

function Generation: TGenerationManager;
begin
  Result := TGenerationManager.Empty;
end;

{ TMessagesHelper }

function TMessagesHelper.Assistant(const Value: string): TMessages;
begin
  Result := Self.Add(TPayload.Assistant(Value));
end;

function TMessagesHelper.Assistant(
  const Value: TArray<TContentBlockParam>): TMessages;
begin
  Result := Self.Add(TPayload.Assistant(Value));
end;

function TMessagesHelper.User(const Value: string): TMessages;
begin
  Result := Self.Add(TPayload.User(Value));
end;

function TMessagesHelper.User(
  const Value: TArray<TContentBlockParam>): TMessages;
begin
  Result := Self.Add(TPayload.User(Value));
end;

{ TContentsHelper }

function TContentsHelper.AddBlockContent(const Value: string): TContents;
begin
  Result := Self.Add( TContentManager.AddBlockContent(Value) );
end;

function TContentsHelper.AddBlockContent(
  const Value: TDocumentBlockParam): TContents;
begin
  Result := Self.Add(Value);
end;

function TContentsHelper.AddFileDocument(const FileId: string): TContents;
begin
  Result := Self.Add( TContentManager.AddFileDocument(FileId) );
end;

function TContentsHelper.AddImage(const Value: TImageBlockParam): TContents;
begin
  Result := Self.Add(Value);
end;

function TContentsHelper.AddImage(const Base64orUri, MimeType: string): TContents;
begin
  Result := Self.Add( TContentManager.AddImage(Base64orUri, Mimetype) );
end;

function TContentsHelper.AddTextPlain(
  const Value: TDocumentBlockParam): TContents;
begin
  Result := Self.Add(Value);
end;

function TContentsHelper.AddTextPlain(const Text: string): TContents;
begin
  Result := Self.Add( TContentManager.AddTextPlain(Text) );
end;

function TContentsHelper.AddPDF(const Value: TDocumentBlockParam): TContents;
begin
  Result := Self.Add(Value);
end;

function TContentsHelper.AddPDF(const Base64orUri: string): TContents;
begin
  Result := Self.Add( TContentManager.AddPDF(Base64orUri) );
end;

function TContentsHelper.AddText(const Value: TTextBlockParam): TContents;
begin
  Result := Self.Add(Value);
end;

function TContentsHelper.AddText(const Value: string): TContents;
begin
  Result := Self.Add( TContentManager.AddText(Value) );
end;

{ TGenerationManager }

class function TGenerationManager.CitationParts: TCitations;
begin
  Result := TCitations.Create();
end;

class function TGenerationManager.MessageParts: TMessages;
begin
  Result := TMessages.Create();
end;

class function TGenerationManager.SkillParts: TSkills;
begin
  Result := TSkills.Create();
end;

class function TGenerationManager.CreateCitationsConfig: TCitationsConfigParam;
begin
  Result := TCitationsConfigParam.Create;
end;

class function TGenerationManager.CreateContainer: TContainerParams;
begin
  Result := TContainerParams.New;
end;

class function TGenerationManager.CreateContextManagement: TContextManagementConfig;
begin
  Result := TContextManagementConfig.Create;
end;

class function TGenerationManager.CreateDocumentBlock: TDocumentBlockParam;
begin
  Result := TDocumentBlockParam.New;
end;

class function TGenerationManager.CreateOutputConfig: TOutputConfig;
begin
  Result := TOutputConfig.New;
end;

class function TGenerationManager.CreateFormat: TOutputConfigFormat;
begin
  Result := TOutputConfigFormat.New;
end;

class function TGenerationManager.CreateTextBlock: TTextBlockParam;
begin
  Result := TTextBlockParam.New;
end;

class function TGenerationManager.CreateThinkingConfig(
  const State: TThinkingType): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam.New(State);
end;

class function TGenerationManager.CreateThinkingConfig(
  const Value: string): TThinkingConfigParam;
begin
  Result := TThinkingConfigParam.New(Value);
end;


class function TGenerationManager.CreateImageBlock: TImageBlockParam;
begin
  Result := TImageBlockParam.New;
end;

class function TGenerationManager.EditParts: TContextEdits;
begin
  Result := TContextEdits.Create();
end;

class function TGenerationManager.Empty: TGenerationManager;
begin
  Result := Default(TGenerationManager);
  FContentManager := Default(TContentManager);
  FCacheControlManager := Default(TCacheControlManager);
  FContextManager := TContextManager.Empty;
  FToolManager := TToolManager.Empty;
  FMCPServerManager := TMCPServerManager.Empty;
  FEditManager := TEditManager.Empty;
  FSkillManager := TSkillManager.Empty;
  FToolChoiceManager := TToolChoiceManager.Empty;
  FDocument := Default(TDocument);
end;

class function TGenerationManager.MCPServerParts: TMCPServerURLDefinitions;
begin
  Result := TMCPServerURLDefinitions.Create();
end;

class function TGenerationManager.ContentParts: TContents;
begin
  Result := TContents.Create();
end;

class function TGenerationManager.TextBlockParts: TTextBlocks;
begin
  Result := TTextBlocks.Create();
end;

class function TGenerationManager.ToolParts: TTools;
begin
  Result := TTools.Create();
end;

class function TGenerationManager.WebSearchToolParts: TWebSearchTools;
begin
  Result := TWebSearchTools.Create();
end;

{ TTextBlocksHelper }

function TTextBlocksHelper.AddText(const Value: TTextBlockParam): TTextBlocks;
begin
  Result := Self.Add(Value);
end;

function TTextBlocksHelper.AddText(const Value: string): TTextBlocks;
begin
  Result := Self.Add( TContentManager.AddText(Value) );
end;

{ TContextManager }

class function TContextManager.CreateRedactedThinking: TRedactedThinkingBlockParam;
begin
  Result := TRedactedThinkingBlockParam.New;
end;

class function TContextManager.CreateSearchResult: TSearchResultBlockParam;
begin
  Result := TSearchResultBlockParam.New;
end;

class function TContextManager.CreateServerToolUse: TServerToolUseBlockParam;
begin
  Result := TServerToolUseBlockParam.New;
end;

class function TContextManager.CreateThinking: TThinkingBlockParam;
begin
  Result := TThinkingBlockParam.New;
end;

class function TContextManager.CreateToolResult: TToolResultBlockParam;
begin
  Result := TToolResultBlockParam.New;
end;

class function TContextManager.CreateToolUse: TToolUseBlockParam;
begin
  Result := TToolUseBlockParam.New;
end;

class function TContextManager.CreateWebSearchToolRequestError: TWebSearchToolRequestError;
begin
  Result := TWebSearchToolRequestError.New;
end;

class function TContextManager.CreateWebSearchToolResult: TWebSearchToolResultBlockParam;
begin
  Result := TWebSearchToolResultBlockParam.New;
end;

class function TContextManager.CreateWebSearchToolResultItem: TWebSearchToolResultBlockItem;
begin
  Result := TWebSearchToolResultBlockItem.New;
end;

class function TContextManager.Empty: TContextManager;
begin
  Result := Default(TContextManager);
  FContextBeta := TContextBetaManager.Empty;
  FCitation := Default(TCitationManager);
end;

{ TContextBetaManager }

class function TContextBetaManager.CreateBashCodeExecutionToolResult: TBashCodeExecutionToolResultBlockParam;
begin
  Result := TBashCodeExecutionToolResultBlockParam.New;
end;

class function TContextBetaManager.CreateCodeExecutionToolResult: TCodeExecutionToolResultBlockParam;
begin
  Result := TCodeExecutionToolResultBlockParam.New;
end;

class function TContextBetaManager.CreateContainer: TContainerParams;
begin
  Result := TContainerParams.New;
end;

class function TContextBetaManager.CreateContainerUpload: TContainerUploadBlockParam;
begin
  Result := TContainerUploadBlockParam.New;
end;

class function TContextBetaManager.CreateMCPToolResult: TMCPToolResultBlockParam;
begin
  Result := TMCPToolResultBlockParam.New;
end;

class function TContextBetaManager.CreateMCPToolUse: TMCPToolUseBlockParam;
begin
  Result := TMCPToolUseBlockParam.New;
end;

class function TContextBetaManager.CreateSkill(const Value: string): TSkillParams;
begin
  Result := TSkillParams.New(Value);
end;

class function TContextBetaManager.CreateSkill(const Value: TSkillType): TSkillParams;
begin
  Result := TSkillParams.New(Value);
end;

class function TContextBetaManager.CreateTextEditorCodeExecutionToolResult: TTextEditorCodeExecutionToolResultBlockParam;
begin
  Result := TTextEditorCodeExecutionToolResultBlockParam.New;
end;

class function TContextBetaManager.CreateToolReference: TToolReferenceBlockParam;
begin
  Result := TToolReferenceBlockParam.New;
end;

class function TContextBetaManager.CreateToolSearchToolResult: TToolSearchToolResultBlockParam;
begin
  Result := TToolSearchToolResultBlockParam.New;
end;

class function TContextBetaManager.CreateWebFetchToolResult: TWebFetchToolResultBlockParam;
begin
  Result := TWebFetchToolResultBlockParam.New;
end;

class function TContextBetaManager.Empty: TContextBetaManager;
begin
  Result := Default(TContextBetaManager);
end;

{ TToolManager }

class function TToolManager.CreateToolCustom: TTool;
begin
  Result := TTool.New;
end;

class function TToolManager.CreateToolBash20250124: TToolBash20250124;
begin
  Result := TToolBash20250124.New;
end;

class function TToolManager.CreateToolTextEditor20250124: TToolTextEditor20250124;
begin
  Result := TToolTextEditor20250124.New;
end;

class function TToolManager.CreateToolTextEditor20250429: TToolTextEditor20250429;
begin
  Result := TToolTextEditor20250429.New;
end;

class function TToolManager.CreateToolTextEditor20250728: TToolTextEditor20250728;
begin
  Result := TToolTextEditor20250728.New;
end;

class function TToolManager.CreateWebSearchTool20250305: TWebSearchTool20250305;
begin
  Result := TWebSearchTool20250305.New;
end;

class function TToolManager.Empty: TToolManager;
begin
  Result := Default(TToolManager);
  FToolBeta := TToolBetaManager.Empty;
end;

{ TToolBetaManager }

class function TToolBetaManager.CreateCodeExecutionTool20250522: TCodeExecutionTool20250522;
begin
  Result := TCodeExecutionTool20250522.New;
end;

class function TToolBetaManager.CreateCodeExecutionTool20250825: TCodeExecutionTool20250825;
begin
  Result := TCodeExecutionTool20250825.New;
end;

class function TToolBetaManager.CreateMCPToolset: TMCPToolset;
begin
  Result := TMCPToolset.New;
end;

class function TToolBetaManager.CreateMemoryTool20250818: TMemoryTool20250818;
begin
  Result := TMemoryTool20250818.New;
end;

class function TToolBetaManager.CreateToolBash20241022: TToolBash20241022;
begin
  Result := TToolBash20241022.New;
end;

class function TToolBetaManager.CreateToolComputerUse20241022: TToolComputerUse20241022;
begin
  Result := TToolComputerUse20241022.New;
end;

class function TToolBetaManager.CreateToolComputerUse20250124: TToolComputerUse20250124;
begin
  Result := TToolComputerUse20250124.New;
end;

class function TToolBetaManager.CreateToolComputerUse20251124: TToolComputerUse20251124;
begin
  Result := TToolComputerUse20251124.New;
end;

class function TToolBetaManager.CreateToolSearchToolBm25_20251119: TToolSearchToolBm25_20251119;
begin
  Result := TToolSearchToolBm25_20251119.New;
end;

class function TToolBetaManager.CreateToolSearchToolRegex20251119: TToolSearchToolRegex20251119;
begin
  Result := TToolSearchToolRegex20251119.New;
end;

class function TToolBetaManager.CreateToolTextEditor20241022: TToolTextEditor20241022;
begin
  Result := TToolTextEditor20241022.New;
end;

class function TToolBetaManager.CreateWebFetchTool20250910: TWebFetchTool20250910;
begin
  Result := TWebFetchTool20250910.New;
end;

class function TToolBetaManager.Empty: TToolBetaManager;
begin
  Result := Default(TToolBetaManager);
end;

{ TMCPServerManager }

class function TMCPServerManager.CreateMCPServer: TRequestMCPServerURLDefinition;
begin
  Result := TRequestMCPServerURLDefinition.New;
end;

class function TMCPServerManager.CreateMCPServerToolConfiguration: TRequestMCPServerToolConfiguration;
begin
  Result := TRequestMCPServerToolConfiguration.New;
end;

class function TMCPServerManager.Empty: TMCPServerManager;
begin
  Result := Default(TMCPServerManager);
end;

{ TEditManager }

class function TEditManager.AllThinkingTurns: TAllThinkingTurns;
begin
  Result := TAllThinkingTurns.New;
end;

class function TEditManager.CreateClearThinking20251015Edit: TClearThinking20251015Edit;
begin
  Result := TClearThinking20251015Edit.New;
end;

class function TEditManager.CreateClearToolUses20250919Edit: TClearToolUses20250919Edit;
begin
  Result := TClearToolUses20250919Edit.New;
end;

class function TEditManager.Empty: TEditManager;
begin
  Result := Default(TEditManager);
end;

class function TEditManager.InputTokensClearAtLeast(
  const Value: Integer): TInputTokensClearAtLeast;
begin
  Result := TInputTokensClearAtLeast.New.Value(Value);
end;

class function TEditManager.InputTokensTrigger(
  const Value: Integer): TInputTokensTrigger;
begin
  Result := TInputTokensTrigger.New.Value(Value);
end;

class function TEditManager.ThinkingTurns(const Value: Integer): TThinkingTurns;
begin
  Result := TThinkingTurns.New.Value(Value);
end;

class function TEditManager.ToolUsesKeep(const Value: Integer): TToolUsesKeep;
begin
  Result := TToolUsesKeep.New.Value(Value);
end;

class function TEditManager.ToolUsesTrigger(
  const Value: Integer): TToolUsesTrigger;
begin
  Result := TToolUsesTrigger.New.Value(Value);
end;

{ TSkillManager }

class function TSkillManager.CreateSkill(const SkillType: TSkillType): TSkillParams;
begin
  Result := TSkillParams.New(SkillType);
end;

class function TSkillManager.CreateSkill(const SkillType: string): TSkillParams;
begin
  Result := TSkillParams.New(SkillType);
end;

class function TSkillManager.Empty: TSkillManager;
begin
  Result := Default(TSkillManager);
end;

{ TToolChoiceManager }

class function TToolChoiceManager.Empty: TToolChoiceManager;
begin
  Result := Default(TToolChoiceManager);
end;

class function TToolChoiceManager.None: TToolChoice;
begin
  Result := TToolChoice.New('none');
end;

class function TToolChoiceManager.Tool: TToolChoice;
begin
  Result := TToolChoice.New('tool');
end;

class function TToolChoiceManager.Any: TToolChoice;
begin
  Result := TToolChoice.New('any');
end;

class function TToolChoiceManager.Auto: TToolChoice;
begin
  Result := TToolChoice.New('auto');
end;

{ TCacheControlManager }

class function TCacheControlManager.AddCacheControl(
  const ttl: string): TCacheControlEphemeral;
begin
  Result := TCacheControlEphemeral.New.Ttl(ttl);
end;

class function TCacheControlManager.AddCacheControl: TCacheControlEphemeral;
begin
  Result := TCacheControlEphemeral.New;
end;

{ TCitationManager }

class function TCitationManager.AddCharLocation(
  const CitedText: string): TCitationCharLocationParam;
begin
  Result := TCitationCharLocationParam.New.CitedText(CitedText);
end;

class function TCitationManager.AddContentBlockLocation(
  const CitedText: string): TCitationContentBlockLocationParam;
begin
  Result := TCitationContentBlockLocationParam.New.CitedText(CitedText);
end;

class function TCitationManager.AddPageLocation(
  const CitedText: string): TCitationPageLocationParam;
begin
  Result := TCitationPageLocationParam.New.CitedText(CitedText);
end;

class function TCitationManager.AddSearchResultLocation(
  const CitedText: string): TCitationSearchResultLocationParam;
begin
  Result := TCitationSearchResultLocationParam.New.CitedText(CitedText);
end;

class function TCitationManager.AddWebSearchResultLocation(
  const CitedText: string): TCitationWebSearchResultLocationParam;
begin
  Result := TCitationWebSearchResultLocationParam.New.CitedText(CitedText);
end;

{ TContentManager }

class function TContentManager.AddBlockContent(
  const Value: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam.New
      .Source( TDocument.ContentBlock
          .Content(Value)
      );
end;

class function TContentManager.AddBlockContent(
  const Value: TDocumentBlockParam): TDocumentBlockParam;
begin
  Result := Value;
end;

class function TContentManager.AddFileDocument(
  const FileId: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam.New
      .Source( TDocument.FileDocument
          .FileId(FileId)
      )
end;

class function TContentManager.AddImage(const Base64orUri, MimeType: string): TImageBlockParam;
begin
  Result := TImageBlockParam.New
      .Source(Base64orUri, MimeType);
end;

class function TContentManager.AddPDF(const Base64orUri: string): TDocumentBlockParam;
begin
  if Base64orUri.ToLower.StartsWith('http') then
    Exit( TDocumentBlockParam.New
            .Source( TDocument.UrlPdf
                .Url(Base64orUri) )
    );

  if Base64orUri.ToLower.StartsWith('file_') then
    Exit( TDocumentBlockParam.New
            .Source( TDocument.FileDocument
                .FileId(Base64orUri) )
    );

  Result := TDocumentBlockParam.New
      .Source( TBase64PDFSource.New
          .Data(Base64orUri)
          .MediaType()
      );
end;

class function TContentManager.AddText(const Text: string): TTextBlockParam;
begin
  Result := TTextBlockParam.New.Text(Text);
end;

class function TContentManager.AddTextPlain(
  const Text: string): TDocumentBlockParam;
begin
  Result := TDocumentBlockParam.New
      .Source( TDocument.PlainText
          .Data(Text)
          .MediaType()
      );
end;

{ TPayload }

class function TPayload.Assistant(const Value: string): TMessageParam;
begin
  Result := TMessageParam.New
      .Role('assistant')
      .Content(Value);
end;

class function TPayload.Assistant(
  const Value: TArray<TContentBlockParam>): TMessageParam;
begin
  Result := TMessageParam.New
      .Role('assistant')
      .Content(Value);
end;

class function TPayload.User(
  const Value: TArray<TContentBlockParam>): TMessageParam;
begin
  Result := TMessageParam.New
      .Role('user')
      .Content(Value);
end;

class function TPayload.User(const Value: string): TMessageParam;
begin
  Result := TMessageParam.New
      .Role('user')
      .Content(Value);
end;

end.

