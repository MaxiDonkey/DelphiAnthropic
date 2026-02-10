unit Main;

interface

uses
  Winapi.ShellAPI, Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.TabControl, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.DialogService, FMX.Objects, FMX.Edit, System.JSON, System.IOUtils,
  FMX.ComboEdit, FMX.ListBox,

  Sample.Key.Managment, Sample.UrlOpen, Sample.IniManagment,

  Anthropic, Anthropic.Types, Anthropic.Helpers, Anthropic.Tutorial.FMX, Anthropic.Async.Promise;

type
  TForm1 = class(TForm)
    Panel2: TPanel;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    ButtonUndo: TButton;
    ButtonFinger: TButton;
    Splitter2: TSplitter;
    Panel1: TPanel;
    Panel4: TPanel;
    TabControl1: TTabControl;
    TabItem8: TTabItem;
    Label53: TLabel;
    Panel7: TPanel;
    Panel10: TPanel;
    Memo3: TMemo;
    Panel11: TPanel;
    Text4: TText;
    Panel12: TPanel;
    Memo4: TMemo;
    Panel13: TPanel;
    Text5: TText;
    Splitter3: TSplitter;
    TabItem9: TTabItem;
    Label59: TLabel;
    Label60: TLabel;
    Label6: TLabel;
    Text1: TText;
    Label7: TLabel;
    TabItem1: TTabItem;
    Label1: TLabel;
    ButtonMainMenu: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    TabItem2: TTabItem;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Layout1: TLayout;
    Button15: TButton;
    Button16: TButton;
    Label24: TLabel;
    Layout2: TLayout;
    Label25: TLabel;
    Button17: TButton;
    Layout3: TLayout;
    Label54: TLabel;
    Button3: TButton;
    Button4: TButton;
    Button2: TButton;
    Label3: TLabel;
    Z: TLayout;
    Label2: TLabel;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Label4: TLabel;
    Layout4: TLayout;
    Label5: TLabel;
    Button9: TButton;
    Button10: TButton;
    Layout5: TLayout;
    Label21: TLabel;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Label22: TLabel;
    Label26: TLabel;
    Button18: TButton;
    Label27: TLabel;
    Layout6: TLayout;
    Label28: TLabel;
    Button19: TButton;
    Button20: TButton;
    Label29: TLabel;
    Button21: TButton;
    Layout7: TLayout;
    Label30: TLabel;
    Button22: TButton;
    Label31: TLabel;
    Button23: TButton;
    Button24: TButton;
    Layout8: TLayout;
    Label32: TLabel;
    Button25: TButton;
    Label33: TLabel;
    Button26: TButton;
    Button27: TButton;
    Button28: TButton;
    Button29: TButton;
    Layout9: TLayout;
    Label15: TLabel;
    Label23: TLabel;
    TabItem3: TTabItem;
    Label34: TLabel;
    Label35: TLabel;
    Layout10: TLayout;
    Label36: TLabel;
    Button30: TButton;
    Button31: TButton;
    Label37: TLabel;
    Button33: TButton;
    Label38: TLabel;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Layout11: TLayout;
    Label39: TLabel;
    Button37: TButton;
    Label40: TLabel;
    Button32: TButton;
    Button38: TButton;
    Button39: TButton;
    Button40: TButton;
    Label41: TLabel;
    Layout12: TLayout;
    Label42: TLabel;
    Button41: TButton;
    Label43: TLabel;
    Button42: TButton;
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure ButtonUndoClick(Sender: TObject);
    procedure ButtonFingerClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Label3Click(Sender: TObject);
    procedure Label4Click(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure ButtonMainMenuClick(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label20Click(Sender: TObject);
    procedure Label22Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Label26Click(Sender: TObject);
    procedure Button17Click(Sender: TObject);
    procedure Button18Click(Sender: TObject);
    procedure Label27Click(Sender: TObject);
    procedure Button19Click(Sender: TObject);
    procedure Button20Click(Sender: TObject);
    procedure Button21Click(Sender: TObject);
    procedure Label29Click(Sender: TObject);
    procedure Button22Click(Sender: TObject);
    procedure Button23Click(Sender: TObject);
    procedure Button24Click(Sender: TObject);
    procedure Label31Click(Sender: TObject);
    procedure Button25Click(Sender: TObject);
    procedure Button26Click(Sender: TObject);
    procedure Button27Click(Sender: TObject);
    procedure Button29Click(Sender: TObject);
    procedure Button28Click(Sender: TObject);
    procedure Label33Click(Sender: TObject);
    procedure Label23Click(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Label14Click(Sender: TObject);
    procedure Button30Click(Sender: TObject);
    procedure Button33Click(Sender: TObject);
    procedure Button34Click(Sender: TObject);
    procedure Button35Click(Sender: TObject);
    procedure Label37Click(Sender: TObject);
    procedure Button31Click(Sender: TObject);
    procedure Button36Click(Sender: TObject);
    procedure Button37Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure Button38Click(Sender: TObject);
    procedure Button39Click(Sender: TObject);
    procedure Button40Click(Sender: TObject);
    procedure Label40Click(Sender: TObject);
    procedure Button41Click(Sender: TObject);
    procedure Button42Click(Sender: TObject);
  private
    Client: IAnthropic;
    FPageIndex: Integer;
    procedure PageUpdate;
    procedure NextPage;
    procedure PreviousPage;
    procedure SetPageIndex(const Value: Integer);
    procedure StartRun(const Prompt: string; const Streamed: Boolean = False);
    procedure StarPayload(const Prompt: string);
    function StreamedSessionCallbacks: TSessionCallbacksStream;
  public
    property PageIndex: Integer read FPageIndex write SetPageIndex;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.ButtonFingerClick(Sender: TObject);
begin
  TabControl1.TabIndex := 0;
end;

procedure TForm1.ButtonMainMenuClick(Sender: TObject);
begin
  TabControl1.TabIndex := 1;
end;

procedure TForm1.ButtonUndoClick(Sender: TObject);
begin
  TabControl1.TabIndex := PageIndex;
end;

procedure TForm1.Button10Click(Sender: TObject);
// Putting words in Claude's mouth
begin
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1;
  var Prompt := 'What is latin for Ant? (A) Apoidea, (B) Rhopalocera, (C) Formicidae';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( Prompt )
              .Assistant('The answer is (')
          )
          .MaxTokens(MaxTokens);
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

end;

procedure TForm1.Button11Click(Sender: TObject);
// Vision - inline data
begin
  var Document := '..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(Document);
  var MimeType := TMediaCodec.GetMimeType(Document);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'What is in the above image?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddImage(Base64, MimeType)
                  .AddText(Prompt)
              )
          )
          .MaxTokens(MaxTokens)
          .Stream;

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button12Click(Sender: TObject);
// Vision - URL-referenced image
begin
  var ImageUrl := 'https://upload.wikimedia.org/wikipedia/commons/a/a7/Camponotus_flavomarginatus_ant.jpg';

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'What is in the above image?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddImage(ImageUrl)
                  .AddText(Prompt)
              )
          )
          .MaxTokens(MaxTokens)
          .Stream;

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button13Click(Sender: TObject);
// PDF support - inline data
begin
  var Document := '..\media\File_Search_file.pdf';
  var Base64 := TMediaCodec.EncodeBase64(Document);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 2048;
  var Prompt := 'How can the work documented in the PDF be relevant?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddPDF(Base64)
                  .AddText(Prompt)
              )
          )
          .MaxTokens(MaxTokens)
          .Stream;

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button14Click(Sender: TObject);
// PDF support - URL-based PDF
begin
  var PDFUrl := 'https://assets.anthropic.com/m/1cd9d098ac3e6467/original/Claude-3-Model-Card-October-Addendum.pdf';

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'What are the key findings in this document?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddPDF(PDFUrl)
                  .AddText(Prompt)
              )
          )
          .MaxTokens(MaxTokens)
          .Stream;

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button15Click(Sender: TObject);
// Using cache_control block
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var SystemPrompt1 := 'You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style.';
  var SystemPrompt2 := '<the entire contents of Pride and Prejudice>';
  var Prompt := 'Analyze the major themes in Pride and Prejudice.';

  StarPayload(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .System( TextBlockParts
              .Add( CreateTextBlock
                  .Text(SystemPrompt1)
              )
              .Add( CreateTextBlock
                  .Text(SystemPrompt2)
                  .CacheControl( Cache
                      .AddCacheControl
                  )
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          )
          .MaxTokens(MaxTokens);
    end;

  var X := TChatParams.Create;
  Payload(X);
  Memo1.Text := X.ToFormat(True);
end;

procedure TForm1.Button16Click(Sender: TObject);
// 1-hour cache duration
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var SystemPrompt1 := 'You are an AI assistant tasked with analyzing literary works. Your goal is to provide insightful commentary on themes, characters, and writing style.';
  var SystemPrompt2 := '<the entire contents of Pride and Prejudice>';
  var Prompt := 'Analyze the major themes in Pride and Prejudice.';

  StarPayload(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .System( TextBlockParts
              .Add( CreateTextBlock
                  .Text(SystemPrompt1)
              )
              .Add( CreateTextBlock
                  .Text(SystemPrompt2)
                  .CacheControl( Cache
                      .AddCacheControl('1h')
                  )
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          )
          .MaxTokens(MaxTokens);
    end;

  var X := TChatParams.Create;
  Payload(X);
  Memo1.Text := X.ToFormat(True);
end;

procedure TForm1.Button17Click(Sender: TObject);
// Extended Reasoning
begin
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var BudgetTokens := 10000;
  var Prompt := 'Are there an infinite number of prime numbers such that n mod 4 == 3?';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Thinking( CreateThinkingConfig('enabled')
              .BudgetTokens(BudgetTokens)
          )
          .Messages( MessageParts
              .User( Prompt )
          );
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);

        var Thought := '';
        for var Item in Value.Content do
          if Item.&Type = TContentBlockType.thinking then
            Thought := Thought + Item.Thinking + sLineBreak;

        ShowMessage('<think>'#10 + Thought + '</think>');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button18Click(Sender: TObject);
// Adaptive Reasoning
begin
    var ModelName := 'claude-opus-4-6';
    var MaxTokens := 16000;
    var EffortValue := 'max';
    var Prompt := 'Explain why the sum of two even numbers is always even.';

    StartRun(Prompt);

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .MaxTokens(MaxTokens)
            .Thinking( CreateThinkingConfig('adaptive') )
            .OutputConfig( CreateOutputConfig
              .Effort( EffortValue )
            )
            .Messages( MessageParts
                .User( Prompt )
            );
        TutorialHub.JSONRequest := Params.ToFormat();
      end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);

        var Thought := '';
        for var Item in Value.Content do
          if Item.&Type = TContentBlockType.thinking then
            Thought := Thought + Item.Thinking + sLineBreak;

        ShowMessage('<think>'#10 + Thought + '</think>');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button19Click(Sender: TObject);
// JSON outputs schema
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Extract the key information from this email: John Smith (john@example.com) is interested in our Enterprise plan and wants to schedule a demo for next Tuesday at 2pm.';

  // Schema Payload creation using TSchemaParams class
  var SchemaPayload := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('name', TJSONObject.Create
           .AddPair('type', 'string')
       )
       .AddPair('email', TJSONObject.Create
           .AddPair('type', 'string')
       )
       .AddPair('plan_interest', TJSONObject.Create
           .AddPair('type', 'string')
       )
       .AddPair('demo_requested', TJSONObject.Create
           .AddPair('type', 'boolean')
       )
    )
    .Required(['name', 'email', 'plan_interest', 'demo_requested'])
    .AdditionalProperties(False);

  StartRun(Prompt);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt )
          )
          .OutputConfig( CreateOutputConfig
              .Format( CreateFormat
                  .Schema( SchemaPayload )
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button20Click(Sender: TObject);
// Strict tool use
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What is the weather in San Francisco?';

  // Schema Payload creation using TSchemaParams class
  var GetWeather := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('location', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('description', 'The city and state, e.g. San Francisco, CA')
       )
       .AddPair('unit', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('enum', TJSONArray.Create
               .Add('celsius')
               .Add('fahrenheit'))
       )
    )
    .Required(['location'])
    .AdditionalProperties(False);

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt )
          )
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .Name('get_weather')
                  .Description('Get the current weather in a given location')
                  .Strict(True)
                  .InputSchema(GetWeather)
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button21Click(Sender: TObject);
// Using both features together
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Help me plan a trip to Paris for next month';

  // Schema Payload creation using TSchemaParams class
  var SchemaPayload := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('summary', TJSONObject.Create
           .AddPair('type', 'string')
       )
       .AddPair('next_steps', TJSONObject.Create
           .AddPair('type', 'array')
           .AddPair('items', TJSONObject.Create.AddPair('type', 'string'))
       )
    )
    .Required(['summary', 'next_steps'])
    .AdditionalProperties(False);

  var SearchFlightsSchema := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('destination', TJSONObject.Create
           .AddPair('type', 'string')
       )
       .AddPair('date', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('format', 'date')
       )
    )
    .Required(['destination', 'date'])
    .AdditionalProperties(False);

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( Prompt )
          )
          .OutputConfig( CreateOutputConfig
              .Format( CreateFormat
                  .Schema( SchemaPayload )
              )
          )
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .Name('search_flights')
                  .Strict(True)
                  .InputSchema( SearchFlightsSchema )
              )
          );
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button22Click(Sender: TObject);
// Activating citations (text/plain)
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What color is the grass and sky?';

  var DataTextPlain := 'The grass is green. The sky is blue.';
  var DocumentContext := 'This is a trustworthy document.';
  var DocumentTitle := 'My Document';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( ContentParts
                  .AddTextPlain( CreateDocumentBlock
                       .Source( Document.PlainText
                           .Data( DataTextPlain )
                       )
                       .Title( DocumentTitle )
                       .Context( DocumentContext )
                       .Citations(True)
                  )
                  .AddText( Prompt )
              )
          );
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button23Click(Sender: TObject);
// Using caching & citations
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'What does this document say about API features?';

  var DataTextPlain := 'This is a very long document with thousands of words...';

  StarPayload(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( ContentParts
                  .AddTextPlain( CreateDocumentBlock
                       .Source( Document.PlainText
                           .Data( DataTextPlain )
                       )
                       .Citations(True)
                       .CacheControl( Cache
                           .AddCacheControl
                       )
                  )
                  .AddText( Prompt )
              )
          );
    end;

  var Params := TChatParams.Create;
  Payload(Params);
  Memo1.Lines.Text := Params.ToFormat(True);
end;

procedure TForm1.Button24Click(Sender: TObject);
// Activating citations (PDF)
begin
  var Document := '..\media\File_Search_file.pdf';
  var Base64 := TMediaCodec.EncodeBase64(Document);

  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 2048;
  var Prompt := 'How can the work documented in the PDF be relevant?';

  var DocumentContext := 'Context about the document that will not be cited from';
  var DocumentTitle := 'Document Title';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( ContentParts
                  .AddPDF( CreateDocumentBlock
                       .Source( Document.Base64PDF
                           .Data( Base64 )
                       )
                       .Title( DocumentTitle )
                       .Context( DocumentContext )
                       .Citations(True)
                  )
                  .AddText( Prompt )
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  Client.HttpClient.ResponseTimeout := 300000;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button25Click(Sender: TObject);
// Basic message
begin
  var ModelName := 'claude-opus-4-6';
  var SystemPrompt := 'You are a scientist';
  var Prompt := 'Hello, Claude';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .System(SystemPrompt)
          .Messages( MessageParts
              .User( Prompt )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitTokenCount(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TTokenCount)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button26Click(Sender: TObject);
// with tools
begin
  var ModelName := 'claude-opus-4-6';
  var Prompt := 'What is the weather in San Francisco?';

  // Schema Payload creation using TSchemaParams class
  var GetWeather := TSchemaParams.New
    .&Type('object')
    .Properties( TJSONObject.Create
       .AddPair('location', TJSONObject.Create
           .AddPair('type', 'string')
           .AddPair('description', 'The city and state, e.g. San Francisco, CA')
       )
    )
    .Required(['location']);

  StartRun(Prompt);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Tools( ToolParts
              .Add( Tool.CreateToolCustom
                  .Name('get_weather')
                  .Description('Get the current weather in a given location')
                  .InputSchema(GetWeather)
              )
          )
          .Messages( MessageParts
              .User( Prompt )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitTokenCount(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TTokenCount)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button27Click(Sender: TObject);
// with images
begin
  var Document := '..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(Document);
  var MimeType := TMediaCodec.GetMimeType(Document);

  var ModelName := 'claude-opus-4-6';
  var Prompt := 'Describe this image';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddImage(Base64, MimeType)
                  .AddText(Prompt)
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitTokenCount(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TTokenCount)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button28Click(Sender: TObject);
// with extended thinking
begin
  var ModelName := 'claude-sonnet-4-5';
  var BudgetTokens := 10000;
  var Prompt := 'Are there an infinite number of prime numbers such that n mod 4 == 3?';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Thinking( CreateThinkingConfig('enabled')
              .BudgetTokens(BudgetTokens)
          )
          .Messages( MessageParts
              .User( Prompt )
          );
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitTokenCount(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TTokenCount)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button29Click(Sender: TObject);
// with PDF
begin
  var Document := '..\media\File_Search_file.pdf';
  var Base64 := TMediaCodec.EncodeBase64(Document);

  var ModelName := 'claude-opus-4-6';
  var Prompt := 'Please summarize this document.';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddPDF(Base64)
                  .AddText(Prompt)
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitTokenCount(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TTokenCount)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button2Click(Sender: TObject);
// Asynchronous orchestration
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 2048;
  var Prompt := 'Write a short story about a magic backpack.';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( Prompt )
          )
          .MaxTokens(MaxTokens);
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  var Callbacks: TSessionCallbacks :=
    function: TPromiseChat
    begin
      Result.Sender := TutorialHub;
      Result.OnSuccess := DisplayPromise;
      Result.OnError := DisplayPromise;
    end;

  Display(TutorialHub, '-------------------------------------- STEP 1'#10);

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload, Callbacks);

  Promise
    .&Then(
      function (First: TChat): TPromise<TChat>
      var
        Answer: string;
      begin
        Answer := First.Content[0].Text;
        Display(TutorialHub, '-------------------------------------- STEP 2'#10);

        // Second promise
        Result := Client.Chat.AsyncAwaitCreate(
          procedure (Params: TChatParams)
          begin
            with Generation do
              Params
                .Model(ModelName)
                .Messages( MessageParts
                    .User( 'Summarize the following answer in 5 bullet points:'#10 + Answer )
                )
                .MaxTokens(MaxTokens);
            TutorialHub.JSONRequest := Params.ToFormat();
          end
        );
      end)
   .&Then(
      procedure (Second: TChat)
      begin
        Display(TutorialHub, '# SUMMARY'#10);
        Display(TutorialHub, Second);
      end)
   .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button30Click(Sender: TObject);
begin
  var FilePath := '..\media\File_Search_file.pdf';

  StartRun('Uploading a file');

  // MultipartFormData payload creation
  var Payload: TUploadParamProc :=
    procedure (Params: TUploadParams)
    begin
      Params.&File(FilePath);
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitUpload(Payload);

  Promise
    .&Then(
      procedure (Value: TFile)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Upload(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button31Click(Sender: TObject);
begin
  var ModelName := 'claude-opus-4-6';
  var MaxTokens := 1024;
  var Prompt := 'Please summarize this document for me.';

  var FileID := TInputContent.Text;
  if FileID.Trim.IsEmpty then
    Exit;

  StartRun(Prompt, True);

  //JSON payload creation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Beta(['files-api-2025-04-14'])
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .Messages( MessageParts
              .User( ContentParts
                  .AddText(Prompt)
                  .AddFileDocument(FileId)
              )
          );

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  Client.HttpClient.ResponseTimeout := 300000;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button32Click(Sender: TObject);
// Batch job list
begin
  StartRun('Batch job list');

  //Query params creation
  var QueryParams: TListParamProc :=
    procedure (Params: TListParams)
    begin
      Params.Limit(20);
    end;

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TBatchList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.List(QueryParams);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
end;

procedure TForm1.Button33Click(Sender: TObject);
// List of files
begin
  StartRun('List of files');

  var QueryParams: TFilesListParamProc :=
    procedure (Params: TFilesListParams)
    begin
      Params.Limit(5);
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TFileList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.List(QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button34Click(Sender: TObject);
// Retrieve a file by it's ID
begin
  StartRun('Retrieve a file by it''s ID');

  var FileID := TInputContent.Text;
  if FileID.Trim.IsEmpty then
    Exit;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitRetrieve(FileID);

  Promise
    .&Then(
      procedure (Value: TFile)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Retrieve(FileId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button35Click(Sender: TObject);
// Delete a file by it's ID
begin
  StartRun('Delete a file by it''s ID');

  var FileID := TInputContent.Text;
  if FileID.Trim.IsEmpty then
    Exit;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitDelete(FileId);

  Promise
    .&Then(
      procedure (Value: TFileDeleted)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Delete(FileId);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button36Click(Sender: TObject);
// Download a file
begin
  StartRun('Download a file by it''s ID');

  var FileName := 'budget.xlsx';

  var FileID := TInputContent.Text;
  if FileID.Trim.IsEmpty then
    Exit;

  // Asynchronous generation (promise-based)
  var Promise := Client.Files.AsyncAwaitDownload(FileId);

  Promise
    .&Then(
      procedure (Value: TFileDownloaded)
      begin
        Value.SaveToFile(FileName);
        Display(TutorialHub, FileName + ' downloaded');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Files.Download(FileId);
//
//  try
//    Value.SaveToFile(FileName);
//    Display(TutorialHub, FileName + ' downloaded');
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button37Click(Sender: TObject);
// Batch job Creation
begin
  StartRun('Batch job Creation');

  var Jsonl := TJSONLHelper.LoadFromFile('..\media\BatchExample.jsonl');

  //JSON payload
  var Payload: TRequestParamProc :=
     procedure (Params: TRequestParams)
     begin
       Params
         .Requests(JsonL);

       TutorialHub.JSONRequest := Params.ToFormat();
     end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Batch.AsyncAwaitCreate(Payload);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  // Synchronous generation
//  var Value := Client.Batch.Create(Payload);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button38Click(Sender: TObject);
// Batch job retrieve
begin
  StartRun('Batch job retrieve');

  var BatchID := TInputContent.Text;
  if BatchID.Trim.IsEmpty then
    Exit;

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitRetrieve(BatchID);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Retrieve(BatchID);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
end;

procedure TForm1.Button39Click(Sender: TObject);
// Batch job cancel
begin
  StartRun('Batch job retrieve');

  var BatchID := TInputContent.Text;
  if BatchID.Trim.IsEmpty then
    Exit;

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitCancel(BatchID);

  Promise
    .&Then(
      procedure (Value: TBatch)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Cancel(BatchId);
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
// Synchronous create
begin
  var ModelName := 'claude-sonnet-4-5';
  var Prompt := 'What should I search for to find the latest developments in renewable energy?';
  var MaxTokens := 1000;

  StartRun(Prompt);

  //JSON payload
  var Payload: TChatParamProc :=
     procedure (Params: TChatParams)
     begin
       Params
         .Model(ModelName)
         .Messages( Generation.MessageParts
             .User( Prompt )
         )
         .MaxTokens(MaxTokens);
       TutorialHub.JSONRequest := Params.ToFormat();
     end;

  Application.ProcessMessages;

  //Synchronous example
  var Generated := Client.Chat.Create(Payload);

  //Display generated content
  try
    Display(TutorialHub, Generated);
  finally
    Generated.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
// Synchronous stream
begin

  var ModelName := 'claude-opus-4-6';  //sonnet-4-5';
  var MaxTokens := 2048;
  var SystemPrompt := 'You are an expert in mathematics topology';
  var Prompt := 'Can we find accumulation points in a discrete topology?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        params
          .Model(ModelName)
          .MaxTokens(MaxTokens)
          .System(SystemPrompt)
          .Messages( MessageParts
            .User( Prompt )
          )
          .Thinking( CreateThinkingConfig('adaptive')
          )
          .Stream;
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Streaming callback
  var StreamEvent: TChatEvent :=
     procedure (var Chat: TChatStream; IsDone: Boolean; var Cancel: Boolean)
     begin
       if not IsDone then
         DisplayStream(TutorialHub, Chat);

       Cancel := TutorialHub.Cancel;
       if Cancel then
         Display(TutorialHub, 'aborted');

       Application.ProcessMessages;
     end;

  //Synchronous example
  Client.Chat.CreateStream(Payload, StreamEvent);
end;

procedure TForm1.Button40Click(Sender: TObject);
// Download batch Results
begin
  StartRun('Download batch Results');

  var BatchID := TInputContent.Text;
  if BatchID.Trim.IsEmpty then
    Exit;

  var BatchResultFileName := 'BatchResult.jsonl';

  // Asynchronous example
  var Promise := Client.Batch.AsyncAwaitRetrieve(BatchID, BatchResultFileName);

  Promise
    .&Then(
      procedure (Value: TStringList)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Batch := Client.Batch.Retrieve(BatchID, BatchResultFileName);
//
//  try
//    Display(TutorialHub, Batch);
//  finally
//    Batch.Free;
//  end;
end;

procedure TForm1.Button41Click(Sender: TObject);
// Models List
begin
  StartRun('Models List');

  // Query params creation
  var QueryParams: TListModelsParamProc :=
    procedure (Params: TListModelsParams)
    begin
      Params.Limit(10);
    end;

  // Asynchronous example
  var Promise := Client.Models.AsyncAwaitList(QueryParams);

  Promise
    .&Then(
      procedure (Value: TModels)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Value := Client.Models.List(QueryParams);
//
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button42Click(Sender: TObject);
// Models Retrieve
begin
  StartRun('Models Retrieve');

  var ModelID := TInputContent.Text;
  if ModelID.Trim.IsEmpty then
    Exit;

  // Asynchronous example
  var Promise := Client.Models.AsyncAwaitRetrieve(ModelID);

  Promise
    .&Then(
      procedure (Value: TModel)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);

  //Synchronous example
//  var Value := Client.Models.Retrieve(ModelID);
//  try
//    Display(TutorialHub, Value);
//  finally
//    Value.Free;
//  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
// Asynchronous create
begin
  var Document := '..\media\Invoice.png';
  var Base64 := TMediaCodec.EncodeBase64(Document);
  var MimeType := TMediaCodec.GetMimeType(Document);

  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1000;
  var Prompt := 'Describe the image';

  StartRun(Prompt);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      with Generation do
        Params
          .Model(ModelName)
          .Messages( MessageParts
              .User( ContentParts
                  .AddImage(Base64, MimeType)
                  .AddText(Prompt)
              )
          )
          .MaxTokens(MaxTokens);
      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreate(Payload);

  // Simple processing or orchestration of promise
  Promise
    .&Then(
      procedure (Value: TChat)
      begin
        Display(TutorialHub, Value);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button6Click(Sender: TObject);
// Asynchronous stream - session callbacks
begin
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var ThinkingBudget := 6000;
  var Prompt := 'From which version of Delphi were multi-line strings introduced?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( Prompt )
          )
        .Thinking( CreateThinkingConfig('enabled')
            .BudgetTokens(ThinkingBudget)
          )
        .Stream;
    TutorialHub.JSONRequest := Params.ToFormat();
  end;

  var SessionCallbacks: TSessionCallbacksStream :=
      function : TPromiseChatStream
      begin
        Result.Sender := TutorialHub;
        Result.OnProgress := DisplayStream;
        Result.OnDoCancel := DoCancellation;
        Result.OnCancellation := DoCancellationStream;
        Result.OnError := DisplayPromise;
      end;

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, SessionCallbacks);

  Promise
    .&Then(
      procedure (Value: TEventData)
      begin
        ShowMessage('<THINK>'#10#10 + Value.Thought + #10'</THINK>');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button7Click(Sender: TObject);
// Asynchronous stream - events callbacks
begin
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 16000;
  var ThinkingBudget := 6000;
  var Prompt := 'From which version of Delphi were multi-line strings introduced?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
  procedure (Params: TChatParams)
  begin
    with Generation do
      Params
        .Model(ModelName)
        .MaxTokens(MaxTokens)
        .Messages( MessageParts
            .User( Prompt )
          )
        .Thinking( CreateThinkingConfig('enabled')
            .BudgetTokens(ThinkingBudget)
          )
        .Stream;
    TutorialHub.JSONRequest := Params.ToFormat();
  end;

  var EventCallbacks := TEventEngineManagerFactory.CreateInstance(
    function : TStreamEventCallBack
    begin
      Result.Sender := TutorialHub;
      Result.OnMessageStart := DisplayMessageStart;
      Result.OnMessageDelta := DisplayMessageDelta;
      Result.OnMessageStop := DisplayMessageStop;
      Result.OnContentStart := DisplayContentStart;
      Result.OnContentDelta := DisplayContentDelta;
      Result.OnContentStop := DisplayContentStop;
      Result.OnError := DisplayStreamError;
    end);

  //Asynchronous promise example
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, EventCallbacks);

  Promise
    .&Then(
      procedure (Value: TEventData)
      begin
        ShowMessage('<THINK>'#10#10 + Value.Thought + #10'</THINK>');
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button8Click(Sender: TObject);
// Asynchronous stream orchestration
begin
    var ModelName := 'claude-sonnet-4-5';
    var MaxTokens := 2048;
    var Prompt := 'Write a short story about a magic backpack.';

    StartRun(Prompt, True);

    //JSON payload generation
    var Payload: TChatParamProc :=
      procedure (Params: TChatParams)
      begin
        with Generation do
          Params
            .Model(ModelName)
            .Messages( MessageParts
                .User( Prompt )
            )
            .MaxTokens(MaxTokens)
            .Stream;
        TutorialHub.JSONRequest := Params.ToFormat();
      end;

  Display(TutorialHub, '-------------------------------------- STEP 1'#10);

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Then(
      function (First: TEventData): TPromise<TEventData>
      begin
        Display(TutorialHub, '-------------------------------------- STEP 2'#10);

        // Second promise: Must be mute - no Callback
        Result := Client.Chat.AsyncAwaitCreateStream(
          procedure (Params: TChatParams)
          begin
             with Generation do
               Params
                 .Model(ModelName)
                 .MaxTokens(MaxTokens)
                 .Messages( MessageParts
                     .User( 'Summarize the following answer: ' + First.Text )
                 )
                 .Stream;
             TutorialHub.JSONRequest := Params.ToFormat();
          end);
      end)
    .&Then(
      procedure (Second: TEventData)
      begin
        ShowMessage('----- SUMMARY'#10 + Second.Text);
      end)
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.Button9Click(Sender: TObject);
// Multiple conversational turns
begin
  var ModelName := 'claude-sonnet-4-5';
  var MaxTokens := 1024;
  var Prompt := 'Can you describe LLMs to me?';

  StartRun(Prompt, True);

  //JSON payload generation
  var Payload: TChatParamProc :=
    procedure (Params: TChatParams)
    begin
      Params
        .Model(ModelName)
        .Messages( Generation.MessageParts
            .User( 'Hello, Claude' )
            .Assistant( 'Hello!' )
            .User( Prompt )
        )
        .MaxTokens(MaxTokens)
        .Stream;

      TutorialHub.JSONRequest := Params.ToFormat();
    end;

  // Asynchronous generation (promise-based)
  var Promise := Client.Chat.AsyncAwaitCreateStream(Payload, StreamedSessionCallbacks());

  Promise
    .&Catch(
      procedure (E: Exception)
      begin
        Display(TutorialHub, E.Message);
      end);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not HttpMonitoring.IsBusy;
  if not CanClose then
    TDialogService.MessageDialog(
      'Requests are still in progress. Please wait for them to complete before closing the application.',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], TMsgDlgBtn.mbOK, 0, nil);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  Client := TAnthropicFactory.CreateInstance(API_KEY);
  TutorialHub := TFMXTutorialHub.Create(Client, Memo1, Memo2, Memo3, Memo4, Button1);

  Width := 1600;
  Height := 900;
  PageIndex := TAppIni.ReadInteger('General', 'pageIndex', 1);
  TabControl1.TabIndex := PageIndex;
  PageUpdate;
  Button1.Visible := False;
end;

procedure TForm1.Label40Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/batch-processing.md');
end;

procedure TForm1.Label4Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/content-generation-sse.md');
end;

procedure TForm1.Label10Click(Sender: TObject);
begin
  TabControl1.TabIndex := 3;
end;

procedure TForm1.Label13Click(Sender: TObject);
begin
  TabControl1.TabIndex := 4;
end;

procedure TForm1.Label14Click(Sender: TObject);
begin
  TabControl1.TabIndex := 4;
end;

procedure TForm1.Label1Click(Sender: TObject);
begin
  TabControl1.TabIndex := 2;
end;

procedure TForm1.Label20Click(Sender: TObject);
begin
  TEnvironmentManager.SetKeyValue;
end;

procedure TForm1.Label22Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/document-understanding.md');
end;

procedure TForm1.Label23Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/fast-mode.md');
end;

procedure TForm1.Label26Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/prompt-caching.md');
end;

procedure TForm1.Label27Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/thinking.md');
end;

procedure TForm1.Label29Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/structured-outputs.md');
end;

procedure TForm1.Label31Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/citations.md');
end;

procedure TForm1.Label33Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/token-counting.md');
end;

procedure TForm1.Label37Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/files-api.md');
end;

procedure TForm1.Label3Click(Sender: TObject);
begin
  OpenUrl('https://github.com/MaxiDonkey/DelphiAnthropic/blob/main/guides/content-generation.md');
end;

procedure TForm1.Label6Click(Sender: TObject);
begin
  PreviousPage;
end;

procedure TForm1.Label7Click(Sender: TObject);
begin
  NextPage;
end;

procedure TForm1.NextPage;
begin
  if TabControl1.TabIndex >= TabControl1.TabCount - 1 then
    begin
      PageUpdate;
      Exit;
    end;

  TabControl1.TabIndex := TabControl1.TabIndex + 1;
  if TabControl1.TabIndex > 0 then
    PageIndex := TabControl1.TabIndex;

  PageUpdate;
end;

procedure TForm1.PageUpdate;
begin
  case TabControl1.TabIndex of
    0:
      Text1.Text := 'JSON Payload-Result';
    1:
      Text1.Text := 'Summary';
    2:
      Text1.Text := 'API message';
    3:
      Text1.Text := 'Capabilities';
    4:
      Text1.Text := 'API Files && Batch processing';
  end;
end;

procedure TForm1.PreviousPage;
begin
  if TabControl1.TabIndex = 0 then
    begin
      PageUpdate;
      Exit;
    end;

  TabControl1.TabIndex := TabControl1.TabIndex - 1;
  if TabControl1.TabIndex > 0 then
    PageIndex := TabControl1.TabIndex;

  PageUpdate;
end;

procedure TForm1.SetPageIndex(const Value: Integer);
begin
  FPageIndex := Value;
  TAppIni.WriteInteger('General', 'pageIndex', FPageIndex);
end;

procedure TForm1.StarPayload(const Prompt: string);
begin
  TutorialHub.HideCancel;
  TutorialHub.JSONUIClear;
  Memo2.Text := Prompt;
end;

procedure TForm1.StartRun(const Prompt: string; const Streamed: Boolean);
begin
  if Streamed then
    TutorialHub.ShowCancel
  else
    TutorialHub.HideCancel;

  TutorialHub.JSONUIClear;
  TabControl1.TabIndex := 0;
  Display(TutorialHub, 'This may take a few seconds. Please wait...' + sLineBreak);
  Memo2.Text := Prompt;
end;

function TForm1.StreamedSessionCallbacks: TSessionCallbacksStream;
begin
  Result :=
    function : TPromiseChatStream
    begin
      Result.Sender := TutorialHub;
      Result.OnProgress := DisplayStream;
      Result.OnDoCancel := DoCancellation;
      Result.OnCancellation := DoCancellationStream;
      Result.OnError := DisplayPromise;
    end;
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex = 0 then
    begin
      PageUpdate;
      Exit;
    end;

  PageIndex := TabControl1.TabIndex;
  PageUpdate;
end;

end.
