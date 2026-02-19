program Sample;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1},
  Anthropic.JSONL in '..\source\Anthropic.JSONL.pas',
  Anthropic.Context.Helper in '..\source\Anthropic.Context.Helper.pas',
  Anthropic.API.MultiFormData in '..\source\Anthropic.API.MultiFormData.pas',
  Anthropic.Files.Helper in '..\source\Anthropic.Files.Helper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
