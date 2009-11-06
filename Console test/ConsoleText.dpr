program ConsoleText;

uses
  Forms,
  UTextScreen in 'UTextScreen.pas',
  UConsole in 'UConsole.pas',
  UConsoleApp in 'UConsoleApp.pas',
  UFileSystem in 'UFileSystem.pas',
  UMainForm in 'UMainForm.pas' {MainForm},
  UMyConsoleApp in 'UMyConsoleApp.pas',
  UKeyboard in 'UKeyboard.pas';

{$R *.res}

begin
  {$WARN SYMBOL_PLATFORM OFF}
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  {$WARN SYMBOL_PLATFORM ON}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
