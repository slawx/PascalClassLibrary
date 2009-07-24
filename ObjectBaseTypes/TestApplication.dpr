program TestApplication;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UObjectString in 'UObjectString.pas',
  UObjectInteger in 'UObjectInteger.pas',
  UObjectTypeBase in 'UObjectTypeBase.pas',
  UObjectBoolean in 'UObjectBoolean.pas',
  UObjectByte in 'UObjectByte.pas',
  UInterfacedBits in 'UInterfacedBits.pas',
  UObjectDouble in 'UObjectDouble.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
