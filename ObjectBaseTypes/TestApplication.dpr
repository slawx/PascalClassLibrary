program TestApplication;

uses
  Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UInterfacedBits in 'UInterfacedBits.pas',
  UObjectTypeBase in 'Level 1\UObjectTypeBase.pas',
  UObjectBoolean in 'Level 1\UObjectBoolean.pas',
  UObjectByte in 'Level 1\UObjectByte.pas',
  UObjectDateTime in 'Level 1\UObjectDateTime.pas',
  UObjectDouble in 'Level 1\UObjectDouble.pas',
  UObjectChar in 'Level 1\UObjectChar.pas',
  UObjectInteger in 'Level 1\UObjectInteger.pas',
  UObjectString in 'Level 2\UObjectString.pas',
  UFileSystem in 'Level 2\UFileSystem.pas',
  UObjectPoint in 'Level 2\UObjectPoint.pas',
  USystem in 'Level 3\USystem.pas',
  UObjectStringList in 'Level 3\UObjectStringList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
