program Test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,UMainForm,USystem,UObjectStringList,UObjectTypeBase,UObjectBoolean,
  UObjectByte,UObjectDateTime,UObjectDouble,UObjectChar,UObjectInteger,
  UObjectString,UBitStream,UFileSystem,UObjectPoint;

{$R *.res}

begin
  Application.Initialize;
  Application.Run;
end.

