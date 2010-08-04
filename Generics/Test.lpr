program Test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, SysUtils, GenericList, GenericThreadedItem, GenericSet,
  GenericStream, GenericTree;

{$R *.res}

begin
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + 'heaptrclog.trc');
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + 'heaptrclog.trc');

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

