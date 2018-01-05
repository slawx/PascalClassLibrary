program Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, UMainForm;

{$R *.res}

{$IFDEF DEBUG}
const
  HeapTraceLogFileName = 'heaptrclog.trc';
  {$ENDIF}
begin
  {$IFDEF DEBUG}
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLogFileName);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLogFileName);
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

