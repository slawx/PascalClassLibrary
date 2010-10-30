program Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, UMainForm, TemplateGenerics;

{$R *.res}

const
  HeapTraceLogFileName = 'heaptrclog.trc';
begin
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLogFileName);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLogFileName);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

