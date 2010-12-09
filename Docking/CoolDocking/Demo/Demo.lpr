program Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UDockForm, CoolDocking, SampleDockableForm, UComponentTree,
  FileUtil, SysUtils;

{$R *.res}

const
  HeapTraceLog = 'heaptrclog.trc';
begin
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TComponentTree, ComponentTree);
  Application.Run;
end.

