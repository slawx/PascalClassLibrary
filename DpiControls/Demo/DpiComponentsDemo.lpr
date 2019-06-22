program DpiComponentsDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFormMain, UDpiFormMain, SysUtils
  { you can add units after this };

{$R *.res}

{$if declared(UseHeapTrace)}
const
  HeapTraceLog = 'heaptrclog.trc';
{$ENDIF}

begin
    {$if declared(UseHeapTrace)}
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  {$ENDIF}

  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.

