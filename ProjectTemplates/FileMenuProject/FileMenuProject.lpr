program FileMenuProject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UFormMain, UCore, Common, CoolTranslator, UProject, UDataFile,
  TemplateGenerics, SysUtils
  { you can add units after this };

{$R *.res}

{$IFDEF DEBUG}
const
  HeapTraceLog = 'heaptrclog.trc';
{$ENDIF}

begin
  {$IFDEF DEBUG}
  // Heap trace
  DeleteFile(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + HeapTraceLog);
  {$ENDIF}
  Application.Scaled:=True;
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TCore, Core);
  Application.Run;
end.

