program GraphicTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, SysUtils, openglcontext, UMainForm, UPlatform, UDrawMethod, UFastBitmap,
  UDrawForm, bgrabitmappack,
  {$IFDEF GRAPHICS32}GR32_L,{$ENDIF}
  UCanvasPixels, UCanvasPixelsUpdateLock,
  ULazIntfImageColorsCopy, ULazIntfImageColorsNoCopy, UBGRABitmapPaintBox,
  UBitmapRawImageDataPaintBox, UBitmapRawImageData, UDummyMethod,
  UBitmapRawImageDataMove, UOpenGLMethod, UOpenGLPBOMethod, UGraphics32Method;

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

  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDrawForm, DrawForm);
  Application.Run;
end.

