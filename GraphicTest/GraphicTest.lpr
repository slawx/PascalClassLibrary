program GraphicTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, UMainForm, UPlatform, UDrawMethod, UFastBitmap,
  bgrabitmappack, UDrawForm, UCanvasPixels, UCanvasPixelsUpdateLock,
  ULazIntfImageColorsCopy, ULazIntfImageColorsNoCopy, UBGRABitmapPaintBox, 
UBitmapRawImageDataPaintBox, UBitmapRawImageData, UDummyMethod, 
UBitmapRawImageDataMove, UOpenGLMethod, UOpenGLPBOMethod;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDrawForm, DrawForm);
  Application.Run;
end.

