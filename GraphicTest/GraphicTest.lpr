program GraphicTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazopenglcontext, UMainForm, UPlatform, UDrawMethod, UFastBitmap,
  UDrawForm, bgrabitmappack,
  {$IFDEF WINDOWS}GR32_L,{$ENDIF}
  UCanvasPixels, UCanvasPixelsUpdateLock,
  ULazIntfImageColorsCopy, ULazIntfImageColorsNoCopy, UBGRABitmapPaintBox,
  UBitmapRawImageDataPaintBox, UBitmapRawImageData, UDummyMethod,
  UBitmapRawImageDataMove, UOpenGLMethod, UOpenGLPBOMethod, UGraphics32Method;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDrawForm, DrawForm);
  Application.Run;
end.

