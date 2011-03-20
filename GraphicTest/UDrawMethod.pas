unit UDrawMethod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, ExtCtrls, UPlatform, UFastBitmap, Graphics,
  LCLType, IntfGraphics, fpImage, GraphType, BGRABitmap, BGRABitmapTypes,
  LclIntf, GL, GLExt, OpenGLContext;

type
  TPaintObject = (poImage, poPaintBox, poOpenGL);

  { TDrawMethod }

  TDrawMethod = class
  private
    FBitmap: TBitmap;
    TempBitmap: TBitmap;
    FPaintBox: TPaintBox;
    procedure SetBitmap(const AValue: TBitmap); virtual;
    procedure SetPaintBox(const AValue: TPaintBox);
  public
    Caption: string;
    Terminated: Boolean;
    FrameDuration: TDateTime;
    PaintObject: TPaintObject;
    OpenGLBitmap: Pointer;
    OpenGLControl: TOpenGLControl;
    TextureId: GLuint;
    procedure Init; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); virtual;
    procedure DrawFrameTiming(FastBitmap: TFastBitmap);
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property PaintBox: TPaintBox read FPaintBox write SetPaintBox;
  end;

  TDrawMethodClass = class of TDrawMethod;

  { TCanvasPixels }

  TCanvasPixels = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TCanvasPixelsUpdateLock }

  TCanvasPixelsUpdateLock = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TLazIntfImageColorsCopy }

  TLazIntfImageColorsCopy = class(TDrawMethod)
    TempIntfImage: TLazIntfImage;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TLazIntfImageColorsNoCopy }

  TLazIntfImageColorsNoCopy = class(TDrawMethod)
    TempIntfImage: TLazIntfImage;
    procedure SetBitmap(const AValue: TBitmap); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBitmapRawImageData }

  TBitmapRawImageData = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBitmapRawImageDataPaintBox }

  TBitmapRawImageDataPaintBox = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBGRABitmapPaintBox }

  TBGRABitmapPaintBox = class(TDrawMethod)
    BGRABitmap: TBGRABitmap;
    procedure SetBitmap(const AValue: TBitmap); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TOpenGLMethod }

  TOpenGLMethod = class(TDrawMethod)
    procedure SetBitmap(const AValue: TBitmap); override;
    constructor Create; override;
    procedure Init; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TOpenGLPBOMethod }

  TOpenGLPBOMethod = class(TDrawMethod)
    pboIds: array[0..1] of GLuint;
    Index, NextIndex: Integer;
    procedure SetBitmap(const AValue: TBitmap); override;
    procedure Init; override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

const
  DrawMethodClasses: array[0..8] of TDrawMethodClass = (
    TCanvasPixels, TCanvasPixelsUpdateLock, TLazIntfImageColorsCopy,
    TLazIntfImageColorsNoCopy, TBitmapRawImageData, TBitmapRawImageDataPaintBox,
    TBGRABitmapPaintBox, TOpenGLMethod, TOpenGLPBOMethod);

implementation

{ TOpenGLPBOMethod }

procedure TOpenGLPBOMethod.SetBitmap(const AValue: TBitmap);
begin
  inherited SetBitmap(AValue);
end;

//procedure glGenBuffersARB2 : procedure(n : GLsizei; buffers : PGLuint); extdecl;

procedure TOpenGLPBOMethod.Init;
var
  DataSize: Integer;
  glExtensions: string;
begin
  OpenGLControl.MakeCurrent;
  DataSize := OpenGLControl.Width * OpenGLControl.Height * SizeOf(Integer);
//  glGenBuffersARB(Length(pboIds), PGLuint(pboIds));
  //if glext_LoadExtension('GL_ARB_pixel_buffer_object') then
  if Load_GL_ARB_vertex_buffer_object then begin
    glGenBuffersARB(2, @pboIds);
    glBindBufferARB(GL_PIXEL_PACK_BUFFER_ARB, pboIds[0]);
    glBufferDataARB(GL_PIXEL_PACK_BUFFER_ARB, DataSize, Pointer(0), GL_STREAM_READ_ARB);
    glBindBufferARB(GL_PIXEL_PACK_BUFFER_ARB, pboIds[1]);
    glBufferDataARB(GL_PIXEL_PACK_BUFFER_ARB, DataSize, Pointer(0), GL_STREAM_READ_ARB);

  end else raise Exception.Create('GL_ARB_pixel_buffer_object not supported');

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, TextureId);
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, OpenGLControl.Width, OpenGLControl.Height,
      0, GL_RGBA, GL_UNSIGNED_BYTE, OpenGLBitmap);
end;

constructor TOpenGLPBOMethod.Create;
begin
  inherited Create;
  Caption := 'OpenGL PBO';
  PaintObject := poOpenGL;
//  SetLength(pboIds, 2);
  Index := 0;
  NextIndex := 1;
end;

destructor TOpenGLPBOMethod.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGLPBOMethod.DrawFrame(FastBitmap: TFastBitmap);
var
  X, Y: Integer;
  P: PInteger;
  R: PInteger;
  Ptr: ^GLubyte;
  TextureShift: TPoint;
  TextureShift2: TPoint;
const
  GL_CLAMP_TO_EDGE = $812F;
begin
  // "index" is used to read pixels from framebuffer to a PBO
  // "nextIndex" is used to update pixels in the other PBO
  Index := (Index + 1) mod 2;
  NextIndex := (Index + 1) mod 2;

  glLoadIdentity;

  glBindTexture(GL_TEXTURE_2D, TextureId);
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    //glTexImage2D(GL_TEXTURE_2D, 0, 4, OpenGLControl.Width, OpenGLControl.Height,
    //  0, GL_RGBA, GL_UNSIGNED_BYTE, OpenGLBitmap);
    //glTexImage2D(GL_TEXTURE_2D, 0, 4, 512, 256,
    //0, GL_RGBA, GL_UNSIGNED_BYTE, OpenGLBitmap);

  // bind the texture and PBO
  //glBindTexture(GL_TEXTURE_2D, textureId);
  glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[index]);

  // copy pixels from PBO to texture object
  // Use offset instead of ponter.
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, OpenGLControl.Width, OpenGLControl.Height,
    GL_BGRA, GL_UNSIGNED_BYTE, Pointer(0));


  // bind PBO to update texture source
  glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, pboIds[nextIndex]);

  // Note that glMapBufferARB() causes sync issue.
  // If GPU is working with this buffer, glMapBufferARB() will wait(stall)
  // until GPU to finish its job. To avoid waiting (idle), you can call
  // first glBufferDataARB() with NULL pointer before glMapBufferARB().
  // If you do that, the previous data in PBO will be discarded and
  // glMapBufferARB() returns a new allocated pointer immediately
  // even if GPU is still working with the previous data.
  glBufferDataARB(GL_PIXEL_UNPACK_BUFFER_ARB, OpenGLControl.Width * OpenGLControl.Height * SizeOf(Integer), Pointer(0), GL_STREAM_DRAW_ARB);

  // map the buffer object into client's memory
  ptr := glMapBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, GL_WRITE_ONLY_ARB);
  if Assigned(ptr) then begin
    // update data directly on the mapped buffer
    P := PInteger(Ptr);
    with FastBitmap do
    for Y := 0 to Size.Y - 2 do begin
      R := P;
      for X := 0 to Size.X - 1 do begin
        R^ := NoSwapBRComponent(Pixels[X, Y]) or $ff000000;
        Inc(R);
      end;
      Inc(P, Size.X);
    end;
    glUnmapBufferARB(GL_PIXEL_PACK_BUFFER_ARB);
  end;

  // it is good idea to release PBOs with ID 0 after use.
  // Once bound with 0, all pixel operations are back to normal ways.
  glBindBufferARB(GL_PIXEL_UNPACK_BUFFER_ARB, 0);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  //glRotatef(30.0, 0, 0, 1.0);
  glTranslatef(-OpenGLControl.Width / 2, -OpenGLControl.Height / 2, 0.0);
  glBindTexture(GL_TEXTURE_2D, TextureId);

  TextureShift := Point(0, 0);
  TextureShift2 := Point(0, 0);
  glBegin(GL_QUADS);
    glColor3ub(255, 255, 255);
    glTexCoord2f(TextureShift.X, TextureShift.Y);
    glVertex3f(TextureShift2.X, TextureShift2.Y, 0);
    glTexCoord2f(TextureShift.X + OpenGLControl.Width div 2, TextureShift.Y);
    glVertex3f(TextureShift2.X + OpenGLControl.Width, TextureShift2.Y, 0);
    glTexCoord2f(TextureShift.X + OpenGLControl.Width div 2, TextureShift.Y + OpenGLControl.Height div 2);
    glVertex3f(TextureShift2.X + OpenGLControl.Width, TextureShift2.Y + OpenGLControl.Height, 0);
    glTexCoord2f(TextureShift.X, TextureShift.Y + OpenGLControl.Height div 2);
    glVertex3f(TextureShift2.X, TextureShift2.Y + OpenGLControl.Height, 0);
  glEnd();

  OpenGLControl.SwapBuffers;
end;

{ TOpenGLMethod }

procedure TOpenGLMethod.SetBitmap(const AValue: TBitmap);
begin
  inherited SetBitmap(AValue);
end;

constructor TOpenGLMethod.Create;
begin
  inherited Create;
  Caption := 'OpenGL';
  PaintObject := poOpenGL;
end;

procedure TOpenGLMethod.Init;
begin
  inherited Init;
  //OpenGLControl.MakeCurrent;
end;

destructor TOpenGLMethod.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGLMethod.DrawFrame(FastBitmap: TFastBitmap);
var
  X, Y: Integer;
  P: PInteger;
  R: PInteger;
const
  GL_CLAMP_TO_EDGE = $812F;
begin
  glLoadIdentity;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  //glLoadIdentity;             { clear the matrix }
  //glTranslatef(0.0, 0.0, -3.0);  // -2.5); { viewing transformation }

  P := OpenGLBitmap;
  with FastBitmap do
  for Y := 0 to Size.Y - 1 do begin
    R := P;
    for X := 0 to Size.X - 1 do begin
      //R^ := Round($ff * (Y / Size.Y)) or $ff000000;
      R^  := NoSwapBRComponent(Pixels[X, Y]) or $ff000000;
      Inc(R);
    end;
    Inc(P, Size.X);
  end;

    //glRotatef(30.0, 0, 0, 1.0);
    glTranslatef(-OpenGLControl.Width div 2, -OpenGLControl.Height div 2, 0.0);

    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D, TextureId);
      //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      //glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, OpenGLControl.Width, OpenGLControl.Height,
        0, GL_RGBA, GL_UNSIGNED_BYTE, OpenGLBitmap);
      //glTexImage2D(GL_TEXTURE_2D, 0, 4, 512, 256,
      //0, GL_RGBA, GL_UNSIGNED_BYTE, OpenGLBitmap);

    //Define how alpha blending will work and enable alpha blending.
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    //glEnable(GL_BLEND);

    glBegin(GL_QUADS);
    //glBegin(GL_POLYGON);
      glColor3ub(255, 255, 255);
      glTexCoord2f(0, 0);
      glVertex3f(0, 0, 0);
      glTexCoord2f(OpenGLControl.Width div 2, 0);
      glVertex3f(OpenGLControl.Width, 0, 0);
      glTexCoord2f(OpenGLControl.Width div 2, OpenGLControl.Height div 2);
      glVertex3f(OpenGLControl.Width, OpenGLControl.Height, 0);
      glTexCoord2f(0, OpenGLControl.Height div 2);
      glVertex3f(0, OpenGLControl.Height, 0);
    glEnd();

  OpenGLControl.SwapBuffers;
end;

{ TBGRABitmapPaintBox }

procedure TBGRABitmapPaintBox.SetBitmap(const AValue: TBitmap);
begin
  inherited;
  BGRABitmap.SetSize(Bitmap.Width, Bitmap.Height);
end;

constructor TBGRABitmapPaintBox.Create;
begin
  inherited;
  Caption := 'TBGRABitmap PaintBox';
  BGRABitmap := TBGRABitmap.Create(0, 0);
  PaintObject := poPaintBox;
end;

destructor TBGRABitmapPaintBox.Destroy;
begin
  BGRABitmap.Free;
  inherited Destroy;
end;

procedure TBGRABitmapPaintBox.DrawFrame(FastBitmap: TFastBitmap);
var
  X, Y: Integer;
  P: PInteger;
begin
  with FastBitmap do
  for Y := 0 to Size.Y - 1 do begin
    P := PInteger(BGRABitmap.ScanLine[Y]);
    for X := 0 to Size.X - 1 do begin
      P^ := NoSwapBRComponent(Pixels[X, Y]) or $ff000000;
      (*P^.red := Pixels[X, Y];
      P^.green := Pixels[X, Y];
      P^.blue := Pixels[X, Y];
      P^.alpha := 255; *)
      Inc(P);
    end;
  end;
  BGRABitmap.InvalidateBitmap; // changed by direct access
  //BGRABitmap.Draw(Bitmap.Canvas, 0, 0, True);
  BGRABitmap.Draw(PaintBox.Canvas, 0, 0, True);
//  Bitmap.RawImage.Ass
end;

{ TBitmapRawImageDataPaintBox }

constructor TBitmapRawImageDataPaintBox.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data PaintBox';
  PaintObject := poPaintBox;
end;

procedure TBitmapRawImageDataPaintBox.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PInteger;
  RowPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  BytePerRow: Integer;
  hPaint, hBmp: HDC;
begin
  P := TempBitmap.PixelFormat;
    with FastBitmap do
    try
      TempBitmap.BeginUpdate(False);
      RawImage := TempBitmap.RawImage;
      RowPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      BytePerRow := RawImage.Description.BytesPerLine;
      for Y := 0 to Size.Y - 1 do begin
        PixelPtr := RowPtr;
        for X := 0 to Size.X - 1 do begin
          PixelPtr^ := Pixels[X, Y];
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
        Inc(PByte(RowPtr), BytePerRow);
      end;
    finally
      TempBitmap.EndUpdate(False);
    end;
    hBmp := TempBitmap.Canvas.Handle;
    hPaint := PaintBox.Canvas.Handle;
    PaintBox.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), TempBitmap.Canvas,
      Rect(0, 0, TempBitmap.Width, TempBitmap.Height));
    //BitBlt(hPaint, 0, 0, TempBitmap.Width, TempBitmap.Height, hBmp, 0, 0, srcCopy);
end;

{ TBitmapRawImageData }

constructor TBitmapRawImageData.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data';
end;

procedure TBitmapRawImageData.DrawFrame(FastBitmap: TFastBitmap);
type
  TFastBitmapPixelComponents = packed record
  end;
var
  Y, X: Integer;
  PixelPtr: PInteger;
  RowPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  BytePerRow: Integer;
begin
  P := Bitmap.PixelFormat;
    with FastBitmap do
    try
      Bitmap.BeginUpdate(False);
      RawImage := Bitmap.RawImage;
      RowPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      BytePerRow := RawImage.Description.BytesPerLine;
      for Y := 0 to Size.Y - 1 do begin
        PixelPtr := RowPtr;
        for X := 0 to Size.X - 1 do begin
          PixelPtr^ := Pixels[X, Y];
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
        Inc(PByte(RowPtr), BytePerRow);
      end;
    finally
      Bitmap.EndUpdate(False);
    end;
end;

{ TLazIntfImageColorsNoCopy }

procedure TLazIntfImageColorsNoCopy.SetBitmap(const AValue: TBitmap);
begin
  inherited SetBitmap(AValue);
  TempIntfImage.Free;
  TempIntfImage := Bitmap.CreateIntfImage;
end;

constructor TLazIntfImageColorsNoCopy.Create;
begin
  inherited;
  Caption := 'TLazIntfImage.Colors no copy';
end;

destructor TLazIntfImageColorsNoCopy.Destroy;
begin
  TempIntfImage.Free;
  inherited Destroy;
end;

procedure TLazIntfImageColorsNoCopy.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
begin
  with FastBitmap do begin
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        TempIntfImage.Colors[X, Y] := TColorToFPColor(SwapBRComponent(Pixels[X, Y]));
    Bitmap.LoadFromIntfImage(TempIntfImage);
  end;
end;

{ TLazIntfImageColorsCopy }

constructor TLazIntfImageColorsCopy.Create;
begin
  inherited;
  Caption := 'TLazIntfImage.Colors copy';
  TempIntfImage := TLazIntfImage.Create(0, 0);
end;

destructor TLazIntfImageColorsCopy.Destroy;
begin
  TempIntfImage.Free;
  inherited Destroy;
end;

procedure TLazIntfImageColorsCopy.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
begin
  with FastBitmap do begin
    TempIntfImage.LoadFromBitmap(Bitmap.Handle,
      Bitmap.MaskHandle);
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        TempIntfImage.Colors[X, Y] := TColorToFPColor(SwapBRComponent(Pixels[X, Y]));
    Bitmap.LoadFromIntfImage(TempIntfImage);
  end;
end;

{ TCanvasPixelsUpdateLock }

constructor TCanvasPixelsUpdateLock.Create;
begin
  inherited;
  Caption := 'TBitmap.Canvas.Pixels Update locking';
end;

procedure TCanvasPixelsUpdateLock.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
begin
  with FastBitmap do
  try
    Bitmap.BeginUpdate(True);
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        Bitmap.Canvas.Pixels[X, Y] := SwapBRComponent(Pixels[X, Y]);
  finally
    Bitmap.EndUpdate(False);
  end;
end;

{ TCanvasPixels }

constructor TCanvasPixels.Create;
begin
  inherited;
  Caption := 'TBitmap.Canvas.Pixels';
end;

procedure TCanvasPixels.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
begin
  with FastBitmap do begin
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        Bitmap.Canvas.Pixels[X, Y] := SwapBRComponent(Pixels[X, Y]);
  end;
end;

{ TDrawMethod }

procedure TDrawMethod.SetBitmap(const AValue: TBitmap);
begin
  if FBitmap = AValue then exit;
  FBitmap := AValue;
  TempBitmap.SetSize(FBitmap.Width, FBitmap.Height);
end;

procedure TDrawMethod.SetPaintBox(const AValue: TPaintBox);
begin
  if FPaintBox = AValue then Exit;
  FPaintBox := AValue;
end;

procedure TDrawMethod.Init;
begin

end;

constructor TDrawMethod.Create;
begin
  TempBitmap := TBitmap.Create;
end;

destructor TDrawMethod.Destroy;
begin
  TempBitmap.Free;
  inherited Destroy;
end;

procedure TDrawMethod.DrawFrame(FastBitmap: TFastBitmap);
begin

end;

procedure TDrawMethod.DrawFrameTiming(FastBitmap: TFastBitmap);
var
  StartTime: TDateTime;
begin
  StartTime := NowPrecise;
  DrawFrame(FastBitmap);
  FrameDuration := NowPrecise - StartTime;
end;

end.
