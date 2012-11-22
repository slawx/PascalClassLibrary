unit UDrawMethod;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, UPlatform, UFastBitmap, Graphics, Controls,
  LCLType, IntfGraphics, fpImage, GraphType, BGRABitmap, BGRABitmapTypes,
  LclIntf{$IFDEF opengl}, GL, GLExt, OpenGLContext{$ENDIF};

type
  TPaintObject = (poImage, poPaintBox, poOpenGL);


  { TDrawMethod }

  TDrawMethod = class
  private
    FControl: TControl;
    TempBitmap: TBitmap;
  public
    Caption: string;
    Terminated: Boolean;
    FrameDuration: TDateTime;
    StepDuration: TDateTime;
    PaintObject: TPaintObject;
    procedure Init(Parent: TWinControl; Size: TPoint); virtual;
    procedure Done; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); virtual;
    procedure DrawFrameTiming(FastBitmap: TFastBitmap);
    property Control: TControl read FControl;
  end;

  TDrawMethodClass = class of TDrawMethod;

  { TDrawMethodImage }

  TDrawMethodImage = class(TDrawMethod)
    Image: TImage;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  { TDrawMethodPaintBox }

  TDrawMethodPaintBox = class(TDrawMethod)
    PaintBox: TPaintBox;
    procedure Paint(Sender: TObject); virtual;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  { TDummyMethod }

  TDummyMethod = class(TDrawMethod)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TCanvasPixels }

  TCanvasPixels = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TCanvasPixelsUpdateLock }

  TCanvasPixelsUpdateLock = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TLazIntfImageColorsCopy }

  TLazIntfImageColorsCopy = class(TDrawMethodImage)
    TempIntfImage: TLazIntfImage;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TLazIntfImageColorsNoCopy }

  TLazIntfImageColorsNoCopy = class(TDrawMethodImage)
    TempIntfImage: TLazIntfImage;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBitmapRawImageData }

  TBitmapRawImageData = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBitmapRawImageDataPaintBox }

  TBitmapRawImageDataPaintBox = class(TDrawMethodPaintBox)
    constructor Create; override;
    procedure Paint(Sender: TObject); override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBitmapRawImageDataMove }

  TBitmapRawImageDataMove = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TBGRABitmapPaintBox }

  TBGRABitmapPaintBox = class(TDrawMethodPaintBox)
    BGRABitmap: TBGRABitmap;
    procedure Paint(Sender: TObject); override;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  {$IFDEF opengl}
  { TDrawMethodOpenGL }

  TDrawMethodOpenGL = class(TDrawMethod)
    OpenGLControl: TOpenGLControl;
    TextureId: GLuint;
    OpenGLBitmap: Pointer;
    procedure InitGL;
    procedure OpenGLControlResize(Sender: TObject);
    procedure Init(AParent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  { TOpenGLMethod }

  TOpenGLMethod = class(TDrawMethodOpenGL)
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;

  { TOpenGLPBOMethod }

  TOpenGLPBOMethod = class(TDrawMethodOpenGL)
    pboIds: array[0..1] of GLuint;
    Index, NextIndex: Integer;
    procedure Init(AParent: TWinControl; Size: TPoint); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;
  {$ENDIF}

const
  DrawMethodClasses: array[0..{$IFDEF opengl}10{$ELSE}8{$ENDIF}] of TDrawMethodClass = (
    TCanvasPixels, TCanvasPixelsUpdateLock, TLazIntfImageColorsCopy,
    TLazIntfImageColorsNoCopy, TBitmapRawImageData, TBitmapRawImageDataPaintBox,
    TBitmapRawImageDataMove, TBGRABitmapPaintBox{$IFDEF opengl}, TOpenGLMethod, TOpenGLPBOMethod{$ENDIF}
    ,TDummyMethod);

implementation


{ TDrawMethodPaintBox }

procedure TDrawMethodPaintBox.Paint(Sender: TObject);
begin

end;

procedure TDrawMethodPaintBox.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  PaintBox := TPaintBox.Create(Parent);
  PaintBox.Parent := Parent;
  PaintBox.SetBounds(0, 0, Size.X, Size.Y);
  PaintBox.OnPaint := Paint;
  PaintBox.Show;
end;

procedure TDrawMethodPaintBox.Done;
begin
  FreeAndNil(PaintBox);
  inherited Done;
end;

{ TDrawMethodImage }

procedure TDrawMethodImage.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  Image := TImage.Create(Parent);
  Image.Parent := Parent;
  Image.SetBounds(0, 0, Size.X, Size.Y);
  Image.Picture.Bitmap.SetSize(Size.X, Size.Y);
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Show;
end;

procedure TDrawMethodImage.Done;
begin
  FreeAndNil(Image);
  inherited Done;
end;

{ TDummyMethod }

constructor TDummyMethod.Create;
begin
  inherited Create;
  Caption := 'Dummy';
end;

procedure TDummyMethod.DrawFrame(FastBitmap: TFastBitmap);
begin
end;

{ TBitmapRawImageDataMove }

constructor TBitmapRawImageDataMove.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data Move';
end;

procedure TBitmapRawImageDataMove.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PInteger;
  RowPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  BytePerRow: Integer;
begin
  P := Image.Picture.Bitmap.PixelFormat;
    with FastBitmap do
    try
      Image.Picture.Bitmap.BeginUpdate(False);
      RawImage := Image.Picture.Bitmap.RawImage;
      RowPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      BytePerRow := RawImage.Description.BytesPerLine;
      Move(FastBitmap.PixelsData^, RowPtr^, Size.Y * BytePerRow);
    finally
      Image.Picture.Bitmap.EndUpdate(False);
    end;
end;

{$IFDEF opengl}
{ TOpenGLPBOMethod }

//procedure glGenBuffersARB2 : procedure(n : GLsizei; buffers : PGLuint); extdecl;

procedure TOpenGLPBOMethod.Init(AParent: TWinControl; Size: TPoint);
var
  DataSize: Integer;
  glExtensions: string;
begin
  inherited;

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
  P: PCardinal;
  R: PCardinal;
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
    P := PCardinal(Ptr);
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

constructor TOpenGLMethod.Create;
begin
  inherited Create;
  Caption := 'OpenGL';
  PaintObject := poOpenGL;
end;

destructor TOpenGLMethod.Destroy;
begin
  inherited Destroy;
end;

procedure TOpenGLMethod.DrawFrame(FastBitmap: TFastBitmap);
var
  X, Y: Integer;
  P: PCardinal;
  R: PCardinal;
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

{ TDrawMethodOpenGL }

procedure TDrawMethodOpenGL.Init(AParent: TWinControl; Size: TPoint);
begin
  inherited Init(aParent, Size);
  OpenGLControl := TOpenGLControl.Create(AParent);
  with OpenGLControl do begin
    Name := 'OpenGLControl';
    Parent := AParent;
    SetBounds(0, 0, Size.X, Size.Y);
    InitGL;
    //OnPaint := OpenGLControl1Paint;
    OnResize := OpenGLControlResize;
  end;
  GetMem(OpenGLBitmap, OpenGLControl.Width * OpenGLControl.Height * SizeOf(Integer));
end;

procedure TDrawMethodOpenGL.Done;
begin
  FreeMem(OpenGLBitmap, OpenGLControl.Width * OpenGLControl.Height);
  FreeAndNil(OpenGLControl);
  inherited;
end;

procedure TDrawMethodOpenGL.OpenGLControlResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);
end;

procedure TDrawMethodOpenGL.InitGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, OpenGLControl.Width, OpenGLControl.Height, 0, 0, 1);
//  glOrtho(0, 1, 1, 0, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glDisable(GL_DEPTH_TEST);
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);
  //gluPerspective( 45.0, (GLfloat)(OpenGLControl1.Width)/(GLfloat)(OpenGLControl1.Height), 0.1f, 500.0 );

    //glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
    //glTranslatef (0.0, 0.0,-3.0);
  //  glClearColor(0.0, 0.0, 0.0, 1.0);

  glGenTextures(1, @TextureId);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
end;

{$ENDIF}

{ TBGRABitmapPaintBox }

procedure TBGRABitmapPaintBox.Paint(Sender: TObject);
begin
  //BGRABitmap.Draw(Bitmap.Canvas, 0, 0, True);
  BGRABitmap.Draw(PaintBox.Canvas, 0, 0, True);
end;

procedure TBGRABitmapPaintBox.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  BGRABitmap.SetSize(PaintBox.Width, PaintBox.Height);
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
  P: PCardinal;
begin
  with FastBitmap do
  for Y := 0 to Size.Y - 1 do begin
    P := PCardinal(BGRABitmap.ScanLine[Y]);
    for X := 0 to Size.X - 1 do begin
      P^ := NoSwapBRComponent(Pixels[X, Y]) or $ff000000;
      //P^ := Pixels[X, Y] or $ff000000;
      (*P^.red := Pixels[X, Y];
      P^.green := Pixels[X, Y];
      P^.blue := Pixels[X, Y];
      P^.alpha := 255; *)
      Inc(P);
    end;
  end;
  BGRABitmap.InvalidateBitmap; // changed by direct access
  PaintBox.Repaint;
end;

{ TBitmapRawImageDataPaintBox }

constructor TBitmapRawImageDataPaintBox.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data PaintBox';
  PaintObject := poPaintBox;
end;

procedure TBitmapRawImageDataPaintBox.Paint(Sender: TObject);
var
  hPaint, hBmp: HDC;
begin
  hBmp := TempBitmap.Canvas.Handle;
  hPaint := PaintBox.Canvas.Handle;
  PaintBox.Canvas.CopyRect(Rect(0, 0, PaintBox.Width, PaintBox.Height), TempBitmap.Canvas,
    Rect(0, 0, TempBitmap.Width, TempBitmap.Height));
 // PaintBox.Canvas.Draw(0, 0, TempBitmap);
  //BitBlt(hPaint, 0, 0, TempBitmap.Width, TempBitmap.Height, hBmp, 0, 0, srcCopy);
end;

procedure TBitmapRawImageDataPaintBox.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PCardinal;
  RowPtr: PCardinal;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  BytePerRow: Integer;
begin
  P := TempBitmap.PixelFormat;
    with FastBitmap do
    try
      TempBitmap.BeginUpdate(False);
      RawImage := TempBitmap.RawImage;
      RowPtr := PCardinal(RawImage.Data);
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
  PaintBox.Repaint;
end;

{ TBitmapRawImageData }

constructor TBitmapRawImageData.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data';
end;

procedure TBitmapRawImageData.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PCardinal;
  RowPtr: PCardinal;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  BytePerRow: Integer;
begin
  P := Image.Picture.Bitmap.PixelFormat;
    with FastBitmap do
    try
      Image.Picture.Bitmap.BeginUpdate(False);
      RawImage := Image.Picture.Bitmap.RawImage;
      RowPtr := PCardinal(RawImage.Data);
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
      Image.Picture.Bitmap.EndUpdate(False);
    end;
end;

{ TLazIntfImageColorsNoCopy }

procedure TLazIntfImageColorsNoCopy.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  TempIntfImage.Free;
  TempIntfImage := Image.Picture.Bitmap.CreateIntfImage;
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
    Image.Picture.Bitmap.LoadFromIntfImage(TempIntfImage);
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
    TempIntfImage.LoadFromBitmap(Image.Picture.Bitmap.Handle,
      Image.Picture.Bitmap.MaskHandle);
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        TempIntfImage.Colors[X, Y] := TColorToFPColor(SwapBRComponent(Pixels[X, Y]));
    Image.Picture.Bitmap.LoadFromIntfImage(TempIntfImage);
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
    Image.Picture.Bitmap.BeginUpdate(True);
    for Y := 0 to Size.Y - 1 do
      for X := 0 to Size.X - 1 do
        Image.Picture.Bitmap.Canvas.Pixels[X, Y] := TColor(SwapBRComponent(Pixels[X, Y]));
  finally
    Image.Picture.Bitmap.EndUpdate(False);
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
    for Y := 0 to Size.Y - 1 do
      for X := 0 to Size.X - 1 do
        Image.Picture.Bitmap.Canvas.Pixels[X, Y] := TColor(SwapBRComponent(Pixels[X, Y]));
  end;
end;

{ TDrawMethod }

procedure TDrawMethod.Init(Parent: TWinControl; Size: TPoint);
begin
  if (TempBitmap.Width <> Size.X) or (TempBitmap.Height <> Size.Y) then
    TempBitmap.SetSize(Size.X, Size.Y);
end;

procedure TDrawMethod.Done;
begin

end;

constructor TDrawMethod.Create;
begin
  TempBitmap := TBitmap.Create;
end;

destructor TDrawMethod.Destroy;
begin
  FreeAndNil(TempBitmap);
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

