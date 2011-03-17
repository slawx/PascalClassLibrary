unit UDrawMethod;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UPlatform, UFastBitmap, Graphics,
  LCLType, IntfGraphics, fpImage, GraphType;

type

  { TDrawMethod }

  TDrawMethod = class
  public
    Caption: string;
    Terminated: Boolean;
    Bitmap: TBitmap;
    FrameDuration: TDateTime;
    constructor Create; virtual;
    procedure DrawFrame(FastBitmap: TFastBitmap); virtual;
    procedure DrawFrameTiming(FastBitmap: TFastBitmap);
    procedure DrawLoop;
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

const
  DrawMethodClasses: array[0..5] of TDrawMethodClass = (
    TCanvasPixels, TCanvasPixelsUpdateLock, TLazIntfImageColorsCopy,
    TLazIntfImageColorsNoCopy, TBitmapRawImageData, TBitmapRawImageDataPaintBox);

implementation

{ TBitmapRawImageDataPaintBox }

constructor TBitmapRawImageDataPaintBox.Create;
begin
  Caption := 'TBitmap.RawImage.Data PaintBox';
end;

procedure TBitmapRawImageDataPaintBox.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  hPaint, hBmp: HDC;
begin
  P := Bitmap.PixelFormat;
    with FastBitmap do
    try
      Bitmap.BeginUpdate(False);
      RawImage := Bitmap.RawImage;
      PixelPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do begin
          PixelPtr^ := Pixels[X, Y] * $010101;
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
    finally
      Bitmap.EndUpdate(False);
    end;
    hBmp := Bitmap.Canvas.Handle;
    //hPaint := PaintBox1.Canvas.Handle;
    //BitBlt(hPaint, 0, 0, Bitmap.Width, Bitmap.Height, hBmp, 0, 0, srcCopy);
end;

{ TBitmapRawImageData }

constructor TBitmapRawImageData.Create;
begin
  Caption := 'TBitmap.RawImage.Data';
end;

procedure TBitmapRawImageData.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
begin
  P := Bitmap.PixelFormat;
    with FastBitmap do
    try
      Bitmap.BeginUpdate(False);
      RawImage := Bitmap.RawImage;
      PixelPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do begin
          PixelPtr^ := Pixels[X, Y] * $010101;
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
    finally
      Bitmap.EndUpdate(False);
    end;
end;

{ TLazIntfImageColorsNoCopy }

constructor TLazIntfImageColorsNoCopy.Create;
begin
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
    if not Assigned(TempIntfImage) then
      TempIntfImage := Bitmap.CreateIntfImage;
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        TempIntfImage.Colors[X, Y] := TColorToFPColor(Pixels[X, Y] * $010101);
    Bitmap.LoadFromIntfImage(TempIntfImage);
  end;
end;

{ TLazIntfImageColorsCopy }

constructor TLazIntfImageColorsCopy.Create;
begin
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
        TempIntfImage.Colors[X, Y] := TColorToFPColor(Pixels[X, Y] * $010101);
    Bitmap.LoadFromIntfImage(TempIntfImage);
  end;
end;

{ TCanvasPixelsUpdateLock }

constructor TCanvasPixelsUpdateLock.Create;
begin
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
        Bitmap.Canvas.Pixels[X, Y] := Pixels[X, Y] * $010101;
  finally
    Bitmap.EndUpdate(False);
  end;
end;

{ TCanvasPixels }

constructor TCanvasPixels.Create;
begin
  Caption := 'TBitmap.Canvas.Pixels';
end;

procedure TCanvasPixels.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
begin
  with FastBitmap do begin
    for X := 0 to Size.X - 1 do
      for Y := 0 to Size.Y - 1 do
        Bitmap.Canvas.Pixels[X, Y] := Pixels[X, Y] * $010101;
  end;
end;

{ TDrawMethod }

constructor TDrawMethod.Create;
begin

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

procedure TDrawMethod.DrawLoop;
begin
end;

end.

