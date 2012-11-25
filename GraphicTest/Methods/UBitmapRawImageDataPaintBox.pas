unit UBitmapRawImageDataPaintBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, Graphics, LCLType,
  FPimage, IntfGraphics, GraphType{$IFDEF windows}, Windows{$ENDIF};

type
  { TBitmapRawImageDataPaintBox }

  TBitmapRawImageDataPaintBox = class(TDrawMethodPaintBox)
    constructor Create; override;
    procedure Paint(Sender: TObject); override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

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
  //PaintBox.Canvas.CopyRect(Rect(0, 0, PaintBox.Width, PaintBox.Height), TempBitmap.Canvas,
  //  Rect(0, 0, TempBitmap.Width, TempBitmap.Height));
  PaintBox.Canvas.Draw(0, 0, TempBitmap);
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


end.
