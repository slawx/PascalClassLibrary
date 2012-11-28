unit UBGRABitmapPaintBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, BGRABitmap, BGRABitmapTypes,
  Controls, Graphics;

type
  { TBGRABitmapPaintBox }

  TBGRABitmapPaintBox = class(TDrawMethodPaintBox)
    BGRABitmap: TBGRABitmap;
    procedure Paint(Sender: TObject); override;
    procedure Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

{ TBGRABitmapPaintBox }

procedure TBGRABitmapPaintBox.Paint(Sender: TObject);
begin
  //BGRABitmap.Draw(Bitmap.Canvas, 0, 0, True);
  BGRABitmap.Draw(PaintBox.Canvas, 0, 0, True);
end;

procedure TBGRABitmapPaintBox.Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat);
begin
  inherited;
  BGRABitmap.SetSize(PaintBox.Width, PaintBox.Height);
end;

constructor TBGRABitmapPaintBox.Create;
begin
  inherited;
  Caption := 'TBGRABitmap PaintBox';
  BGRABitmap := TBGRABitmap.Create(0, 0);
  PaintObject := poPaintBox;
  Description.Add('This method use graphic library BGRABitmap. ' +
    'PaintBox is used to display image data.');
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


end.

