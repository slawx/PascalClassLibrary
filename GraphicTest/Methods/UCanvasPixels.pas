unit UCanvasPixels;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, Graphics;

type
  { TCanvasPixels }

  TCanvasPixels = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

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


end.

