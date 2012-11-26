unit ULazIntfImageColorsNoCopy;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, IntfGraphics, Graphics, Controls;

type
  { TLazIntfImageColorsNoCopy }

  TLazIntfImageColorsNoCopy = class(TDrawMethodImage)
    TempIntfImage: TLazIntfImage;
    procedure Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

{ TLazIntfImageColorsNoCopy }

procedure TLazIntfImageColorsNoCopy.Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat);
begin
  inherited;
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


end.

