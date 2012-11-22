unit ULazIntfImageColorsCopy;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, IntfGraphics, GraphType,
  fpImage, Graphics;

type
  { TLazIntfImageColorsCopy }

  TLazIntfImageColorsCopy = class(TDrawMethodImage)
    TempIntfImage: TLazIntfImage;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

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


end.

