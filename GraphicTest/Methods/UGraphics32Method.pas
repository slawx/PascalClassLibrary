unit UGraphics32Method;

{$mode delphi}

interface

uses
  Classes, SysUtils, UFastBitmap, UDrawMethod, GR32, GR32_Image, Controls,
  Graphics;

type
  { TGraphics32Method }

  TGraphics32Method = class(TDrawMethod)
    Image: TImage32;
    constructor Create; override;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
    procedure Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat); override;
    procedure Done; override;
  end;


implementation

{ TGraphics32Method }

constructor TGraphics32Method.Create;
begin
  inherited Create;
  Caption := 'TGR32Image';
  Description.Add('Graphics32 is well implemented highly optimized Delphi graphic ' +
    'library also ported to Lazarus/LCL. It uses static 32-bit wide pixel:');
  Description.Add('TColor32Entry = packed record');
  Description.Add('  case Integer of');
  Description.Add('    0: (B, G, R, A: Byte);');
  Description.Add('    1: (ARGB: TColor32);');
  Description.Add('    2: (Planes: array[0..3] of Byte);');
  Description.Add('    3: (Components: array[TColor32Component] of Byte);');
  Description.Add('end;');
end;

destructor TGraphics32Method.Destroy;
begin
  inherited Destroy;
end;

procedure TGraphics32Method.DrawFrame(FastBitmap: TFastBitmap);
var
  Y, X: Integer;
  PixelPtr: PColor32;
  RowPtr: PColor32;
  ImagePtr: PColor32;
  BytePerPixel: Integer;
  BytePerRow: Integer;
begin
    with FastBitmap do
    try
      Image.Bitmap.BeginUpdate;
      BytePerPixel := 4;
      BytePerRow := 4 * Image.Bitmap.Width;
      ImagePtr := Image.Bitmap.PixelPtr[0, 0];
      RowPtr := ImagePtr;
      for Y := 0 to Size.Y - 1 do begin
        PixelPtr := RowPtr;
        for X := 0 to Size.X - 1 do begin
          PixelPtr^ := Pixels[X, Y];
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
        Inc(PByte(RowPtr), BytePerRow);
      end;
    finally
      Image.Bitmap.EndUpdate;
    end;
  Image.Repaint;
end;

procedure TGraphics32Method.Init(Parent: TWinControl; Size: TPoint; PixelFormat: TPixelFormat);
begin
  inherited;
  Image := TImage32.Create(Parent);
  Image.Parent := Parent;
  Image.SetBounds(0, 0, Size.X, Size.Y);
  Image.Bitmap.SetSize(Size.X, Size.Y);
  Image.Show;
end;

procedure TGraphics32Method.Done;
begin
  FreeAndNil(Image);
  inherited Done;
end;

end.

