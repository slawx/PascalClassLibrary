unit UBitmapRawImageDataMove;

{$mode delphi}

interface

uses
  Classes, SysUtils, UDrawMethod, UFastBitmap, Graphics, GraphType;

type
  { TBitmapRawImageDataMove }

  TBitmapRawImageDataMove = class(TDrawMethodImage)
    constructor Create; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); override;
  end;


implementation

{ TBitmapRawImageDataMove }

constructor TBitmapRawImageDataMove.Create;
begin
  inherited;
  Caption := 'TBitmap.RawImage.Data Move';
  Description.Add('This is same as BitmapRawImageData but data is not converted from different format. ' +
   'But only moved to TImage raw data. ' +
    'Then TImage is responsible for show loaded data.');
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


end.

