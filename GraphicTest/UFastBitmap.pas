unit UFastBitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
 { TFastBitmap }

  TFastBitmap = class
  private
    function GetSize: TPoint;
    procedure SetSize(const AValue: TPoint);
  public
    Pixels: array of array of Byte;
    procedure RandomImage;
    property Size: TPoint read GetSize write SetSize;
  end;

implementation

{ TFastBitmap }

function TFastBitmap.GetSize: TPoint;
begin
  Result.X := Length(Pixels);
  if Result.X > 0 then Result.Y := Length(Pixels[0])
    else Result.Y := 0;
end;

procedure TFastBitmap.SetSize(const AValue: TPoint);
begin
  SetLength(Pixels, AValue.X, AValue.Y);
end;

procedure TFastBitmap.RandomImage;
var
  X, Y: Integer;
begin
  for Y := 0 to Size.Y - 1 do
    for X := 0 to Size.X - 1 do
      Pixels[X, Y] := Random(256);
end;


end.

