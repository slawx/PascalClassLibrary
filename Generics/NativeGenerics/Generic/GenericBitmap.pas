unit GenericBitmap;

{$mode delphi}

interface

uses
  GenericMatrix;

type
  TGBitmap<T> = class(TGMatrix<T>)
  private
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
  public
    property Pixels[X, Y: Integer]: T
      read GetItemXY write PutItemXY;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  end;


implementation

function TGBitmap<T>.GetWidth: Integer;
begin
  Result := Count.X;
end;

function TGBitmap<T>.GetHeight: Integer;
begin
  Result := Count.Y;
end;

procedure TGBitmap<T>.SetWidth(Value: Integer);
begin
  Count := CreateIndex(Value, Count.Y);
end;

procedure TGBitmap<T>.SetHeight(Value: Integer);
begin
  Count := CreateIndex(Count.X, Value);
end;

end.
