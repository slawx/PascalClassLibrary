unit GenericPoint;

{$mode delphi}{$H+}

interface

uses
  Classes;

type

  { TGPoint }

  TGPoint<TDimension> = record
  private
    function GetTPoint: TPoint;
    procedure SetTPoint(AValue: TPoint);
  public
    X: TDimension;
    Y: TDimension;
    property AsTPoint: TPoint read GetTPoint write SetTPoint;
  end;


implementation

{ TGPoint<TDimension> }

function TGPoint<TDimension>.GetTPoint: TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TGPoint<TDimension>.SetTPoint(AValue: TPoint);
begin
  X := AValue.X;
  Y := AValue.Y;
end;

end.

