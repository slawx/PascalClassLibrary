unit GenericRange;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericList;

type
  TGRange<TItem> = class
  private
    procedure SetDistance(const AValue: TItem);
    function GetDistance: TItem;
  public
    A: TItem;
    B: TItem;
    procedure SetRange(NewA, NewB: TItem);
    property Distance: TItem read GetDistance write SetDistance;
  end;

implementation

{ TGRange }

procedure TGRange<TItem>.SetDistance(const AValue: TItem);
begin
  B := A + AValue;
end;

function TGRange<TItem>.GetDistance: TItem;
begin
  Result := B - A;
end;

procedure TGRange<TItem>.SetRange(NewA, NewB: TItem);
begin
  if NewA > NewB then begin
    A := NewB;
    B := NewA;
  end else begin
    A := NewA;
    B := NewB;
  end;
end;

end.
