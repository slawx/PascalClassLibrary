unit GenericRange;

{$mode delphi}

interface

type

  TGRange<T> = class
  private
    procedure SetDistance(const AValue: T);
    function GetDistance: T;
  public
    A: T;
    B: T;
    procedure SetRange(NewA, NewB: T);
    property Distance: T read GetDistance write SetDistance;
  end;


implementation

{ TGRange }

procedure TGRange<T>.SetDistance(const AValue: T);
begin
  B := A + AValue;
end;

function TGRange<T>.GetDistance: T;
begin
  Result := B - A;
end;

procedure TGRange<T>.SetRange(NewA, NewB: T);
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
