unit GenericPoint;

{$mode delphi}

interface

type
  TGPoint<T> = record
    X: T;
    Y: T;
    procedure Add(Point: TGPoint<T>);
 end;


implementation

procedure TGPoint<T>.Add(Point: TGPoint<T>);
begin
  X := X + Point.X;
  Y := Y + Point.Y;
end;

end.
