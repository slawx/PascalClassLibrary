{$IFDEF INTERFACE}

// TGPoint<TPointType> = class
TGPoint = class
  X: TGPointType;
  Y: TGPointType;
  procedure Add(Point: TGPoint);
end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TGPoint.Add(Point: TGPoint);
begin
  X := X + Point.X;
  Y := Y + Point.Y;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}

