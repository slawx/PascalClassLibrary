
{ TGRange }

procedure TGRange.SetDistance(const AValue: TRangeItem);
begin
  B := A + AValue;
end;

function TGRange.GetDistance: TRangeItem;
begin
  Result := B - A;
end;

procedure TGRange.SetRange(NewA, NewB: TRangeItem);
begin
  if NewA > NewB then begin
    A := NewB;
    B := NewA;
  end else begin
    A := NewA;
    B := NewB;
  end;
end;



