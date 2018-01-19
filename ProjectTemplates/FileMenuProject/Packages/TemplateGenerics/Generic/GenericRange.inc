{$IFDEF INTERFACE}

  // TGRange<TRangeItem> = class
  TGRange = class
  private
    procedure SetDistance(const AValue: TRangeItem);
    function GetDistance: TRangeItem;
  public
    A: TRangeItem;
    B: TRangeItem;
    procedure SetRange(NewA, NewB: TRangeItem);
    property Distance: TRangeItem read GetDistance write SetDistance;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

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

{$UNDEF IMPLEMENTATION}
{$ENDIF}