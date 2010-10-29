
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
