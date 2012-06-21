{$IFDEF INTERFACE}

// TGRectangle<TGRectangleDimension> = class
TGRectangle = class
private
  function GetBottomLeft: TGRectanglePoint;
  function GetBottomRight: TGRectanglePoint;
  function GetHeight: TGRectangleDimension;
  function GetSize: TGRectanglePoint;
  function GetTopLeft: TGRectanglePoint;
  function GetTopRight: TGRectanglePoint;
  function GetWidth: TGRectangleDimension;
  function GetEmpty: Boolean;
  procedure SetBottom(const AValue: TGRectangleDimension);
  procedure SetBottomLeft(const AValue: TGRectanglePoint);
  procedure SetBottomRight(const AValue: TGRectanglePoint);
  procedure SetHeight(const AValue: TGRectangleDimension);
  procedure SetLeft(const AValue: TGRectangleDimension);
  procedure SetRight(const AValue: TGRectangleDimension);
  procedure SetSize(const AValue: TGRectanglePoint);
  procedure SetTop(const AValue: TGRectangleDimension);
  procedure SetTopLeft(const AValue: TGRectanglePoint);
  procedure SetTopRight(const AValue: TGRectanglePoint);
  procedure SetWidth(const AValue: TGRectangleDimension);
  procedure SetEmpty(const AValue: Boolean);
public
  FLeft: TGRectangleDimension;
  FTop: TGRectangleDimension;
  FRight: TGRectangleDimension;
  FBottom: TGRectangleDimension;
  KeepSize: Boolean;

  procedure Assign(Source: TGRectangle);
  function IsPointInside(Pos: TGRectanglePoint): Boolean;
  function IsRectInside(Rect: TGRectangle): Boolean;
  procedure Intersect(Rect1, Rect2: TGRectangle);
  procedure IntersectWith(Rect: TGRectangle);
  procedure Union(Rect1, Rect2: TGRectangle);
  procedure UnionWith(Rect: TGRectangle);

  procedure SetRect(Left, Top, Width, Height: TGRectangleDimension);
  procedure SetBounds(Left, Top, Right, Bottom: TGRectangleDimension);

  property Left: TGRectangleDimension read FLeft write SetLeft;
  property Top: TGRectangleDimension read FTop write SetTop;
  property Right: TGRectangleDimension read FRight write SetRight;
  property Bottom: TGRectangleDimension read FBottom write SetBottom;

  property Width: TGRectangleDimension read GetWidth write SetWidth;
  property Height: TGRectangleDimension read GetHeight write SetHeight;

  property TopLeft: TGRectanglePoint read GetTopLeft write SetTopLeft;
  property TopRight: TGRectanglePoint read GetTopRight write SetTopRight;
  property BottomLeft: TGRectanglePoint read GetBottomLeft write SetBottomLeft;
  property BottomRight: TGRectanglePoint read GetBottomRight write SetBottomRight;

  property Size: TGRectanglePoint read GetSize write SetSize;
  property Empty: Boolean read GetEmpty write SetEmpty;
end;


{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION_USES}

uses Math;

{$UNDEF IMPLEMENTATION_USES}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TGRectangle }

function TGRectangle.GetBottomLeft: TGRectanglePoint;
begin
  Result.X := Left;
  Result.Y := Bottom;
end;

function TGRectangle.GetBottomRight: TGRectanglePoint;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TGRectangle.GetHeight: TGRectangleDimension;
begin
  Result := Bottom - Top;
end;

function TGRectangle.GetSize: TGRectanglePoint;
begin
  Result.X := Width;
  Result.Y := Height;
end;

function TGRectangle.GetTopLeft: TGRectanglePoint;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TGRectangle.GetTopRight: TGRectanglePoint;
begin
  Result.X := Right;
  Result.Y := Top;
end;

function TGRectangle.GetWidth: TGRectangleDimension;
begin
  Result := Right - Left;
end;

procedure TGRectangle.SetBottom(const AValue: TGRectangleDimension);
begin
  if FBottom = AValue then exit;
  if KeepSize then FTop := FTop + (AValue - FBottom);
  FBottom := AValue;
end;

procedure TGRectangle.SetBottomLeft(const AValue: TGRectanglePoint);
begin
  Left := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle.SetBottomRight(const AValue: TGRectanglePoint);
begin
  Right := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle.SetHeight(const AValue: TGRectangleDimension);
begin
  Bottom := Top + AValue;
end;

procedure TGRectangle.SetLeft(const AValue: TGRectangleDimension);
begin
  if FLeft = AValue then Exit;
  if KeepSize then FRight := FRight + (AValue - FLeft);
  FLeft := AValue;
end;

procedure TGRectangle.SetRight(const AValue: TGRectangleDimension);
begin
  if FRight = AValue then Exit;
  if KeepSize then FLeft := FLeft + (AValue - FRight);
  FRight := AValue;
end;

procedure TGRectangle.SetSize(const AValue: TGRectanglePoint);
begin
  Width := AValue.X;
  Height := AValue.Y;
end;

procedure TGRectangle.SetTop(const AValue: TGRectangleDimension);
begin
  if FTop = AValue then Exit;
  if KeepSize then FBottom := FBottom + (AValue - FTop);
  FTop := AValue;
end;

procedure TGRectangle.SetTopLeft(const AValue: TGRectanglePoint);
begin
  Left := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle.SetTopRight(const AValue: TGRectanglePoint);
begin
  Right := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle.SetWidth(const AValue: TGRectangleDimension);
begin
  Right := Left + AValue;
end;

procedure TGRectangle.Assign(Source: TGRectangle);
begin
  Left := Source.Left;
  Top := Source.Top;
  Right := Source.Right;
  Bottom := Source.Bottom;
  KeepSize := Source.KeepSize;
end;

function TGRectangle.IsPointInside(Pos: TGRectanglePoint): Boolean;
begin
  Result := (Pos.X >= Left) and (Pos.Y >= Top) and
    (Pos.X <= Right) and (Pos.Y <= Bottom);
end;

function TGRectangle.IsRectInside(Rect: TGRectangle): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Top >= Top) and
    (Rect.Right <= Right) and (Rect.Bottom <= Bottom);
end;

procedure TGRectangle.Intersect(Rect1, Rect2: TGRectangle);
begin
  if Rect1.Empty or Rect2.Empty then Empty := True
  else begin
    Left := Max(Rect1.Left, Rect2.Left);
    Top := Max(Rect1.Top, Rect2.Top);
    Right := Min(Rect1.Right, Rect2.Right);
    Bottom := Min(Rect1.Bottom, Rect2.Bottom);
  end;
end;

procedure TGRectangle.IntersectWith(Rect: TGRectangle);
begin
  if Empty or Rect.Empty then Empty := True
  else begin
    Left := Max(Left, Rect.Left);
    Top := Max(Top, Rect.Top);
    Right := Min(Right, Rect.Right);
    Bottom := Min(Bottom, Rect.Bottom);
  end;
end;

procedure TGRectangle.Union(Rect1, Rect2: TGRectangle);
begin
  if Rect1.Empty then Assign(Rect2)
  else
  if Rect2.Empty then Assign(Rect1)
  else begin
    Left := Min(Rect1.Left, Rect2.Left);
    Top := Min(Rect1.Top, Rect2.Top);
    Right := Max(Rect1.Right, Rect2.Right);
    Bottom := Max(Rect1.Bottom, Rect2.Bottom);
  end;
end;

procedure TGRectangle.UnionWith(Rect: TGRectangle);
begin
  if Empty then Assign(Rect)
  else
  if not Rect.Empty then begin
    Left := Min(Left, Rect.Left);
    Top := Min(Top, Rect.Top);
    Right := Max(Right, Rect.Right);
    Bottom := Max(Bottom, Rect.Bottom);
  end;
end;

procedure TGRectangle.SetRect(Left, Top, Width, Height: TGRectangleDimension);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Width := Width;
  Self.Height := Height;
end;

procedure TGRectangle.SetBounds(Left, Top, Right, Bottom: TGRectangleDimension);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

function TGRectangle.GetEmpty: Boolean;
begin
  Result := (Bottom <= Top) or (Right <= Left);
end;

procedure TGRectangle.SetEmpty(const AValue: Boolean);
begin
  Top := 0;
  Bottom := 0;
  Left := 0;
  Right := 0;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}

