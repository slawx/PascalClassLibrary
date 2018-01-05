unit GenericRectangle;

{$mode delphi}

interface

uses
  GenericPoint;

type
  TGRectangle<T> = class
  private
    function GetBottomLeft: TGPoint<T>;
    function GetBottomRight: TGPoint<T>;
    function GetHeight: T;
    function GetSize: TGPoint<T>;
    function GetTopLeft: TGPoint<T>;
    function GetTopRight: TGPoint<T>;
    function GetWidth: T;
    function GetEmpty: Boolean;
    procedure SetBottom(const AValue: T);
    procedure SetBottomLeft(const AValue: TGPoint<T>);
    procedure SetBottomRight(const AValue: TGPoint<T>);
    procedure SetHeight(const AValue: T);
    procedure SetLeft(const AValue: T);
    procedure SetRight(const AValue: T);
    procedure SetSize(const AValue: TGPoint<T>);
    procedure SetTop(const AValue: T);
    procedure SetTopLeft(const AValue: TGPoint<T>);
    procedure SetTopRight(const AValue: TGPoint<T>);
    procedure SetWidth(const AValue: T);
    procedure SetEmpty(const AValue: Boolean);
    function Max(Value1, Value2: T): T;
    function Min(Value1, Value2: T): T;
  public
    FLeft: T;
    FTop: T;
    FRight: T;
    FBottom: T;
    KeepSize: Boolean;

    procedure Assign(Source: TGRectangle<T>);
    function IsPointInside(Pos: TGPoint<T>): Boolean;
    function IsRectInside(Rect: TGRectangle<T>): Boolean;
    procedure Intersect(Rect1, Rect2: TGRectangle<T>);
    procedure IntersectWith(Rect: TGRectangle<T>);
    procedure Union(Rect1, Rect2: TGRectangle<T>);
    procedure UnionWith(Rect: TGRectangle<T>);

    procedure SetRect(Left, Top, Width, Height: T);
    procedure SetBounds(Left, Top, Right, Bottom: T);

    property Left: T read FLeft write SetLeft;
    property Top: T read FTop write SetTop;
    property Right: T read FRight write SetRight;
    property Bottom: T read FBottom write SetBottom;

    property Width: T read GetWidth write SetWidth;
    property Height: T read GetHeight write SetHeight;

    property TopLeft: TGPoint<T> read GetTopLeft write SetTopLeft;
    property TopRight: TGPoint<T> read GetTopRight write SetTopRight;
    property BottomLeft: TGPoint<T> read GetBottomLeft write SetBottomLeft;
    property BottomRight: TGPoint<T> read GetBottomRight write SetBottomRight;

    property Size: TGPoint<T> read GetSize write SetSize;
    property Empty: Boolean read GetEmpty write SetEmpty;
  end;


implementation

uses
  Math;


{ TGRectangle }

function TGRectangle<T>.Max(Value1, Value2: T): T;
begin
  if Value1 > Value2 then Result := Value1
    else Result := Value2;
end;

function TGRectangle<T>.Min(Value1, Value2: T): T;
begin
  if Value1 < Value2 then Result := Value1
    else Result := Value2;
end;

function TGRectangle<T>.GetBottomLeft: TGPoint<T>;
begin
  Result.X := Left;
  Result.Y := Bottom;
end;

function TGRectangle<T>.GetBottomRight: TGPoint<T>;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TGRectangle<T>.GetHeight: T;
begin
  Result := Bottom - Top;
end;

function TGRectangle<T>.GetSize: TGPoint<T>;
begin
  Result.X := Width;
  Result.Y := Height;
end;

function TGRectangle<T>.GetTopLeft: TGPoint<T>;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TGRectangle<T>.GetTopRight: TGPoint<T>;
begin
  Result.X := Right;
  Result.Y := Top;
end;

function TGRectangle<T>.GetWidth: T;
begin
  Result := Right - Left;
end;

procedure TGRectangle<T>.SetBottom(const AValue: T);
begin
  if FBottom = AValue then exit;
  if KeepSize then FTop := FTop + (AValue - FBottom);
  FBottom := AValue;
end;

procedure TGRectangle<T>.SetBottomLeft(const AValue: TGPoint<T>);
begin
  Left := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle<T>.SetBottomRight(const AValue: TGPoint<T>);
begin
  Right := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle<T>.SetHeight(const AValue: T);
begin
  Bottom := Top + AValue;
end;

procedure TGRectangle<T>.SetLeft(const AValue: T);
begin
  if FLeft = AValue then Exit;
  if KeepSize then FRight := FRight + (AValue - FLeft);
  FLeft := AValue;
end;

procedure TGRectangle<T>.SetRight(const AValue: T);
begin
  if FRight = AValue then Exit;
  if KeepSize then FLeft := FLeft + (AValue - FRight);
  FRight := AValue;
end;

procedure TGRectangle<T>.SetSize(const AValue: TGPoint<T>);
begin
  Width := AValue.X;
  Height := AValue.Y;
end;

procedure TGRectangle<T>.SetTop(const AValue: T);
begin
  if FTop = AValue then Exit;
  if KeepSize then FBottom := FBottom + (AValue - FTop);
  FTop := AValue;
end;

procedure TGRectangle<T>.SetTopLeft(const AValue: TGPoint<T>);
begin
  Left := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle<T>.SetTopRight(const AValue: TGPoint<T>);
begin
  Right := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle<T>.SetWidth(const AValue: T);
begin
  Right := Left + AValue;
end;

procedure TGRectangle<T>.Assign(Source: TGRectangle<T>);
begin
  Left := Source.Left;
  Top := Source.Top;
  Right := Source.Right;
  Bottom := Source.Bottom;
  KeepSize := Source.KeepSize;
end;

function TGRectangle<T>.IsPointInside(Pos: TGPoint<T>): Boolean;
begin
  Result := (Pos.X >= Left) and (Pos.Y >= Top) and
    (Pos.X <= Right) and (Pos.Y <= Bottom);
end;

function TGRectangle<T>.IsRectInside(Rect: TGRectangle<T>): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Top >= Top) and
    (Rect.Right <= Right) and (Rect.Bottom <= Bottom);
end;

procedure TGRectangle<T>.Intersect(Rect1, Rect2: TGRectangle<T>);
begin
  if Rect1.Empty or Rect2.Empty then Empty := True
  else begin
    Left := Max(Rect1.Left, Rect2.Left);
    Top := Max(Rect1.Top, Rect2.Top);
    Right := Min(Rect1.Right, Rect2.Right);
    Bottom := Min(Rect1.Bottom, Rect2.Bottom);
  end;
end;

procedure TGRectangle<T>.IntersectWith(Rect: TGRectangle<T>);
begin
  if Empty or Rect.Empty then Empty := True
  else begin
    Left := Max(Left, Rect.Left);
    Top := Max(Top, Rect.Top);
    Right := Min(Right, Rect.Right);
    Bottom := Min(Bottom, Rect.Bottom);
  end;
end;

procedure TGRectangle<T>.Union(Rect1, Rect2: TGRectangle<T>);
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

procedure TGRectangle<T>.UnionWith(Rect: TGRectangle<T>);
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

procedure TGRectangle<T>.SetRect(Left, Top, Width, Height: T);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Width := Width;
  Self.Height := Height;
end;

procedure TGRectangle<T>.SetBounds(Left, Top, Right, Bottom: T);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

function TGRectangle<T>.GetEmpty: Boolean;
begin
  Result := (Bottom <= Top) or (Right <= Left);
end;

procedure TGRectangle<T>.SetEmpty(const AValue: Boolean);
begin
  Top := 0;
  Bottom := 0;
  Left := 0;
  Right := 0;
end;

end.
