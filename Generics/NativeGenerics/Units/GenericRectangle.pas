unit GenericRectangle;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, GenericPoint;

type
  TGRectangle<TDimension> = class
  private
    function GetBottomLeft: TGPoint<TDimension>;
    function GetBottomRight: TGPoint<TDimension>;
    function GetHeight: TDimension;
    function GetSize: TGPoint<TDimension>;
    function GetTopLeft: TGPoint<TDimension>;
    function GetTopRight: TGPoint<TDimension>;
    function GetWidth: TDimension;
    function GetEmpty: Boolean;
    procedure SetBottom(const AValue: TDimension);
    procedure SetBottomLeft(const AValue: TGPoint<TDimension>);
    procedure SetBottomRight(const AValue: TGPoint<TDimension>);
    procedure SetHeight(const AValue: TDimension);
    procedure SetLeft(const AValue: TDimension);
    procedure SetRight(const AValue: TDimension);
    procedure SetSize(const AValue: TGPoint<TDimension>);
    procedure SetTop(const AValue: TDimension);
    procedure SetTopLeft(const AValue: TGPoint<TDimension>);
    procedure SetTopRight(const AValue: TGPoint<TDimension>);
    procedure SetWidth(const AValue: TDimension);
    procedure SetEmpty(const AValue: Boolean);
    function Max(Value1, Value2: TDimension): TDimension;
    function Min(Value1, Value2: TDimension): TDimension;
  public
    FLeft: TDimension;
    FTop: TDimension;
    FRight: TDimension;
    FBottom: TDimension;
    KeepSize: Boolean;

    procedure Assign(Source: TGRectangle<TDimension>);
    function IsPointInside(Pos: TGPoint<TDimension>): Boolean;
    function IsRectInside(Rect: TGRectangle<TDimension>): Boolean;
    procedure Intersect(Rect1, Rect2: TGRectangle<TDimension>);
    procedure IntersectWith(Rect: TGRectangle<TDimension>);
    procedure Union(Rect1, Rect2: TGRectangle<TDimension>);
    procedure UnionWith(Rect: TGRectangle<TDimension>);

    procedure SetRect(Left, Top, Width, Height: TDimension);
    procedure SetBounds(Left, Top, Right, Bottom: TDimension);

    property Left: TDimension read FLeft write SetLeft;
    property Top: TDimension read FTop write SetTop;
    property Right: TDimension read FRight write SetRight;
    property Bottom: TDimension read FBottom write SetBottom;

    property Width: TDimension read GetWidth write SetWidth;
    property Height: TDimension read GetHeight write SetHeight;

    property TopLeft: TGPoint<TDimension> read GetTopLeft write SetTopLeft;
    property TopRight: TGPoint<TDimension> read GetTopRight write SetTopRight;
    property BottomLeft: TGPoint<TDimension> read GetBottomLeft write SetBottomLeft;
    property BottomRight: TGPoint<TDimension> read GetBottomRight write SetBottomRight;

    property Size: TGPoint<TDimension> read GetSize write SetSize;
    property Empty: Boolean read GetEmpty write SetEmpty;
  end;

  { TRectangle }

  TRectangle = class(TGRectangle<LongInt>)
  private
    procedure SetTRect(const AValue: TRect);
    function GetTRect: TRect;
  public
    property AsTRect: TRect read GetTRect write SetTRect;
  end;


implementation

{ TGRectangle }

function TGRectangle<TDimension>.GetBottomLeft: TGPoint<TDimension>;
begin
  Result.X := Left;
  Result.Y := Bottom;
end;

function TGRectangle<TDimension>.GetBottomRight: TGPoint<TDimension>;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TGRectangle<TDimension>.GetHeight: TDimension;
begin
  Result := Bottom - Top;
end;

function TGRectangle<TDimension>.GetSize: TGPoint<TDimension>;
begin
  Result.X := Width;
  Result.Y := Height;
end;

function TGRectangle<TDimension>.GetTopLeft: TGPoint<TDimension>;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TGRectangle<TDimension>.GetTopRight: TGPoint<TDimension>;
begin
  Result.X := Right;
  Result.Y := Top;
end;

function TGRectangle<TDimension>.GetWidth: TDimension;
begin
  Result := Right - Left;
end;

procedure TGRectangle<TDimension>.SetBottom(const AValue: TDimension);
begin
  if FBottom = AValue then exit;
  if KeepSize then FTop := FTop + (AValue - FBottom);
  FBottom := AValue;
end;

procedure TGRectangle<TDimension>.SetBottomLeft(const AValue: TGPoint<TDimension>);
begin
  Left := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle<TDimension>.SetBottomRight(const AValue: TGPoint<TDimension>);
begin
  Right := AValue.X;
  Bottom := AValue.Y;
end;

procedure TGRectangle<TDimension>.SetHeight(const AValue: TDimension);
begin
  Bottom := Top + AValue;
end;

procedure TGRectangle<TDimension>.SetLeft(const AValue: TDimension);
begin
  if FLeft = AValue then Exit;
  if KeepSize then FRight := FRight + (AValue - FLeft);
  FLeft := AValue;
end;

procedure TGRectangle<TDimension>.SetRight(const AValue: TDimension);
begin
  if FRight = AValue then Exit;
  if KeepSize then FLeft := FLeft + (AValue - FRight);
  FRight := AValue;
end;

procedure TGRectangle<TDimension>.SetSize(const AValue: TGPoint<TDimension>);
begin
  Width := AValue.X;
  Height := AValue.Y;
end;

procedure TGRectangle<TDimension>.SetTop(const AValue: TDimension);
begin
  if FTop = AValue then Exit;
  if KeepSize then FBottom := FBottom + (AValue - FTop);
  FTop := AValue;
end;

procedure TGRectangle<TDimension>.SetTopLeft(const AValue: TGPoint<TDimension>);
begin
  Left := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle<TDimension>.SetTopRight(const AValue: TGPoint<TDimension>);
begin
  Right := AValue.X;
  Top := AValue.Y;
end;

procedure TGRectangle<TDimension>.SetWidth(const AValue: TDimension);
begin
  Right := Left + AValue;
end;

procedure TGRectangle<TDimension>.Assign(Source: TGRectangle<TDimension>);
begin
  Left := Source.Left;
  Top := Source.Top;
  Right := Source.Right;
  Bottom := Source.Bottom;
  KeepSize := Source.KeepSize;
end;

function TGRectangle<TDimension>.IsPointInside(Pos: TGPoint<TDimension>): Boolean;
begin
  Result := (Pos.X >= Left) and (Pos.Y >= Top) and
    (Pos.X <= Right) and (Pos.Y <= Bottom);
end;

function TGRectangle<TDimension>.IsRectInside(Rect: TGRectangle<TDimension>): Boolean;
begin
  Result := (Rect.Left >= Left) and (Rect.Top >= Top) and
    (Rect.Right <= Right) and (Rect.Bottom <= Bottom);
end;

procedure TGRectangle<TDimension>.Intersect(Rect1, Rect2: TGRectangle<TDimension>);
begin
  if Rect1.Empty or Rect2.Empty then Empty := True
  else begin
    Left := Max(Rect1.Left, Rect2.Left);
    Top := Max(Rect1.Top, Rect2.Top);
    Right := Min(Rect1.Right, Rect2.Right);
    Bottom := Min(Rect1.Bottom, Rect2.Bottom);
  end;
end;

procedure TGRectangle<TDimension>.IntersectWith(Rect: TGRectangle<TDimension>);
begin
  if Empty or Rect.Empty then Empty := True
  else begin
    Left := Max(Left, Rect.Left);
    Top := Max(Top, Rect.Top);
    Right := Min(Right, Rect.Right);
    Bottom := Min(Bottom, Rect.Bottom);
  end;
end;

procedure TGRectangle<TDimension>.Union(Rect1, Rect2: TGRectangle<TDimension>);
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

procedure TGRectangle<TDimension>.UnionWith(Rect: TGRectangle<TDimension>);
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

procedure TGRectangle<TDimension>.SetRect(Left, Top, Width, Height: TDimension);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Width := Width;
  Self.Height := Height;
end;

procedure TGRectangle<TDimension>.SetBounds(Left, Top, Right, Bottom: TDimension);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

function TGRectangle<TDimension>.GetEmpty: Boolean;
begin
  Result := (Bottom <= Top) or (Right <= Left);
end;

procedure TGRectangle<TDimension>.SetEmpty(const AValue: Boolean);
begin
  Top := 0;
  Bottom := 0;
  Left := 0;
  Right := 0;
end;

function TGRectangle<TDimension>.Max(Value1, Value2: TDimension): TDimension;
begin
  if Value1 > Value2 then Result := Value1
    else Result := Value2;
end;

function TGRectangle<TDimension>.Min(Value1, Value2: TDimension): TDimension;
begin
  if Value1 < Value2 then Result := Value1
    else Result := Value2;
end;

{ TRectangle }

procedure TRectangle.SetTRect(const AValue: TRect);
begin
  Left := AValue.Left;
  Top := AValue.Top;
  Bottom := AValue.Bottom;
  Right := AValue.Right;
end;

function TRectangle.GetTRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

end.

