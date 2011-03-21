unit URectangle;

// Date: 2011-03-20

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils; 

type
  { TRectangle }

  TRectangle = class
  private
    function GetBottomLeft: TPoint;
    function GetBottomRight: TPoint;
    function GetHeight: Integer;
    function GetTopLeft: TPoint;
    function GetTopRight: TPoint;
    function GetTRect: TRect;
    function GetWidth: Integer;
    procedure SetBottomLeft(const AValue: TPoint);
    procedure SetBottomRight(const AValue: TPoint);
    procedure SetHeight(const AValue: Integer);
    procedure SetTopLeft(const AValue: TPoint);
    procedure SetTopRight(const AValue: TPoint);
    procedure SetTRect(const AValue: TRect);
    procedure SetWidth(const AValue: Integer);
  public
    Left: Integer;
    Top: Integer;
    Right: Integer;
    Bottom: Integer;

    procedure Assign(Source: TRectangle);
    function IsInside(Pos: TPoint): Boolean;

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;

    property TopLeft: TPoint read GetTopLeft write SetTopLeft;
    property TopRight: TPoint read GetTopRight write SetTopRight;
    property BottomLeft: TPoint read GetBottomLeft write SetBottomLeft;
    property BottomRight: TPoint read GetBottomRight write SetBottomRight;

    property AsTRect: TRect read GetTRect write SetTRect;
  end;

implementation

{ TRectangle }

function TRectangle.GetBottomLeft: TPoint;
begin
  Result.X := Left;
  Result.Y := Bottom;
end;

function TRectangle.GetBottomRight: TPoint;
begin
  Result.X := Right;
  Result.Y := Bottom;
end;

function TRectangle.GetHeight: Integer;
begin
  Result := Bottom - Top;
end;

function TRectangle.GetTopLeft: TPoint;
begin
  Result.X := Left;
  Result.Y := Top;
end;

function TRectangle.GetTopRight: TPoint;
begin
  Result.X := Right;
  Result.Y := Top;
end;

function TRectangle.GetTRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function TRectangle.GetWidth: Integer;
begin
  Result := Right - Left;
end;

procedure TRectangle.SetBottomLeft(const AValue: TPoint);
begin
  Left := AValue.X;
  Bottom := AValue.Y;
end;

procedure TRectangle.SetBottomRight(const AValue: TPoint);
begin
  Right := AValue.X;
  Bottom := AValue.Y;
end;

procedure TRectangle.SetHeight(const AValue: Integer);
begin
  Bottom := Top + AValue;
end;

procedure TRectangle.SetTopLeft(const AValue: TPoint);
begin
  Left := AValue.X;
  Top := AValue.Y;
end;

procedure TRectangle.SetTopRight(const AValue: TPoint);
begin
  Right := AValue.X;
  Top := AValue.Y;
end;

procedure TRectangle.SetTRect(const AValue: TRect);
begin
  Left := AValue.Left;
  Top := AValue.Top;
  Bottom := AValue.Bottom;
  Right := AValue.Right;
end;

procedure TRectangle.SetWidth(const AValue: Integer);
begin
  Right := Left + AValue;
end;

procedure TRectangle.Assign(Source: TRectangle);
begin
  Left := Source.Left;
  Top := Source.Top;
  Right := Source.Right;
  Bottom := Source.Bottom;
end;

function TRectangle.IsInside(Pos: TPoint): Boolean;
begin
  Result := (Pos.X >= Left) and (Pos.Y >= Top) and
    (Pos.X <= Right) and (Pos.Y <= Bottom);
end;

end.

