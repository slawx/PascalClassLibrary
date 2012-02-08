unit GenericBitmap;

{$mode delphi}{$H+}

interface

uses
  SysUtils, Classes, GenericMatrix;

type

  { TGBitmap }

  TGBitmap<TColor> = class(TGMatrix<TColor>)
  public
    type
      TIndex = TIndexX; // or TIndexY
  private
    function GetWidth: TIndexX;
    function GetHeight: TIndexX;
    procedure SetWidth(Value: TIndexX);
    procedure SetHeight(Value: TIndexY);
  public
    procedure Line(X1: TIndexX; Y1: TIndexY; X2: TIndexX; Y2: TIndexY;
      Color: TColor);
    procedure Circle(X: TIndexX; Y: TIndexY; Radius: TIndex; Color: TColor);
    property Pixels[X: TIndexX; Y: TIndexY]: TColor
      read GetItemXY write PutItemXY;
    property Width: TIndexX read GetWidth write SetWidth;
    property Height: TIndexY read GetHeight write SetHeight;
  end;

  TGPen<TColor> = class
    Color: TColor;
    Position: TPoint;
    //Style: TPenStyle;
  end;

  TGBrush<TColor> = class
    Color: TColor;
    //Style: TBrushStyle;
  end;

  { TGCanvas }

  TGCanvas<TColor> = class
  type
    TIndex = NativeInt;
  public
    Bitmap: TGBitmap<TColor>;
    Pen: TGPen<TColor>;
    Brush: TGBrush<TColor>;
    procedure MoveTo(X, Y: TIndex);
    procedure LineTo(X, Y: TIndex);
  end;


implementation

function TGBitmap<TColor>.GetWidth: TIndexX;
begin
  Result := Count.X;
end;

function TGBitmap<TColor>.GetHeight: TIndexX;
begin
  Result := Count.Y;
end;

procedure TGBitmap<TColor>.SetWidth(Value: TIndexX);
begin
  Count := CreateIndex(Value, Count.Y);
end;

procedure TGBitmap<TColor>.SetHeight(Value: TIndexY);
begin
  Count := CreateIndex(Count.X, Value);
end;

procedure TGBitmap<TColor>.Line(X1: TIndexX; Y1: TIndexY; X2: TIndexX; Y2: TIndexY;
  Color: TColor);
var
  CurrentX, CurrentY: TIndex;
  Xinc, Yinc: TIndex;
  Dx, Dy: TIndex;
  TwoDx, TwoDy: TIndex;
  TwoDxAccumulatedError, TwoDyAccumulatedError: TIndex;
begin
  Dx := (X2 - X1);
  Dy := (Y2 - Y1);

  TwoDx := Dx + Dx;
  TwoDy := Dy + Dy;

  CurrentX := X1;
  CurrentY := Y1;

  Xinc := 1;
  Yinc := 1;

  if Dx < 0 then begin
    Xinc := -1;
    Dx := -Dx;
    TwoDx := -TwoDx;
  end;
  if Dy < 0 then begin
    Yinc := -1;
    Dy := -Dy;
    TwoDy := -TwoDy;
  end;

  //Pixels[X1, Y1] := Color

  if (Dx <> 0) or (Dy <> 0) then begin
    if Dy <= Dx then begin
      TwoDxAccumulatedError := 0;
      repeat
	Inc(CurrentX, Xinc);
	Inc(TwoDxAccumulatedError, TwoDy);
  	if TwoDxAccumulatedError > Dx then begin
  	  Inc(CurrentY, Yinc);
  	  Dec(TwoDxAccumulatedError, TwoDx);
        end;
        Pixels[CurrentX, CurrentY] := Color;
      until CurrentX = X2;
    end else begin
      TwoDyAccumulatedError := 0;
      repeat
        Inc(CurrentY, Yinc);
  	Inc(TwoDyAccumulatedError, TwoDx);
  	if TwoDyAccumulatedError > Dy then begin
 	  Inc(CurrentX, Xinc);
  	  Inc(TwoDyAccumulatedError, TwoDy);
  	end;
        Pixels[CurrentX, CurrentY] := Color;
      until CurrentY = Y2;
    end;
  end;
end;

procedure TGBitmap<TColor>.Circle(X: TIndexX; Y: TIndexY; Radius: TIndex;
  Color: TColor);
var
  PosX, PosY: TIndex;
  XChange, YChange: TIndex;
  RadiusError: TIndex;
begin
  PosX := Radius;
  PosY := 0;
  XChange := 1 - 2 * Radius;
  YChange := 1;
  RadiusError := 0;
  while PosX >= PosY do begin
    Pixels[X + PosX, Y + PosY] := Color;
    Pixels[X - PosX, Y + PosY] := Color;
    Pixels[X - PosX, Y - PosY] := Color;
    Pixels[X + PosX, Y - PosY] := Color;
    Pixels[X + PosY, Y + PosX] := Color;
    Pixels[X - PosY, Y + PosX] := Color;
    Pixels[X - PosY, Y - PosX] := Color;
    Pixels[X + PosY, Y - PosX] := Color;
    PosY := PosY + 1;
    RadiusError := RadiusError + YChange;
    YChange := YChange + 2;
    if 2 * RadiusError + XChange > 0 then begin
      PosX := PosX - 1;
      RadiusError := RadiusError + XChange;
      XChange := XChange + 2;
    end;
  end;
end;

{ TGCanvas<TColor> }

procedure TGCanvas<TColor>.MoveTo(X, Y: TIndex);
begin

end;

procedure TGCanvas<TColor>.LineTo(X, Y: TIndex);
begin

end;

end.

