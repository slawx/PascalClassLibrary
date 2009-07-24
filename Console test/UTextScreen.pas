unit UTextScreen;

interface

uses
  Types, Graphics, SysUtils, UConsole, Forms;

type
  TTextScreenCharacter = record
    Character: Char;
    TextColor: TColor;
    BackgroundColor: TColor;
  end;

  TTextMatrix = array of array of TTextScreenCharacter;

  TOnPaintCharEvent = procedure(Position: TPoint; Character: Char; TextColor, BackgroundColor: TColor; CursorVisible: Boolean) of object;
  TOnSizeChangeEvent = procedure(Size: TPoint) of object;

  TTextScreen = class
  private
    Dimensions: TPoint;
    CharMatrix: TTextMatrix;
    FOnPaintChar: TOnPaintCharEvent;
    FOnSizeChange: TOnSizeChangeEvent;
    procedure WriteChar(Character: Char);
  public
    CursorBlinkState: Boolean;
    CursorPosition: TPoint;
    TextColor: TColor;
    BackgroundColor: TColor;
    CursorVisible: Boolean;
    constructor Create;
    function Width: Integer;
    function Height: Integer;
    procedure WriteLn(Text: string); overload;
    procedure WriteLn; overload;
    procedure Write(Text: string);
    procedure GotoXY(X, Y: Integer);
    procedure Clear;
    procedure ClearEol;
    procedure Scroll;
    procedure SetSize(Size: TPoint);
    procedure PaintChar(Position: TPoint);
    procedure Paint;
    property OnPaintChar: TOnPaintCharEvent read FOnPaintChar write FOnPaintChar;
    property OnSizeChange: TOnSizeChangeEvent read FOnSizeChange write FOnSizeChange;
  end;

implementation

{ TTextScreen }

procedure TTextScreen.Clear;
var
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to Dimensions.Y - 1 do
    for X := 0 to Dimensions.X - 1 do begin
      CharMatrix[Y, X].Character := ' ';
      CharMatrix[Y, X].TextColor := TextColor;
      CharMatrix[Y, X].BackgroundColor := BackGroundColor;
    end;
  CursorPosition := Point(0, 0);
  Paint;
end;

procedure TTextScreen.ClearEol;
var
  I: Integer;
begin
  for I := CursorPosition.X to Dimensions.X - 1 do
    WriteChar(' ');
end;

constructor TTextScreen.Create;
begin
  SetSize(Point(40, 25));
  BackGroundColor := clBlack;
  TextColor := clWhite;
  CursorVisible := True;
  Clear;
end;

procedure TTextScreen.GotoXY(X, Y: Integer);
begin
  CursorPosition := Point(X, Y);
end;

function TTextScreen.Height: Integer;
begin
  Result := Dimensions.Y;
end;

procedure TTextScreen.Paint;
var
  X: Integer;
  Y: Integer;
begin
  for Y := 0 to Dimensions.Y - 1 do
    for X := 0 to Dimensions.X - 1 do
      PaintChar(Point(X, Y));
end;

procedure TTextScreen.PaintChar(Position: TPoint);
begin
  if Assigned(FOnPaintChar) then
    with CharMatrix[Position.Y, Position.X] do
      FOnPaintChar(Position, Character, TextColor, BackgroundColor, CursorVisible and (CursorPosition.X = Position.X) and
    (CursorPosition.Y = Position.Y) and CursorBlinkState);
end;

procedure TTextScreen.Scroll;
var
  X: Integer;
  Y: Integer;
begin
  // Shift lines up
  for Y := 0 to Dimensions.Y - 2 do
    for X := 0 to Dimensions.X - 1 do begin
      CharMatrix[Y, X] := CharMatrix[Y + 1, X];
    end;
  // Clear last bottom line
  for X := 0 to Dimensions.X - 1 do begin
    CharMatrix[Dimensions.Y - 1, X].Character := ' ';
    CharMatrix[Dimensions.Y - 1, X].TextColor := TextColor;
    CharMatrix[Dimensions.Y - 1, X].BackgroundColor := BackGroundColor;
  end;
  Paint;
end;

procedure TTextScreen.SetSize(Size: TPoint);
begin
  Dimensions := Size;
  SetLength(CharMatrix, Size.Y, Size.X);
  if Assigned(FOnSizeChange) then FOnSizeChange(Size);
  Paint;
end;

function TTextScreen.Width: Integer;
begin
  Result := Dimensions.X;
end;

procedure TTextScreen.WriteChar(Character: Char);
begin
  CursorBlinkState := False;
  if Character = #8 then begin
    PaintChar(CursorPosition);
    Dec(CursorPosition.X);
    if CursorPosition.X < 0 then begin
      CursorPosition.X := Dimensions.X - 1;
      Dec(CursorPosition.Y);
      if CursorPosition.Y < 0 then CursorPosition.Y := 0;
    end;
    CharMatrix[CursorPosition.Y, CursorPosition.X].Character := ' ';
    PaintChar(CursorPosition);
  end else
  if Character = #13 then begin
    CursorPosition.X := 0;
    Inc(CursorPosition.Y);
    if CursorPosition.Y >= Dimensions.Y then begin
      Dec(CursorPosition.Y);
      Scroll;
    end;
  end else
  if Character < #32 then begin
  end else begin
    CharMatrix[CursorPosition.Y, CursorPosition.X].Character := Character;
    CharMatrix[CursorPosition.Y, CursorPosition.X].TextColor := TextColor;
    CharMatrix[CursorPosition.Y, CursorPosition.X].BackgroundColor := BackGroundColor;
    PaintChar(CursorPosition);
    Inc(CursorPosition.X);
    if CursorPosition.X >= Dimensions.X then begin
      CursorPosition.X := 0;
      Inc(CursorPosition.Y);
      if CursorPosition.Y >= Dimensions.Y then begin
        Dec(CursorPosition.Y);
        Scroll;
      end;
    end;
  end;
end;

procedure TTextScreen.WriteLn;
begin
  Write(#13#10);
end;

procedure TTextScreen.WriteLn(Text: string);
begin
  Write(Text + #13#10);
end;

procedure TTextScreen.Write(Text: string);
var
  I: Integer;
begin
  for I := 1 to Length(Text) do begin
    if Text[I] = #13 then CursorPosition.X := 0
    else if Text[I] = #10 then begin
      Inc(CursorPosition.Y);
      if CursorPosition.Y >= Dimensions.Y then begin
        Dec(CursorPosition.Y);
        Scroll;
      end;
    end else WriteChar(Text[I]);
  end;
end;

end.
