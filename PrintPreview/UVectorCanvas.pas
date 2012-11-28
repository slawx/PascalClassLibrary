unit UVectorCanvas;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphType, GraphMath, types, LCLType, fpvectorial,
  Contnrs;

type

  { TCanvasElement }

  TCanvasElement = class
    procedure Render(Canvas: TCanvas); virtual;
  end;

  { TElementLine }

  TElementLine = class(TCanvasElement)
    Points: array of TPoint;
    Color: TColor;
    procedure AddPoint(X, Y: Integer);
    procedure Render(Canvas: TCanvas); override;
  end;

  { TElementText }

  TElementText = class(TCanvasElement)
    Text: string;
    Position: TPoint;
    Font: TFont;
    procedure Render(Canvas: TCanvas); override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TElementFont }

  TElementFont = class(TCanvasElement)
    Font: TFont;
    constructor Create;
    destructor Destroy; override;
  end;

  { TElementBitmap }

  TElementBitmap = class(TCanvasElement)
    Bitmap: TBitmap;
    DestRect: TRect;
    constructor Create;
    destructor Destroy; override;
    procedure Render(Canvas: TCanvas); override;
  end;

  { TElementFillRect }

  TElementFillRect = class(TCanvasElement)
    Dimension: TRect;
    Color: TColor;
    procedure Render(Canvas: TCanvas); override;
  end;

  { TVectorBrush }

  TVectorBrush = class
  private
    FColor: TColor;
    FStyle: TBrushStyle;
    procedure SetColor(AValue: TColor);
    procedure SetStyle(AValue: TBrushStyle);
  published
    property Color: TColor read FColor write SetColor;
    property Style: TBrushStyle read FStyle write SetStyle;
  end;

  { TVectorPen }

  TVectorPen = class
  private
    FColor: TColor;
    FPosition: TPoint;
    FStyle: TPenStyle;
    procedure SetColor(AValue: TColor);
    procedure SetPosition(AValue: TPoint);
    procedure SetStyle(AValue: TPenStyle);
  public
    property Position: TPoint read FPosition write SetPosition;
    property Color: TColor read FColor write SetColor;
    property Style: TPenStyle read FStyle write SetStyle;
  end;

  { TVectorCanvas }

  TVectorCanvas = class
  private
    FBrush: TVectorBrush;
    FFont: TFont;
    FPen: TVectorPen;
    FSize: TPoint;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetBrush(AValue: TVectorBrush);
    procedure SetFont(AValue: TFont);
    procedure SetPen(AValue: TVectorPen);
    procedure SetSize(Value: TPoint);
  protected
  public
    Elements: TObjectList; // TObjectList<TCanvasElement>
    procedure Clear;

    // TCanvas compatibility
    procedure Line(x1, y1, x2, y2: Integer);
    procedure TextOut(X, Y: Integer; Text: string);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    function TextWidth(Text: string): Integer;
    function TextHeight(Text: string): Integer;
    procedure CopyRect(Dest: TRect; SrcCanvas: TCanvas; Source: TRect);

    constructor Create;
    destructor Destroy; override;
    procedure Render(Canvas: TCanvas);
    property Pen: TVectorPen read FPen write SetPen;
    property Brush: TVectorBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Size: TPoint read FSize write SetSize;
  end;

implementation

{ TElementBitmap }

constructor TElementBitmap.Create;
begin
  Bitmap := TBitmap.Create;
end;

destructor TElementBitmap.Destroy;
begin
  Bitmap.Free;
  inherited Destroy;
end;

procedure TElementBitmap.Render(Canvas: TCanvas);
begin
  inherited Render(Canvas);
  Canvas.CopyRect(DestRect, Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height));
end;

{ TElementText }

procedure TElementText.Render(Canvas: TCanvas);
begin
  inherited Render(Canvas);
  Canvas.Font.Assign(Font);
  Canvas.TextOut(Position.X, Position.Y, Text);
end;

constructor TElementText.Create;
begin
  Font := TFont.Create;
end;

destructor TElementText.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

{ TVectorPen }

procedure TVectorPen.SetColor(AValue: TColor);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
end;

procedure TVectorPen.SetPosition(AValue: TPoint);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
end;

procedure TVectorPen.SetStyle(AValue: TPenStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
end;

{ TElementFillRect }

procedure TElementFillRect.Render(Canvas: TCanvas);
begin
  inherited Render(Canvas);
  Canvas.FillRect(Dimension);
end;

{ TVectorBrush }

procedure TVectorBrush.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TVectorBrush.SetStyle(AValue: TBrushStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
end;

{ TElementFont }

constructor TElementFont.Create;
begin
  Font := TFont.Create;
end;

destructor TElementFont.Destroy;
begin
  Font.Free;
  inherited Destroy;
end;

{ TElementLine }

procedure TElementLine.AddPoint(X, Y: Integer);
begin
  SetLength(Points, Length(Points) + 1);
  Points[High(Points)] := Point(X, Y);
end;

procedure TElementLine.Render(Canvas: TCanvas);
var
  I: Integer;
begin
  inherited Render(Canvas);
  for I := 0 to High(Points) do
    if I = 0 then Canvas.MoveTo(Points[I].X, Points[I].Y)
      else Canvas.LineTo(Points[I].X, Points[I].Y);
end;

{ TCanvasElement }

procedure TCanvasElement.Render(Canvas: TCanvas);
begin

end;

{ TVectorCanvas }

procedure TVectorCanvas.SetFont(AValue: TFont);
begin
  if FFont = AValue then Exit;
  FFont := AValue;
end;

procedure TVectorCanvas.SetPen(AValue: TVectorPen);
begin
  if FPen=AValue then Exit;
  FPen:=AValue;
end;

procedure TVectorCanvas.Clear;
begin
  with TElementFillRect(Elements[Elements.Add(TElementFillRect.Create)]) do begin
    Color := Brush.Color;
    Dimension := Rect(0, 0, FSize.X, FSize.Y);
  end;
end;

procedure TVectorCanvas.SetBrush(AValue: TVectorBrush);
begin
  if FBrush=AValue then Exit;
  FBrush:=AValue;
end;

function TVectorCanvas.GetHeight: Integer;
begin
  Result := FSize.Y;
end;

function TVectorCanvas.GetWidth: Integer;
begin
  Result := FSize.X;
end;

procedure TVectorCanvas.Line(x1, y1, x2, y2: Integer);
begin
  with TElementLine(Elements[Elements.Add(TElementLine.Create)]) do begin
    Color := Pen.Color;
    AddPoint(X1, Y1);
    AddPoint(X2, Y2);
  end;
end;

procedure TVectorCanvas.TextOut(X, Y: Integer; Text: string);
var
  TextElement: TElementText;
begin
  TextElement := TElementText(Elements[Elements.Add(TElementText.Create)]);
  TextElement.Text := Text;
  TextElement.Position := Point(X, Y);
  TextElement.Font.Assign(Font);
end;

procedure TVectorCanvas.MoveTo(X, Y: Integer);
begin
  FPen.Position := Point(X, Y);
end;

procedure TVectorCanvas.LineTo(X, Y: Integer);
begin
  Line(FPen.Position.X, FPen.Position.Y, X, Y);
  FPen.Position := Point(X, Y);
end;

function TVectorCanvas.TextWidth(Text: string): Integer;
begin
  Result := Abs(FFont.Height * Length(Text));
end;

function TVectorCanvas.TextHeight(Text: string): Integer;
begin
  Result := Abs(FFont.Height);
end;

procedure TVectorCanvas.CopyRect(Dest: TRect; SrcCanvas: TCanvas; Source: TRect
  );
begin
  with TElementBitmap(Elements[Elements.Add(TElementBitmap.Create)]) do begin
    Bitmap.SetSize(Dest.Right - Dest.Left, Dest.Bottom - Dest.Top);
    Bitmap.Canvas.CopyRect(Rect(0, 0, Dest.Right - Dest.Left, Dest.Bottom - Dest.Top),
      SrcCanvas, Source);
    DestRect := Dest;
  end;
end;

procedure TVectorCanvas.SetSize(Value: TPoint);
begin
  FSize := Value;
end;

constructor TVectorCanvas.Create;
begin
  inherited;
  Elements := TObjectList.Create;
  Brush := TVectorBrush.Create;
  Pen := TVectorPen.Create;
  Font := TFont.Create;
end;

destructor TVectorCanvas.Destroy;
begin
  Font.Free;
  Pen.Free;
  Brush.Free;
  Elements.Free;
  inherited;
end;

procedure TVectorCanvas.Render(Canvas: TCanvas);
var
  I: Integer;
begin
  //Size := Point(Canvas.Width, Canvas.Height);
  Pen.Position := Point(0, 0);
  with Canvas do
  for I := 0 to Elements.Count - 1 do
    TCanvasElement(Elements[I]).Render(Canvas);
end;

end.


