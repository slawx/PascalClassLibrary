unit UVectorObject;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics;

type
  TVectorGroup = class;

  { TVectorObject }

  TVectorObject = class
  private
    FScale: Double;
    procedure SetScale(const AValue: Double); virtual;
  public
    Parent: TVectorGroup;
    Position: TPoint;
    property Scale: Double read FScale write SetScale;
    function TransformX(Value: Double): Integer;
    function TransformY(Value: Double): Integer;
    procedure Draw; virtual;
    constructor Create(Owner: TVectorGroup = nil);
  end;

  { TVectorDot }

  TVectorDot = class(TVectorObject)
    procedure Draw; override;
  end;

  { TVectorMultiLine }

  TVectorLine = class(TVectorObject)
    Points: TList; // of TVectorDot
    procedure Add(Position: TPoint);
    procedure Draw; override;
    constructor Create(Owner: TVectorGroup = nil);
    destructor Destroy; override;
  end;

  { TVectorRectangle }

  TVectorRectangle = class(TVectorObject)
    Size: TPoint;
    Brush: TBrush;
    procedure Draw; override;
    constructor Create(Owner: TVectorGroup = nil);
    destructor Destroy; override;
  end;

  { TVectorBitmap }

  TVectorBitmap = class(TVectorObject)
    Bitmap: TBitmap;
    procedure Draw; override;
  end;

  { TVectorText }

  TVectorText = class(TVectorObject)
    Font: TFont;
    Text: string;
    Brush: TBrush;
    procedure Draw; override;
    constructor Create(Owner: TVectorGroup = nil);
    destructor Destroy; override;
  end;

  { TVectorCanvas }

  { TVectorGroup }

  TVectorGroup = class(TVectorObject)
  private
    procedure SetScale(const AValue: Double); override;
  public
    Brush: TBrush;
    Objects: TList; // of TVectorObject
    BitmapCanvas: TCanvas;
    Pen: TPen;
    Font: TFont;
    constructor Create(Owner: TVectorGroup = nil);
    destructor Destroy; override;
    procedure Draw; override;
    procedure Ellipse(x1, y1, x2, y2: Integer);
    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);
    procedure Rectangle(x1, y1, x2, y2: Integer);
    procedure TextOut(X, Y: Integer; Text: string);
  end;

implementation

{ TVectorObject }

procedure TVectorObject.SetScale(const AValue: Double);
begin
  if FScale = AValue then exit;
  FScale := AValue;
end;

function TVectorObject.TransformX(Value: Double): Integer;
begin
  Result := Round((Value + Position.X) * Scale);
end;

function TVectorObject.TransformY(Value: Double): Integer;
begin
  Result := Round((Value + Position.Y) * Scale);
end;

procedure TVectorObject.Draw;
begin

end;

constructor TVectorObject.Create(Owner: TVectorGroup = nil);
begin
  Position := Point(0, 0);
  Scale := 1;
  Parent := Owner;
  if Assigned(Owner) then
    Owner.Objects.Add(Self);
end;

{ TVectorLine }

procedure TVectorLine.Add(Position: TPoint);
var
  NewPoint: TVectorDot;
begin
  NewPoint := TVectorDot.Create;
  NewPoint.Position := Position;
end;

procedure TVectorLine.Draw;
var
  I: Integer;
begin
  inherited Draw;
  for I := 0 to Points.Count - 1 do
  begin
    if I = 0 then Parent.MoveTo(TransformX(TVectorObject(Points[I]).Position.X),
      TransformY(TVectorObject(Points[I]).Position.Y))
      else Parent.LineTo(TransformX(TVectorObject(Points[I]).Position.X),
      TransformY(TVectorObject(Points[I]).Position.Y));
  end;
end;

constructor TVectorLine.Create(Owner: TVectorGroup);
begin
  inherited;
  Points := TList.Create;
end;

destructor TVectorLine.Destroy;
var
  I: Integer;
begin
  for I := 0 to Points.Count - 1 do
    TVectorObject(Points[I]).Destroy;
  Points.Destroy;
  inherited Destroy;
end;

{ TVectorDot }

procedure TVectorDot.Draw;
begin
  inherited Draw;
  Parent.Ellipse(Round(Position.X - Scale / 2), Round(Position.Y - Scale / 2),
    Round(Position.X + Scale / 2), Round(Position.Y + Scale / 2));
end;

{ TVectorRectangle }

procedure TVectorRectangle.Draw;
begin
  inherited Draw;
  Parent.Brush.Assign(Brush);
  Parent.Rectangle(TransformX(-Size.X / 2), TransformY(-Size.Y / 2),
    TransformX(Size.X / 2), TransformY(Size.Y / 2));
end;

constructor TVectorRectangle.Create;
begin
  inherited;
  Brush := TBrush.Create;
  Size := Point(1, 1);
end;

destructor TVectorRectangle.Destroy;
begin
  Brush.Destroy;
  inherited;
end;

{ TVectorGroup }

procedure TVectorGroup.SetScale(const AValue:Double);
begin
  inherited SetScale(AValue);
end;

constructor TVectorGroup.Create(Owner: TVectorGroup = nil);
begin
  inherited;
  Objects := TList.Create;
  Brush := TBrush.Create;
  Pen := TPen.Create;
  Font := TFont.Create;
end;

destructor TVectorGroup.Destroy;
var
  I: Integer;
begin
  Font.Destroy;
  Pen.Destroy;
  Brush.Destroy;
  for I := 0 to Objects.Count - 1 do
    TVectorObject(Objects[I]).Destroy;
  Objects.Destroy;
  inherited Destroy;
end;

procedure TVectorGroup.Draw;
var
  I: Integer;
begin
  for I := 0 to Objects.Count - 1 do
    TVectorObject(Objects[I]).Draw;
end;

procedure TVectorGroup.Ellipse(x1,y1,x2,y2:Integer);
begin
  if Assigned(Parent) then Parent.Ellipse(TransformX(X1), TransformY(Y1),
    TransformX(X2), TransformY(Y2))
    else BitmapCanvas.Ellipse(TransformX(X1), TransformY(Y1),
      TransformX(X2), TransformY(Y2));
end;

procedure TVectorGroup.MoveTo(X, Y: Integer);
begin
  if Assigned(Parent) then Parent.MoveTo(TransformX(X), TransformY(Y))
    else BitmapCanvas.MoveTo(TransformX(X), TransformY(Y));
end;

procedure TVectorGroup.LineTo(X, Y: Integer);
begin
  if Assigned(Parent) then Parent.LineTo(TransformX(X), TransformY(Y))
    else BitmapCanvas.LineTo(TransformX(X), TransformY(Y));
end;

procedure TVectorGroup.Rectangle(x1,y1,x2,y2:Integer);
begin
  if Assigned(Parent) then begin
    Parent.Brush.Assign(Brush);
    Parent.Rectangle(TransformX(X1), TransformY(Y1),
      TransformX(X2), TransformY(Y2));
  end else begin
    BitmapCanvas.Brush.Assign(Brush);
    BitmapCanvas.Rectangle(TransformX(X1), TransformY(Y1),
      TransformX(X2), TransformY(Y2));
  end;
end;

procedure TVectorGroup.TextOut(X,Y:Integer;Text:string);
begin
  if Assigned(Parent) then begin
    Parent.Font.Assign(Font);
    Parent.Font.Size := Round(Font.Size * Scale);
    Parent.TextOut(TransformX(X), TransformY(Y), Text)
  end else begin
    BitmapCanvas.Font.Assign(Font);
    BitmapCanvas.Font.Size := Round(Font.Size * Scale);
    BitmapCanvas.TextOut(TransformX(X - BitmapCanvas.TextWidth(Text) / 2),
      TransformY(Y - BitmapCanvas.TextHeight(Text) / 2), Text);
  end;
end;

{ TVectorText }

procedure TVectorText.Draw;
begin
  inherited Draw;
  Parent.Brush.Assign(Brush);
  Parent.Font.Assign(Font);
  Parent.Font.Size := Round(Font.Size * Scale);
  Parent.TextOut(TransformX(0), TransformY(0), Text);
end;

constructor TVectorText.Create(Owner:TVectorGroup);
begin
  inherited;
  Brush := TBrush.Create;
  Font := TFont.Create;
end;

destructor TVectorText.Destroy;
begin
  Font.Destroy;
  Brush.Destroy;
  inherited Destroy;
end;

{ TVectorBitmap }

procedure TVectorBitmap.Draw;
begin
  inherited Draw;
end;

end.

