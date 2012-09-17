unit UCoolBar;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Controls, SpecializedList;

type
  TCoolBar = class;

  { TCoolBand }

  TCoolBand = class
  private
    FControl: TControl;
    FCoolBar: TCoolBar;
    FText: string;
    FVisible: Boolean;
    procedure SetControl(AValue: TControl);
    procedure SetText(AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure ControlVisibleChange(Sender: TObject);
  published
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
    property CoolBar: TCoolBar read FCoolBar;
    property Control: TControl read FControl write SetControl;
  end;

  { TCoolBar }

  TCoolBar = class(TCustomControl)
  private
    FBands: TListObject;
    FRowSize: Integer;
    function GetBand(Index: Integer): TCoolBand;
    procedure SetBand(Index: Integer; AValue: TCoolBand);
    procedure BandsUpdate(Sender: TObject);
    procedure SetRowSize(AValue: Integer);
    procedure Paint; override;
    function SearchBand(Control: TControl): TCoolBand;
    procedure Resize; override;
  public
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Arrange;
    property Bands: TListObject read FBands;
    property BandsIndex[Index: Integer]: TCoolBand read GetBand write SetBand;
  published
    property PopupMenu;
    property Align;
    property Color;
    property RowSize: Integer read FRowSize write SetRowSize;
  end;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('Samples', [TCoolBar]);
end;

{ TCoolBand }

procedure TCoolBand.SetControl(AValue: TControl);
begin
  if FControl = AValue then Exit;
  FControl := AValue;
  if Assigned(AValue) then begin
    AValue.Parent := CoolBar;
    AValue.AddHandlerOnVisibleChanged(ControlVisibleChange);
  end else AValue.RemoveHandlerOnVisibleChanged(ControlVisibleChange);
  CoolBar.Arrange;
end;

procedure TCoolBand.SetText(AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
end;

procedure TCoolBand.SetVisible(AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
  if Assigned(Control) then Visible := FVisible;
end;

procedure TCoolBand.ControlVisibleChange(Sender: TObject);
begin
  FVisible := Control.Visible;
end;

{ TCoolBar }

function TCoolBar.GetBand(Index: Integer): TCoolBand;
begin
  Result := TCoolBand(FBands[Index]);
end;

procedure TCoolBar.SetBand(Index: Integer; AValue: TCoolBand);
begin
  FBands[Index] := AValue;
end;

procedure TCoolBar.BandsUpdate(Sender: TObject);
begin
  Arrange;
end;

procedure TCoolBar.SetRowSize(AValue: Integer);
begin
  if FRowSize = AValue then Exit;
  FRowSize := AValue;
  Arrange;
end;

procedure TCoolBar.Paint;
begin
  inherited Paint;
end;

procedure TCoolBar.InsertControl(AControl: TControl; Index: integer);
var
  NewBand: TCoolBand;
begin
  inherited InsertControl(AControl, Index);
  NewBand := TCoolBand(FBands.AddNew(TCoolBand.Create));
  NewBand.FCoolBar := Self;
  NewBand.Control := AControl;
  Arrange;
end;

procedure TCoolBar.RemoveControl(AControl: TControl);
var
  Band: TCoolBand;
begin
  Band := SearchBand(AControl);
  FBands.Remove(Band);
  inherited RemoveControl(AControl);
  Arrange;
end;

function TCoolBar.SearchBand(Control: TControl): TCoolBand;
var
  I: Integer;
begin
  I := 0;
  while (I < FBands.Count) and (TCoolBand(FBands[I]).Control <> Control) do Inc(I);
  if I < FBands.Count then Result := TCoolBand(FBands[I])
    else Result := nil;
end;

procedure TCoolBar.Resize;
begin
  inherited Resize;
//  Arrange;
end;

constructor TCoolBar.Create(TheOwner: TComponent);
begin
  inherited;
  FBands := TListObject.Create;
  FBands.OnUpdate := BandsUpdate;
  FRowSize := 24;
  Align := alTop;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Arrange;
end;

destructor TCoolBar.Destroy;
begin
  FBands.Free;
  inherited;
end;

procedure TCoolBar.Arrange;
var
  X, Y: Integer;
  I: Integer;
  NewHeight: Integer;
begin
  X := 0;
  Y := 0;
  for I := 0 to FBands.Count - 1 do
  with TCoolBand(FBands[I]) do begin
    if Assigned(Control) and Control.Visible then begin
      if (CoolBar.Width - X) > Control.Width then begin
        if (Control.Left <> X) or (Control.Top <> Y) or
          (Control.Width <> Control.Width) or (Control.Height <> RowSize) then
        Control.SetBounds(X, Y, Control.Width, RowSize);
        Inc(X, Control.Width);
      end else begin
        Inc(Y, RowSize);
        X := 0;
        if (Control.Left <> X) or (Control.Top <> Y) or
          (Control.Width <> Control.Width) or (Control.Height <> RowSize) then
        Control.SetBounds(X, Y, Control.Width, RowSize);
        Inc(X, Control.Width);
      end;
    end;
  end;
  Y := Y + RowSize;
  if Y > RowSize then NewHeight := Y
    else NewHeight := RowSize;
  if NewHeight <> Height then Height := NewHeight;
end;

end.

