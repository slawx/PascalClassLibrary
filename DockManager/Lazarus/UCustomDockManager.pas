unit UCustomDockManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms;

const
  GrabberSize = 18;

type
  TDockDirection = (ddNone, ddHorizontal, ddVertical);

  TCustomDockManager = class;

  { TConjoinDockForm }

  TConjoinDockForm = class(TForm)
    Panel: TPanel;
    constructor Create(TheOwner: TComponent); override;
  end;

  { TDockClientPanel }

  TDockClientPanel = class(TPanel)
    OwnerDockManager: TCustomDockManager;
    CloseButton: TSpeedButton;
    Control: TControl;
    Splitter: TSplitter;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockPanelPaint(Sender: TObject);
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
    procedure CloseButtonClick(Sender: TObject);
    procedure ResizeExecute(Sender: TObject);
  end;

  { TCustomDockManager }

  TCustomDockManager = class(TDockManager)
  private
    FDockDirection: TDockDirection;
    FDockSite: TWinControl;
    FDockPanels: TObjectList; // of TDockClientPanel
    function FindControlInPanels(Control: TControl): TDockClientPanel;
  public
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure GetControlBounds(Control: TControl;
      out AControlBounds: TRect); override;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; override;
    procedure InsertControl(ADockObject: TDragDockObject); override; overload;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override; overload;
    procedure LoadFromStream(Stream: TStream); override;
    procedure PaintSite(DC: HDC); override;
    procedure MessageHandler(Sender: TControl; var Message: TLMessage); override;
    procedure PositionDockRect(ADockObject: TDragDockObject); override; overload;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override; overload;
    procedure RemoveControl(Control: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetReplacingControl(Control: TControl); override;
    function AutoFreeByControl: Boolean; override;
  end;

implementation

{ TCustomDockManager }

function TCustomDockManager.FindControlInPanels(Control: TControl
  ): TDockClientPanel;
var
  I: Integer;
begin
  I := 0;
  while (I < FDockPanels.Count) and
    (TDockClientPanel(FDockPanels[I]).Control <> Control) do Inc(I);
  if I < FDockPanels.Count then Result := TDockClientPanel(FDockPanels[I])
    else Result := nil;
end;

constructor TCustomDockManager.Create(ADockSite: TWinControl);
begin
  FDockSite := ADockSite;
  FDockPanels := TObjectList.Create;
  inherited Create(ADockSite);
end;

destructor TCustomDockManager.Destroy;
begin
  FDockPanels.Free;
  inherited Destroy;
end;

procedure TCustomDockManager.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TCustomDockManager.EndUpdate;
begin
  inherited EndUpdate;
end;

procedure TCustomDockManager.GetControlBounds(Control: TControl; out
  AControlBounds: TRect);
begin
end;

function TCustomDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
begin
  Result := inherited GetDockEdge(ADockObject);
end;

procedure TCustomDockManager.InsertControl(ADockObject: TDragDockObject);
begin
  inherited InsertControl(ADockObject);
end;

procedure TCustomDockManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewPanel: TDockClientPanel;
  I: Integer;
  NewConjoinDockForm: TConjoinDockForm;
begin
  if (FDockSite is TForm) and (not Assigned(FDockSite.Parent)) then begin
    NewConjoinDockForm := TConjoinDockForm.Create(nil);
    NewConjoinDockForm.Visible := True;
    Control.ManualDock(NewConjoinDockForm.Panel);
    FDockSite.ManualDock(NewConjoinDockForm.Panel);
  end else
  if FDockSite is TPanel then begin
    if FDockSite.DockClientCount = 2 then begin
      if (InsertAt = alTop) or (InsertAt = alBottom) then
        FDockDirection := ddVertical
      else
      if (InsertAt = alLeft) or (InsertAt = alRight) then
        FDockDirection := ddHorizontal
      else FDockDirection := ddHorizontal;
    end;
    if FDockSite.DockClientCount > 1 then begin
      NewSplitter := TSplitter.Create(nil);
      NewSplitter.Parent := FDockSite;
      NewSplitter.Visible := True;
      NewSplitter.Color := clRed;
      with NewSplitter do
      if FDockDirection = ddVertical then begin
        Align := alTop;
        Top := FDockSite.Height;
      end else
      if FDockDirection = ddHorizontal then begin
        Align := alLeft;
        Left := FDockSite.Width;
      end;

      with TDockClientPanel(FDockPanels.Last) do
      if FDockDirection = ddVertical then
        Align := alTop
      else
      if FDockDirection = ddHorizontal then
        Align := alLeft;
    end;
    NewPanel := TDockClientPanel.Create(nil);
    with NewPanel do begin
      Splitter := NewSplitter;
      Parent := FDockSite;
      OwnerDockManager := Self;
      Visible := True;
      Align := alClient;
    end;
    NewPanel.Control := Control;
    Control.Parent := NewPanel;
    FDockPanels.Add(NewPanel);

    for I := 0 to FDockPanels.Count - 1 do begin
      TDockClientPanel(FDockPanels[I]).Height := FDockSite.Height div
        FDockSite.DockClientCount;
      TDockClientPanel(FDockPanels[I]).Width := FDockSite.Width div
        FDockSite.DockClientCount;
    end;
  end;

//  FDockPanel.Invalidate;
  inherited;
end;

procedure TCustomDockManager.LoadFromStream(Stream: TStream);
begin
end;

procedure TCustomDockManager.PaintSite(DC: HDC);
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  for I := 0 to FDockPanels.Count - 1 do
    with TDockClientPanel(FDockPanels[I]) do begin
      Invalidate;
    end;
end;

procedure TCustomDockManager.MessageHandler(Sender: TControl;
  var Message: TLMessage);
begin
  inherited MessageHandler(Sender, Message);
end;

procedure TCustomDockManager.PositionDockRect(ADockObject: TDragDockObject);
begin
  inherited PositionDockRect(ADockObject);
end;

procedure TCustomDockManager.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
begin
  case DropAlign of
    alNone: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
    alRight: begin
      DockRect := Rect(FDockSite.ClientWidth div 2, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
    alLeft: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth div 2, FDockSite.ClientHeight);
    end;
    alTop: begin
      DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight div 2);
    end;
    alBottom: begin
      DockRect := Rect(0, FDockSite.ClientHeight div 2, FDockSite.ClientWidth, FDockSite.ClientHeight);
    end;
  end;
  DockRect.TopLeft := FDockSite.ClientToScreen(DockRect.TopLeft);
  DockRect.BottomRight := FDockSite.ClientToScreen(DockRect.BottomRight);
end;

procedure TCustomDockManager.RemoveControl(Control: TControl);
var
  ClientPanel: TDockClientPanel;
begin
  inherited;
  if Control.HostDockSite = Self.FDockSite then begin
    ClientPanel := FindControlInPanels(Control);
    //if Assigned(ClientPanel) then ClientPanel.Splitter.Free;
    FDockPanels.Remove(ClientPanel);
    if FDockSite.DockClientCount = 2 then FDockDirection := ddNone;
    //FDockSite.Invalidate;
  end;
end;

procedure TCustomDockManager.ResetBounds(Force: Boolean);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
end;

procedure TCustomDockManager.SaveToStream(Stream: TStream);
begin
end;

procedure TCustomDockManager.SetReplacingControl(Control: TControl);
begin
  inherited SetReplacingControl(Control);
end;

function TCustomDockManager.AutoFreeByControl: Boolean;
begin
  Result := inherited AutoFreeByControl;
end;

{ TDockClientPanel }

constructor TDockClientPanel.Create(TheOwner: TComponent);
begin
  inherited;
  CloseButton := TSpeedButton.Create(Self);
  with CloseButton do begin
    Parent := Self;
    Caption := 'X';
    Font.Size := 6;
    Width := 14;
    Height := 14;
    Visible := True;
    OnClick := CloseButtonClick;
  end;
  OnPaint := DockPanelPaint;
  OnMouseDown := DockPanelMouseDown;
  OnResize := ResizeExecute;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TDockClientPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TDockClientPanel.DrawGrabber(Canvas: TCanvas; AControl: TControl);
begin
  with Canvas do begin
    Brush.Color := clBtnFace;
    Pen.Color := clBlack;
    FillRect(0, 0, AControl.Width, GrabberSize);

    if (AControl as TWinControl).Focused then
      Font.Style := Font.Style + [fsBold]
      else Font.Style := Font.Style - [fsBold];
    Rectangle(1, 1, AControl.Width - 1, GrabberSize - 1);
    TextOut(6, 2, AControl.Caption);

    CloseButton.Left := AControl.Width - CloseButton.Width - 2;
    CloseButton.Top := 2;
  end;
end;

procedure TDockClientPanel.CloseButtonClick(Sender: TObject);
begin
  Control.Hide;
end;

procedure TDockClientPanel.ResizeExecute(Sender: TObject);
begin
  Control.Top := GrabberSize;
  Control.Left := 0;
  Control.Width := Width;
  Control.Height := Height - GrabberSize;
  //Control.SetBounds(0, GrabberSize, Width - Control.Left,
  //  Height - Control.Top);
end;

procedure TDockClientPanel.DockPanelPaint(Sender: TObject);
var
  I: Integer;
  R: TRect;
begin
  R := Control.ClientRect;
  Canvas.FillRect(R);
  DrawGrabber(Canvas, Control);
end;

procedure TDockClientPanel.DockPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) then begin
    DragManager.DragStart(Control, False, 1);
  end;
end;

{ TConjoinDockForm }

constructor TConjoinDockForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Panel := TPanel.Create(Self);
  with Panel do begin
    Parent := Self;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
  end;
  DragKind := dkDock;
  DragMode := dmAutomatic;
end;

initialization
  DefaultDockManagerClass := TCustomDockManager;

end.

