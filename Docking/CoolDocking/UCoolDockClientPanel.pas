unit UCoolDockClientPanel;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, Forms, StdCtrls, ExtCtrls, Graphics,
  Buttons;

type

  THeaderPos = (hpAuto, hpLeft, hpTop, hpRight, hpBottom);

  { TCoolDockHeader }

  TCoolDockClientPanel = class;

  TCoolDockHeader = class(TPanel)
  private
    procedure CloseButtonClick(Sender: TObject);
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
  public
    CloseButton: TSpeedButton;
    Title: TLabel;
    Icon: TImage;
    ParentClientPanel: TCoolDockClientPanel;
    Shape: TShape;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TCoolDockClientPanel }

  TCoolDockClientPanel = class(TPanel)
  private
    FAutoHide: Boolean;
    FHeaderPos: THeaderPos;
    FShowHeader: Boolean;
    FControl: TControl;
    function GetAutoHideEnabled: Boolean;
    function GetControl: TControl;
    procedure SetAutoHide(const AValue: Boolean);
    procedure SetAutoHideEnabled(const AValue: Boolean);
    procedure SetControl(const AValue: TControl);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetShowHeader(const AValue: Boolean);
  public
    OwnerDockManager: TObject; // TCoolDockManager;
    Splitter: TSplitter;
    ClientAreaPanel: TPanel;
    Header: TCoolDockHeader;
    procedure VisibleChange(Sender: TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockPanelPaint(Sender: TObject);
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResizeExecute(Sender: TObject);
    property ShowHeader: Boolean read FShowHeader write SetShowHeader;
    property AutoHideEnabled: Boolean read GetAutoHideEnabled
      write SetAutoHideEnabled;
    property HeaderPos: THeaderPos read FHeaderPos write SetHeaderPos;
    property Control: TControl read GetControl write SetControl;
  end;

implementation

uses
  UCoolDocking;

{ TCoolDockClientPanel }

procedure TCoolDockClientPanel.SetShowHeader(const AValue: Boolean);
begin
  if FShowHeader = AValue then Exit;
  FShowHeader := AValue;
  DockPanelPaint(Self);
end;

procedure TCoolDockClientPanel.VisibleChange(Sender: TObject);
var
  ControlVisible: Boolean;
  Temp: TControl;
  Temp2: TControl;
begin
  Temp := TControl(Sender);
  if Assigned(Control) then
  begin
    ControlVisible := TControl(Sender).Visible;
    (*if Assigned(ClientAreaPanel) then
      ClientAreaPanel.Visible := ControlVisible;
    if Assigned(Splitter) then
      Splitter.Visible := ControlVisible;
      *)
//    if Assigned(TCoolDockManager(OwnerDockManager).DockStyleHandler) then
    if Assigned(OwnerDockManager) then
    with TCoolDockManager(OwnerDockManager) do
    if Assigned(DockStyleHandler) then
    with DockStyleHandler do begin
      //UpdateClientSize;
      if ControlVisible then
        Switch(DockPanels.IndexOf(FindControlInPanels(TControl(Sender))));
      if not (Control is TWinControl) then raise Exception.Create('Not TWinControl');
      if not Assigned(Control) then raise Exception.Create('Control not assigned');
      ChangeVisible(TWinControl(Control), ControlVisible);
      // Show parent control
      Temp := TControl(Sender).HostDockSite;

      if ControlVisible then
        TControl(Sender).HostDockSite.Visible := ControlVisible;
    end;
    if csDestroying in Control.ComponentState then Control := nil;
  end;
end;

procedure TCoolDockClientPanel.SetAutoHide(const AValue: Boolean);
begin
  if FAutoHide = AValue then Exit;
  FAutoHide := AValue;
end;

function TCoolDockClientPanel.GetAutoHideEnabled: Boolean;
begin
end;

function TCoolDockClientPanel.GetControl: TControl;
begin
  Result := FControl;
end;

procedure TCoolDockClientPanel.SetAutoHideEnabled(const AValue: Boolean);
begin

end;

procedure TCoolDockClientPanel.SetControl(const AValue: TControl);
begin
  FControl := AValue;
end;

procedure TCoolDockClientPanel.SetHeaderPos(const AValue: THeaderPos);
begin
  if FHeaderPos=AValue then exit;
  FHeaderPos:=AValue;
end;

constructor TCoolDockClientPanel.Create(TheOwner: TComponent);
begin
  inherited;
  ShowHeader := True;
  Header := TCoolDockHeader.Create(Self);
  with Header do begin
    Parent := Self;
    Visible := ShowHeader;
    Align := alTop;
    Height := GrabberSize;
    ParentClientPanel := Self;
  end;
  ClientAreaPanel := TPanel.Create(Self);
  with ClientAreaPanel do begin
    Parent := Self;
    Visible := True;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    //Color := clGreen;
  end;
  Splitter := TSplitter.Create(Self);
  with Splitter do begin
    //Color := clRed;
  end;
  OnPaint := DockPanelPaint;
  Header.Shape.OnMouseDown := DockPanelMouseDown;
  OnResize := ResizeExecute;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  AutoHideEnabled := True;
  HeaderPos := hpTop;
end;

destructor TCoolDockClientPanel.Destroy;
var
  Temp: TControl;
begin
  Temp := Control;
  //if ClientAreaPanel.GetControlIndex(Control) <> - 1 then
  if Assigned(Control) then
    Control.RemoveHandlerOnVisibleChanged(VisibleChange);
  // If panel is destroyed undock docket control
  //TWinControl(Control).ManualFloat(TWinControl(Control).BoundsRect);

  inherited Destroy;
end;

procedure TCoolDockClientPanel.ResizeExecute(Sender: TObject);
begin
  if Assigned(Control) then begin
    Control.Top := GrabberSize;
    Control.Left := 0;
    Control.Width := Width;
    Control.Height := Height - GrabberSize;
    //Control.SetBounds(0, GrabberSize, Width - Control.Left,
    //  Height - Control.Top);
  end;
end;

procedure TCoolDockClientPanel.DockPanelPaint(Sender: TObject);
var
  I: Integer;
  R: TRect;
begin
  if not (csDesigning in ComponentState) then
  if Assigned(Control) then begin
    R := Control.ClientRect;
    Canvas.FillRect(R);
    Header.Visible := ShowHeader;
    if ShowHeader then begin
      if ClientAreaPanel.DockClientCount = 0 then
        Header.DrawGrabber(Canvas, Control) else
      Header.DrawGrabber(Canvas, ClientAreaPanel);
    end;
  end;
end;

procedure TCoolDockClientPanel.DockPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Control is TForm then begin
    //TForm(Control).SetFocus;
    DockPanelPaint(Self);
  end;
  if (Button = mbLeft) then begin
    //(Control as TWinControl).DockSite := False;
    ClientAreaPanel.DockSite := False;
    (Control as TWinControl).BeginDrag(False, 10);
    //DragManager.DragStart(Control, False, 1);
  end;
end;


{ TCoolDockHeader }

constructor TCoolDockHeader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Shape := TShape.Create(Self);
  with Shape do begin
    Parent := Self;
    Anchors := [akRight, akBottom, akLeft, akTop];
    Left := 1;
    Top := 1;
    Width := Self.Width - 2;
    Height := Self.Height - 2;
    Brush.Style := bsClear;
  end;
  CloseButton := TSpeedButton.Create(Self);
  with CloseButton do begin
    Parent := Self;
    Caption := 'X';
    Font.Size := 6;
    Width := GrabberSize - 8;
    Height := GrabberSize - 8;
    Anchors := [akRight, akTop];
    Left := Self.Width - Width - 4;
    Top := 4;
    Visible := True;
    OnClick := CloseButtonClick;
  end;
  Title := TLabel.Create(Self);
  with Title do begin
    Parent := Self;
    Visible := True;
    Top := 4;
    Left := 6;
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;
  Icon := TImage.Create(Self);
  with Icon do begin
    Parent := Self;
    Left := 4;
    Top := 2;
    Visible := True;
  end;
end;

destructor TCoolDockHeader.Destroy;
begin
  inherited Destroy;
end;

procedure TCoolDockHeader.DrawGrabber(Canvas: TCanvas; AControl: TControl);
begin
  with Canvas do begin
    Brush.Color := clBtnFace;
    Pen.Color := clBlack;
    //FillRect(0, 0, AControl.Width, GrabberSize);

    if (AControl as TWinControl).Focused then
      Title.Font.Style := Font.Style + [fsBold]
      else Title.Font.Style := Font.Style - [fsBold];
    Rectangle(1, 1, AControl.Width - 1, GrabberSize - 1);
    if Icon.Picture.Width > 0 then Title.Left := 8 + Icon.Picture.Width
      else Title.Left := 6;
    Title.Caption := AControl.Caption;
  end;
end;

procedure TCoolDockHeader.CloseButtonClick(Sender: TObject);
begin
  ParentClientPanel.Control.Hide;
end;


end.

