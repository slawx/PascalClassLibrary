unit UCustomDockManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls;

const
  GrabberSize = 18;

type

  { TCustomDockManager }

  TCustomDockManager = class(TDockManager)
  private
    FDockSite: TWinControl;
    FDockPanel: TPanel;
    CloseButton: TSpeedButton;
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
    procedure CloseButtonClick(Sender: TObject);
    procedure DockPanelPaint(Sender: TObject);
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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

procedure TCustomDockManager.DrawGrabber(Canvas: TCanvas; AControl: TControl);
begin
  with Canvas do begin
    Brush.Color := clBtnFace;
    Pen.Color := clBlack;
    FillRect(0, 0, AControl.Width, GrabberSize);
    Rectangle(1, 1, AControl.Width - 1, GrabberSize - 1);
    TextOut(6, 2, AControl.Caption);

    CloseButton.Left := AControl.Width - CloseButton.Width - 2;
    CloseButton.Top := 2;
  end;
end;

procedure TCustomDockManager.CloseButtonClick(Sender: TObject);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
  for I := 0 to FDockSite.ControlCount - 1 do
    begin
      Control := FDockSite.Controls[I];
      if Control.Visible and (Control.HostDockSite = FDockSite) then
      begin
        Control.Hide;
      end;
    end;
  FDockPanel.Hide;
end;

procedure TCustomDockManager.DockPanelPaint(Sender: TObject);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
  CloseButton.Visible := FDockSite.DockClientCount > 0;
  for I := 0 to FDockSite.DockClientCount - 1 do begin
    Control := FDockSite.DockClients[I];
    if Control.Visible and (Control.HostDockSite = FDockSite) then
    begin
      R := Control.BoundsRect;
      //Control.SetBounds(0, GrabberSize, FDockSite.Width - Control.Left,
      //  FDockSite.Height - Control.Top);
      //Canvas.FillRect(R);
      DrawGrabber(FDockPanel.Canvas, Control);
    end;
  end;
end;

procedure TCustomDockManager.DockPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbLeft) and (FDockSite.DockClientCount > 0) then
    DragManager.DragStart(FDockSite.DockClients[0], False, 1);
end;

constructor TCustomDockManager.Create(ADockSite: TWinControl);
begin
  FDockSite := ADockSite;
  FDockPanel := TPanel.Create(nil);
  with FDockPanel do begin
    Parent := ADockSite;
    Align := alClient;
    Visible := True;
    OnPaint := DockPanelPaint;
    OnMouseDown := DockPanelMouseDown();
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;

  CloseButton := TSpeedButton.Create(FDockPanel);
  with CloseButton do begin
    Parent := FDockPanel;
    Caption := 'X';
    Font.Size := 6;
    Width := 14;
    Height := 14;
    Visible := True;
    OnClick := CloseButtonClick;
  end;
  inherited Create(ADockSite);
end;

destructor TCustomDockManager.Destroy;
begin
  FDockPanel.Free;
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
  FDockPanel.Repaint;
end;

procedure TCustomDockManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
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
  Canvas := TControlCanvas.Create;
  try
    //Canvas.Control := FDockSite;
    Canvas.Control := FDockPanel;
    Canvas.Lock;
    try
      Canvas.Handle := DC;
      try
        for I := 0 to FDockSite.ControlCount - 1 do
        begin
          Control := FDockSite.Controls[I];
          if Control.Visible and (Control.HostDockSite = FDockSite) then
          begin
            R := Control.BoundsRect;
            Control.SetBounds(0, GrabberSize, FDockSite.Width - Control.Left,
              FDockSite.Height - Control.Top);
            Canvas.FillRect(R);
            DrawGrabber(Canvas, Control);
          end;
        end;
      finally
        Canvas.Handle := 0;
      end;
    finally
      Canvas.Unlock;
    end;
  finally
    Canvas.Free;
  end;
  FDockPanel.Repaint;
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
  DockRect := Rect(0, 0, FDockSite.ClientWidth, FDockSite.ClientHeight);
  DockRect.TopLeft := FDockSite.ClientToScreen(DockRect.TopLeft);
  DockRect.BottomRight := FDockSite.ClientToScreen(DockRect.BottomRight);
end;

procedure TCustomDockManager.RemoveControl(Control: TControl);
begin
  inherited;
  //FDockPanel.Invalidate;
  FDockSite.Invalidate;
end;

procedure TCustomDockManager.ResetBounds(Force: Boolean);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
  for I := 0 to FDockSite.ControlCount - 1 do
    begin
      Control := FDockSite.Controls[I];
      if Control.Visible and (Control.HostDockSite = FDockSite) then
      begin
        R := Control.BoundsRect;
        Control.SetBounds(0, GrabberSize, FDockSite.Width - Control.Left,
          FDockSite.Height - Control.Top);
      end;
    end;
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

initialization
  DefaultDockManagerClass := TCustomDockManager;

end.

