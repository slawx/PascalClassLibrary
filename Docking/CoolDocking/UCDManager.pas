unit UCDManager;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCDCommon, Controls, Contnrs,
  UCDPopupMenu, LCLType, LMessages, Graphics, Buttons,
  UCDConjoinForm, Menus, StdCtrls, ExtCtrls, Forms;

const
  GrabberSize = 22;

type
  THeaderPos = (hpAuto, hpLeft, hpTop, hpRight, hpBottom);

  TCDManager = class;
  TCDManagerItem = class;

  TCDHeader = class(TPanel)
  private
    procedure CloseButtonClick(Sender: TObject);
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
  public
    CloseButton: TSpeedButton;
    Title: TLabel;
    Icon: TImage;
    ManagerItem: TCDManagerItem;
    Shape: TShape;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TCDPanelHeader }

  TCDPanelHeader = class(TPanel)
  private
    FHeaderPos: THeaderPos;
    FShowHeader: Boolean;
    procedure SetHeaderPos(const AValue: THeaderPos);
  public
    Header: TCDHeader;
    ControlPanel: TPanel;
    property ShowHeader: Boolean read FShowHeader write FShowHeader;
    property HeaderPos: THeaderPos read FHeaderPos write SetHeaderPos;
    constructor Create(TheOwner: TComponent);
    destructor Destroy; override;
  end;

  { TCDManagerItem }

  TCDManagerItem = class
  private
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResizeExecute(Sender: TObject);
  public
    Control: TControl;
    Manager: TCDManager;
    procedure Paint(Sender: TObject); virtual;
    procedure VisibleChange(Sender: TObject); virtual;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCDManager }

  TCDManager = class(TCDManagerBase)
  private
    FDockSite: TWinControl;
    function GetDockSite: TWinControl;
    function GetHeaderPos: THeaderPos;
    function GetMoveDuration: Integer;
    procedure SetDockStyle(const AValue: TCDStyleType);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetMoveDuration(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
  public
    FDockStyle: TCDStyleType;
    PopupMenu: TCDPopupMenu;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
    procedure UpdateClientSize; virtual;
    procedure Switch(Index: Integer); virtual;
    procedure ChangeVisible(Control: TWinControl; Visible: Boolean); virtual;
    procedure Assign(Source: TCDManager); virtual;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); virtual;
    procedure DoSetVisible(const AValue: Boolean); virtual;

    // Inherited from TDockManager
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

    function FindControlInPanels(Control: TControl): TCDManagerItem; virtual;
    function CreateContainer(InsertAt: TAlign): TCDConjoinForm;
    property DockStyle: TCDStyleType read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read GetMoveDuration write SetMoveDuration;
    property DockSite: TWinControl read GetDockSite;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property Visible: Boolean write SetVisible;
  end;


implementation

uses
  UCDManagerRegions, UCDManagerTabs, UCDManagerRegionsPopup, UCDManagerTabsPopup;

{ TCDPanelHeader }

procedure TCDPanelHeader.SetHeaderPos(const AValue: THeaderPos);
begin
  if FHeaderPos=AValue then exit;
  FHeaderPos:=AValue;
  //Paint(Self);
end;

constructor TCDPanelHeader.Create(TheOwner: TComponent);
begin
  inherited;
  //Paint.OnPaint := Paint;
//  Header.Shape.OnMouseDown := DockPanelMouseDown;
//  Header.Title.OnMouseDown := DockPanelMouseDown;
  HeaderPos := hpTop;

  ShowHeader := True;
  ControlPanel := TPanel.Create(Self);
  with ControlPanel do begin
    Parent := Self;
    Visible := True;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    //Color := clGreen;
  end;
  Header := TCDHeader.Create(Self);
  with Header do begin
    Parent := Self;
    Visible := ShowHeader;
    Align := alTop;
    Height := GrabberSize;
    //ManagerItem := Self;
  end;
  //OnResize := ResizeExecute;
  BevelInner := bvNone;
  BevelOuter := bvNone;
end;

destructor TCDPanelHeader.Destroy;
begin
  inherited Destroy;
end;

{ TCDManagerItem }

procedure TCDManagerItem.Paint(Sender: TObject);
var
  I: Integer;
  R: TRect;
begin
(*  if not (csDesigning in ComponentState) then
  if Assigned(Control) then begin
    R := Control.ClientRect;
    Canvas.FillRect(R);
    Header.Visible := ShowHeader;
    if ShowHeader then begin
      if ClientAreaPanel.DockClientCount = 0 then
        Header.DrawGrabber(Canvas, Control) else
      Header.DrawGrabber(Canvas, ClientAreaPanel);
    end;
  end;*)
end;

constructor TCDManagerItem.Create;
begin
end;

procedure TCDManagerItem.ResizeExecute(Sender: TObject);
begin
(*  if Assigned(Control) then begin
    Control.Top := GrabberSize;
    Control.Left := 0;
    Control.Width := Width;
    Control.Height := Height - GrabberSize;
    //Control.SetBounds(0, GrabberSize, Width - Control.Left,
    //  Height - Control.Top);
  end;*)
end;

procedure TCDManagerItem.DockPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Control is TForm then begin
    //TForm(Control).SetFocus;
    Paint(Self);
  end;
  if (Button = mbLeft) then begin
    //(Control as TWinControl).DockSite := False;
    //ClientAreaPanel.DockSite := False;
    //(Control as TWinControl).BeginDrag(False, 10);
    //DragManager.DragStart(Control, False, 1);
  end;
end;

destructor TCDManagerItem.Destroy;
begin
  if Assigned(Control) then
    Control.RemoveHandlerOnVisibleChanged(VisibleChange);
  inherited Destroy;
end;

procedure TCDManagerItem.VisibleChange(Sender: TObject);
var
  ControlVisible: Boolean;
  Temp: TControl;
  Temp2: TControl;
begin
{  Temp := TControl(Sender);
  if Assigned(Control) then
  begin
    ControlVisible := TControl(Sender).Visible;
    (*if Assigned(ClientAreaPanel) then
      ClientAreaPanel.Visible := ControlVisible;
    if Assigned(Splitter) then
      Splitter.Visible := ControlVisible;
      *)
//    if Assigned(TCDManager(OwnerDockManager).DockStyleHandler) then
    if Assigned(Manager) then
    with TCDManager(Manager) do
    begin
      //UpdateClientSize;
      if ControlVisible then
        Switch(DockItems.IndexOf(FindControlInPanels(TControl(Sender))));
      if not (Control is TWinControl) then raise Exception.Create('Not TWinControl');
      if not Assigned(Control) then raise Exception.Create('Control not assigned');
      ChangeVisible(TWinControl(Control), ControlVisible);
      // Show parent control
      Temp := TControl(Sender).HostDockSite;

      if ControlVisible then
        TControl(Sender).HostDockSite.Visible := ControlVisible;
    end;
    if csDestroying in Control.ComponentState then Control := nil;
  end;}
end;

{ TCDManager }

function TCDManager.FindControlInPanels(Control: TControl
  ): TCDManagerItem;
begin
  Result := nil;
end;

function TCDManager.GetDockSite: TWinControl;
begin
  Result := FDockSite;
end;

function TCDManager.GetHeaderPos: THeaderPos;
begin

end;

function TCDManager.GetMoveDuration: Integer;
begin

end;

constructor TCDManager.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
begin
  inherited Create(ADockSite);

  FDockSite := ADockSite;

  FDockStyle := dsList; // dsNone
  PopupMenu := TCDPopupMenu.Create(Self);
  PopupMenu.Parent := ADockSite;
end;

destructor TCDManager.Destroy;
begin
  PopupMenu.Free;
  inherited Destroy;
end;

procedure TCDManager.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TCDManager.EndUpdate;
begin
  inherited EndUpdate;
end;

procedure TCDManager.GetControlBounds(Control: TControl; out
  AControlBounds: TRect);
begin
end;

function TCDManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
begin
  Result := inherited GetDockEdge(ADockObject);
end;

procedure TCDManager.InsertControl(ADockObject: TDragDockObject);
begin
  inherited InsertControl(ADockObject);
end;

procedure TCDManager.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
end;

procedure TCDManager.DoSetVisible(const AValue: Boolean);
begin

end;

procedure TCDManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewDockPanel: TCDManagerItem;
  NewPanel: TPanel;
  I: Integer;
  NewConjoinDockForm: TCDConjoinForm;
  NewDockSite: TWinControl;
  NewForm: TForm;
begin
  if (FDockSite is TForm) and (not (FDockSite is TCDConjoinForm)) then begin
    if (not Assigned(FDockSite.Parent)) then begin
      // Create conjointed form
      NewConjoinDockForm := CreateContainer(InsertAt);
      FDockSite.ManualDock(NewConjoinDockForm);
      Control.ManualDock(NewConjoinDockForm, nil, InsertAt);
    end else begin
      NewConjoinDockForm := CreateContainer(InsertAt);
      NewDockSite := FDockSite.HostDockSite;
//      FDockSite.ManualFloat(FDockSite.BoundsRect);
      NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
      FDockSite.ManualDock(NewConjoinDockForm);
      Control.ManualDock(NewConjoinDockForm, nil, InsertAt);
    end;
  end else
  if (FDockSite is TCDConjoinForm) or (FDockSite is TPanel)  then begin
    InsertControlPanel(Control, InsertAt, DropCtl);
  end;

//  FDockPanel.Invalidate;
  //inherited;
end;

procedure TCDManager.LoadFromStream(Stream: TStream);
begin
end;

procedure TCDManager.PaintSite(DC: HDC);
begin
end;

procedure TCDManager.MessageHandler(Sender: TControl;
  var Message: TLMessage);
begin
  inherited MessageHandler(Sender, Message);
end;

procedure TCDManager.PositionDockRect(ADockObject: TDragDockObject);
begin
  inherited PositionDockRect(ADockObject);
end;

procedure TCDManager.PositionDockRect(Client, DropCtl: TControl;
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

procedure TCDManager.RemoveControl(Control: TControl);
//var
//  ClientPanel: TCDClientPanel;
begin
  //DockStyleHandler.RemoveControl(Control);
  //inherited;
(*  if Control.HostDockSite = Self.FDockSite then begin
    ClientPanel := FindControlInPanels(Control);

    //if Assigned(ClientPanel) then ClientPanel.Splitter.Free;
    //Control.RemoveHandlerOnVisibleChanged(ClientPanel.VisibleChange);
    FDockPanels.Remove(ClientPanel);
    DockStyleHandler.RemoveControl(Control);
    UpdateClientSize;
    //FDockSite.Invalidate;
    //if (FDockSite is TCDConjoinForm) and (FDockSite.DockClientCount = 1) then
    //  FDockSite.Free;
    DockStyle := DockStyle;
  end;
  *)
end;

procedure TCDManager.ResetBounds(Force: Boolean);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
end;

procedure TCDManager.SaveToStream(Stream: TStream);
begin
end;

procedure TCDManager.SetReplacingControl(Control: TControl);
begin
  inherited SetReplacingControl(Control);
end;

function TCDManager.AutoFreeByControl: Boolean;
begin
  Result := inherited AutoFreeByControl;
end;

function TCDManager.CreateContainer(InsertAt: TAlign): TCDConjoinForm;
var
  NewDockSite: TWinControl;
  NewConjoinDockForm: TCDConjoinForm;
begin
  NewConjoinDockForm := TCDConjoinForm.Create(Application);
  NewConjoinDockForm.Name := GetUniqueName('ConjoinForm');
  NewConjoinDockForm.Visible := True;
  NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
  NewConjoinDockForm.CoolDockClient.Master := Self.Master;
  NewDockSite := FDockSite.HostDockSite;
  // FDockSite.ManualFloat(FDockSite.BoundsRect);
  //NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
  Result := NewConjoinDockForm;
end;

procedure TCDManager.SetDockStyle(const AValue: TCDStyleType);
var
  I: Integer;
  NewManager: TCDManager;
begin
  if FDockStyle <> AValue then begin
    FDockStyle := AValue;
    if AValue = dsTabs then begin
      NewManager := TCDManagerTabs.Create(FDockSite);
    end else
    if AValue = dsList then begin
      NewManager := TCDManagerRegions.Create(FDockSite);
    end else
    if AValue = dsPopupList then begin
      NewManager := TCDManagerPopupRegions.Create(FDockSite);
    end else
    if AValue = dsPopupTabs then begin
      NewManager := TCDStylePopupTabs.Create(FDockSite);
    end;
    if DockSite.DockManager is TCDManager then
      NewManager.Assign(TCDManager(DockSite.DockManager));
    DockSite.DockManager := NewManager;
    NewManager.UpdateClientSize;
  end;
end;

procedure TCDManager.SetHeaderPos(const AValue: THeaderPos);
begin

end;

procedure TCDManager.SetMoveDuration(const AValue: Integer);
begin
end;

procedure TCDManager.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  DoSetVisible(AValue);
  //Visible := AValue;
//  for I := 0 to DockPanels.Count - 1 do
//    TCDClientPanel(DockPanels[I]).Visible := AValue;
end;

procedure TCDManager.UpdateClientSize;
begin
end;

procedure TCDManager.Switch(Index: Integer);
begin

end;

procedure TCDManager.ChangeVisible(Control: TWinControl; Visible: Boolean);
begin

end;

procedure TCDManager.Assign(Source: TCDManager);
begin
  FDockStyle := Source.FDockStyle;
  FDockSite := Source.FDockSite;
end;

{ TCDHeader }

constructor TCDHeader.Create(TheOwner: TComponent);
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
  Title := TLabel.Create(Self);
  with Title do begin
    Parent := Self;
    Visible := True;
    Top := 4;
    Left := 6;
    BevelInner := bvNone;
    BevelOuter := bvNone;
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
  Icon := TImage.Create(Self);
  with Icon do begin
    Parent := Self;
    Left := 4;
    Top := 2;
    Visible := True;
  end;
end;

destructor TCDHeader.Destroy;
begin
  inherited Destroy;
end;

procedure TCDHeader.DrawGrabber(Canvas: TCanvas; AControl: TControl);
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

procedure TCDHeader.CloseButtonClick(Sender: TObject);
begin
  ManagerItem.Control.Hide;
end;



end.

