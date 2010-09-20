unit UCoolDocking;

{$mode delphi}{$H+}

// Date: 2010-09-17

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms, ComCtrls, Dialogs, Menus, FileUtil,
  UCoolDockCustomize, DOM, XMLWrite, XMLRead;

const
  GrabberSize = 22;

type
  TDockDirection = (ddNone, ddHorizontal, ddVertical);
  THeaderPos = (hpAuto, hpLeft, hpTop, hpRight, hpBottom);

  TCoolDockManager = class;
  TCoolDockClientPanel = class;

  { TCoolDockConjoinForm }

  TCoolDockConjoinForm = class(TForm)
    Panel: TPanel;
    constructor Create(TheOwner: TComponent); override;
  end;

  TDockStyle = (dsList, dsTabs);

  { TCoolDockHeader }

  TCoolDockHeader = class(TPanel)
    CloseButton: TSpeedButton;
    Title: TLabel;
    Icon: TIcon;
    ParentClientPanel: TCoolDockClientPanel;
    Shape: TShape;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure CloseButtonClick(Sender: TObject);
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
  end;

  { TCoolDockClientPanel }

  TCoolDockClientPanel = class(TPanel)
  private
    FAutoHide: Boolean;
    FHeaderPos: THeaderPos;
    FShowHeader: Boolean;
    procedure SetAutoHide(const AValue: Boolean);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetShowHeader(const AValue: Boolean);
  public
    Header: TCoolDockHeader;
    OwnerDockManager: TCoolDockManager;
    Control: TControl;
    Splitter: TSplitter;
    ClientAreaPanel: TPanel;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DockPanelPaint(Sender: TObject);
    procedure DockPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ResizeExecute(Sender: TObject);
    property ShowHeader: Boolean read FShowHeader write SetShowHeader;
    property AutoHide: Boolean read FAutoHide write SetAutoHide;
    property HeaderPos: THeaderPos read FHeaderPos write SetHeaderPos;
  end;

  { TCoolDockManager }

  TCoolDockManager = class(TDockManager)
  private
    FMoveDuration: Integer;
    FTabsPos: THeaderPos;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    FDockStyle: TDockStyle;
    TabControl: TTabControl;
    FDockDirection: TDockDirection;
    FDockSite: TWinControl;
    FDockPanels: TObjectList; // of TCoolDockClientPanel
    function FindControlInPanels(Control: TControl): TCoolDockClientPanel;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
    procedure PopupMenuTabCloseClick(Sender: TObject);
    procedure SetDockStyle(const AValue: TDockStyle);
    procedure SetMoveDuration(const AValue: Integer);
    procedure SetTabsPos(const AValue: THeaderPos);
    procedure UpdateClientSize;
    procedure TabControlChange(Sender: TObject);
    procedure PopupMenuListClick(Sender: TObject);
    procedure PopupMenuTabsClick(Sender: TObject);
    procedure PopupMenuCloseClick(Sender: TObject);
    procedure PopupMenuRenameClick(Sender: TObject);
    procedure PopupMenuPositionAutoClick(Sender: TObject);
    procedure PopupMenuPositionLeftClick(Sender: TObject);
    procedure PopupMenuPositionRightClick(Sender: TObject);
    procedure PopupMenuPositionTopClick(Sender: TObject);
    procedure PopupMenuPositionBottomClick(Sender: TObject);
    procedure PopupMenuUndockClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;

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

    function CreateContainer(InsertAt: TAlign): TCoolDockConjoinForm;
    property DockStyle: TDockStyle read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read FMoveDuration write SetMoveDuration;
    property TabsPos: THeaderPos read FTabsPos write SetTabsPos;
  end;

  { TCoolDockMaster }

  TCoolDockMaster = class(TComponent)
  private
    FDefaultHeaderPos: THeaderPos;
    FTabsEnabled: Boolean;
    procedure SetTabsEnabled(const AValue: Boolean);
  public
    procedure SaveLayoutToStream(Stream: TStream);
    procedure LoadLayoutFromStream(Stream: TStream);
    procedure SaveLayoutToFile(FileName: string);
    procedure LoadLayoutFromFile(FileName: string);
  published
    property TabsEnabled: Boolean read FTabsEnabled write SetTabsEnabled;
    property DefaultHeaderPos: THeaderPos read FDefaultHeaderPos
      write FDefaultHeaderPos;
  end;

procedure Register;

implementation

resourcestring
  SDockStyle = 'Style';
  SDockList = 'List';
  SDockTabs = 'Tabs';
  SCloseForm = 'Close';
  SRenameForm = 'Rename';
  SPosition = 'Position';
  SPositionAuto = 'Auto';
  SPositionTop = 'Top';
  SPositionLeft = 'Left';
  SPositionRight = 'Right';
  SPositionBottom = 'Bottom';
  SUndock = 'Undock';

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCoolDockMaster]);
  RegisterComponents('CoolDocking', [TCoolDockCustomize]);
end;


{ TCoolDockManager }

function TCoolDockManager.FindControlInPanels(Control: TControl
  ): TCoolDockClientPanel;
var
  I: Integer;
begin
  I := 0;
  while (I < FDockPanels.Count) and
    (TCoolDockClientPanel(FDockPanels[I]).Control <> Control) do Inc(I);
  if I < FDockPanels.Count then Result := TCoolDockClientPanel(FDockPanels[I])
    else Result := nil;
end;

constructor TCoolDockManager.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
begin
  inherited Create(ADockSite);
  FDockSite := ADockSite;
  FDockPanels := TObjectList.Create;
  PopupMenu1 := TPopupMenu.Create(FDockSite);

  Timer1 := TTimer.Create(nil);
  Timer1.Enabled := False;
  Timer1.OnTimer := Timer1Timer;

  NewMenuItem := TMenuItem.Create(PopupMenu1);
  NewMenuItem.Caption := SDockStyle;
  PopupMenu1.Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockList;
  NewMenuItem2.OnClick := PopupMenuListClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SDockTabs;
  NewMenuItem2.OnClick := PopupMenuTabsClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(PopupMenu1);
  NewMenuItem.Caption := SPosition;
  PopupMenu1.Items.Add(NewMenuItem);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionAuto;
  NewMenuItem2.OnClick := PopupMenuPositionAutoClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionTop;
  NewMenuItem2.OnClick := PopupMenuPositionTopClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionLeft;
  NewMenuItem2.OnClick := PopupMenuPositionLeftClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionBottom;
  NewMenuItem2.OnClick := PopupMenuPositionBottomClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem2 := TMenuItem.Create(NewMenuItem);
  NewMenuItem2.Caption := SPositionRight;
  NewMenuItem2.OnClick := PopupMenuPositionRightClick;
  NewMenuItem.Add(NewMenuItem2);

  NewMenuItem := TMenuItem.Create(PopupMenu1);
  NewMenuItem.Caption := SCloseForm;
  NewMenuItem.OnClick := PopupMenuCloseClick;
  PopupMenu1.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenu1);
  NewMenuItem.Caption := SRenameForm;
  NewMenuItem.OnClick := PopupMenuRenameClick;
  PopupMenu1.Items.Add(NewMenuItem);

  NewMenuItem := TMenuItem.Create(PopupMenu1);
  NewMenuItem.Caption := SUndock;
  NewMenuItem.OnClick := PopupMenuUndockClick;
  PopupMenu1.Items.Add(NewMenuItem);

  TabControl := TTabControl.Create(FDockSite);
  with TabControl do begin
    Parent := FDockSite;
    Visible := False;
    Align := alTop;
    Height := 24;
    OnChange := TabControlChange;
    PopupMenu := PopupMenu1;
    OnMouseDown := TabControlMouseDown;
  end;
  TabsPos := hpTop;
  MoveDuration := 1000; // ms
end;

destructor TCoolDockManager.Destroy;
begin
  Timer1.Free;
  FDockPanels.Free;
  inherited Destroy;
end;

procedure TCoolDockManager.BeginUpdate;
begin
  inherited BeginUpdate;
end;

procedure TCoolDockManager.EndUpdate;
begin
  inherited EndUpdate;
end;

procedure TCoolDockManager.GetControlBounds(Control: TControl; out
  AControlBounds: TRect);
begin
end;

function TCoolDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
begin
  Result := inherited GetDockEdge(ADockObject);
end;

procedure TCoolDockManager.InsertControl(ADockObject: TDragDockObject);
begin
  inherited InsertControl(ADockObject);
end;

procedure TCoolDockManager.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewPanel: TCoolDockClientPanel;
  I: Integer;
begin
    if FDockSite.DockClientCount = 2 then begin
      if (InsertAt = alTop) or (InsertAt = alBottom) then
        FDockDirection := ddVertical
      else
      if (InsertAt = alLeft) or (InsertAt = alRight) then
        FDockDirection := ddHorizontal
      else FDockDirection := ddHorizontal;
    end;// else FDockSite.DockClientCount > 2 then begin

    //end;
    if FDockSite.DockClientCount > 1 then begin
      with TCoolDockClientPanel(FDockPanels.Last).Splitter do begin
        Parent := FDockSite;
        Visible := (DockStyle = dsList);
        if FDockDirection = ddVertical then begin
          Align := alTop;
          Top := FDockSite.Height;
        end else
        if FDockDirection = ddHorizontal then begin
          Align := alLeft;
          Left := FDockSite.Width;
        end;
      end;

      with TCoolDockClientPanel(FDockPanels.Last) do
      if FDockDirection = ddVertical then
        Align := alTop
      else
      if FDockDirection = ddHorizontal then
        Align := alLeft;
    end;
    NewPanel := TCoolDockClientPanel.Create(nil);
    with NewPanel do begin
      Parent := FDockSite;
      OwnerDockManager := Self;
      if DockStyle = dsList then Visible := True;
      Align := alClient;
    end;

    if DockStyle = dsTabs then begin
      TabControl.Tabs.Add(Control.Caption);
      if Assigned(NewPanel.Splitter) then
        NewPanel.Splitter.Visible := False;
      NewPanel.ClientAreaPanel.Visible := False;
      NewPanel.Visible := False;
      TabControlChange(Self);
    end;
    NewPanel.Control := Control;
    Control.Parent := NewPanel.ClientAreaPanel;
    Control.Align := alClient;
    FDockPanels.Add(NewPanel);
    UpdateClientSize;
end;

procedure TCoolDockManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewDockPanel: TCoolDockClientPanel;
  NewPanel: TPanel;
  I: Integer;
  NewConjoinDockForm: TCoolDockConjoinForm;
  NewDockSite: TWinControl;
  NewForm: TForm;
begin
  if (FDockSite is TForm) then begin
    if (not Assigned(FDockSite.Parent)) then begin
      // Create conjointed form
      NewConjoinDockForm := TCoolDockConjoinForm.Create(Application);
      NewConjoinDockForm.Visible := True;
      NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
      FDockSite.ManualDock(NewConjoinDockForm.Panel);
      Control.ManualDock(NewConjoinDockForm.Panel, nil, InsertAt);
    end else begin
      NewConjoinDockForm := TCoolDockConjoinForm.Create(Application);
      NewConjoinDockForm.Visible := True;
      NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
      NewConjoinDockForm.DragMode := dmAutomatic;
      NewConjoinDockForm.DragKind := dkDock;
      NewDockSite := FDockSite.HostDockSite;
//      FDockSite.ManualFloat(FDockSite.BoundsRect);
      NewConjoinDockForm.ManualDock(NewDockSite);
      FDockSite.ManualDock(NewConjoinDockForm.Panel);
      Control.ManualDock(NewConjoinDockForm.Panel, nil, InsertAt);
    end;
  end else
  if (FDockSite is TPanel) or (FDockSite is TCoolDockClientPanel) then begin
    InsertControlPanel(Control, InsertAt, DropCtl);
  end;

//  FDockPanel.Invalidate;
  inherited;
end;

procedure TCoolDockManager.LoadFromStream(Stream: TStream);
begin
end;

procedure TCoolDockManager.PaintSite(DC: HDC);
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  for I := 0 to FDockPanels.Count - 1 do
    with TCoolDockClientPanel(FDockPanels[I]) do begin
      Invalidate;
    end;
end;

procedure TCoolDockManager.MessageHandler(Sender: TControl;
  var Message: TLMessage);
begin
  inherited MessageHandler(Sender, Message);
end;

procedure TCoolDockManager.PositionDockRect(ADockObject: TDragDockObject);
begin
  inherited PositionDockRect(ADockObject);
end;

procedure TCoolDockManager.PositionDockRect(Client, DropCtl: TControl;
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

procedure TCoolDockManager.RemoveControl(Control: TControl);
var
  ClientPanel: TCoolDockClientPanel;
begin
  inherited;
  if Control.HostDockSite = Self.FDockSite then begin
    ClientPanel := FindControlInPanels(Control);
    //if Assigned(ClientPanel) then ClientPanel.Splitter.Free;
    FDockPanels.Remove(ClientPanel);
    if FDockSite.DockClientCount = 2 then FDockDirection := ddNone;
    UpdateClientSize;
    //FDockSite.Invalidate;
    //if (FDockSite is TCoolDockConjoinForm) and (FDockSite.DockClientCount = 1) then
    //  FDockSite.Free;
    DockStyle := DockStyle;
  end;
end;

procedure TCoolDockManager.ResetBounds(Force: Boolean);
var
  I: Integer;
  Control: TControl;
  R: TRect;
begin
end;

procedure TCoolDockManager.SaveToStream(Stream: TStream);
begin
end;

procedure TCoolDockManager.SetReplacingControl(Control: TControl);
begin
  inherited SetReplacingControl(Control);
end;

function TCoolDockManager.AutoFreeByControl: Boolean;
begin
  Result := inherited AutoFreeByControl;
end;

function TCoolDockManager.CreateContainer(InsertAt: TAlign): TCoolDockConjoinForm;
var
  NewDockSite: TWinControl;
  NewConjoinDockForm: TCoolDockConjoinForm;
begin
  NewConjoinDockForm := TCoolDockConjoinForm.Create(Application);
  NewConjoinDockForm.Visible := True;
  NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
  NewConjoinDockForm.DragMode := dmAutomatic;
  NewConjoinDockForm.DragKind := dkDock;
  NewDockSite := FDockSite.HostDockSite;
  //      FDockSite.ManualFloat(FDockSite.BoundsRect);
  NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
  Result := NewConjoinDockForm;
end;

procedure TCoolDockManager.SetDockStyle(const AValue: TDockStyle);
var
  I: Integer;
begin
  FDockStyle := AValue;
  if AValue = dsTabs then begin
    TabControl.Visible := True;
    TabControl.Tabs.Clear;
    for I := 0 to FDockPanels.Count - 1 do begin
      TabControl.Tabs.Add(TCoolDockClientPanel(FDockPanels[I]).Control.Caption);
      if Assigned(TCoolDockClientPanel(FDockPanels[I]).Splitter) then
        TCoolDockClientPanel(FDockPanels[I]).Splitter.Visible := False;
      TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := False;
      TCoolDockClientPanel(FDockPanels[I]).Visible := False;
    end;
    TabControlChange(Self);
  end else
  if AValue = dsList then begin
    TabControl.Visible := False;
    TabControl.Tabs.Clear;
    for I := 0 to FDockPanels.Count - 1 do begin
      if Assigned(TCoolDockClientPanel(FDockPanels[I]).Splitter) then
        TCoolDockClientPanel(FDockPanels[I]).Splitter.Visible := True;
      TCoolDockClientPanel(FDockPanels[I]).Visible := True;
      TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Parent := TCoolDockClientPanel(FDockPanels[I]);
      TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := True;
    end;
  end;
  UpdateClientSize;
end;

procedure TCoolDockManager.SetMoveDuration(const AValue: Integer);
begin
  if FMoveDuration=AValue then exit;
  FMoveDuration := AValue;
  //Timer1.Interval := AValue;
end;

procedure TCoolDockManager.SetTabsPos(const AValue: THeaderPos);
begin
  if FTabsPos=AValue then exit;
  FTabsPos := AValue;
  with TabControl do
  case AValue of
    hpAuto, hpTop: begin
      Align := alTop;
      TabPosition := tpTop;
      Height := GrabberSize;
    end;
    hpLeft: begin
      Align := alLeft;
      TabPosition := tpLeft;
      Width := GrabberSize;
    end;
    hpRight: begin
      Align := alRight;
      TabPosition := tpRight;
      Width := GrabberSize;
    end;
    hpBottom: begin
      Align := alBottom;
      TabPosition := tpBottom;
      Height := GrabberSize;
    end;
  end;
end;

procedure TCoolDockManager.UpdateClientSize;
var
  I: Integer;
begin
  if DockStyle = dsList then begin
    for I := 0 to FDockPanels.Count - 1 do begin
      TCoolDockClientPanel(FDockPanels[I]).Height := FDockSite.Height div
        FDockSite.DockClientCount;
      TCoolDockClientPanel(FDockPanels[I]).Width := FDockSite.Width div
        FDockSite.DockClientCount;
      //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    end;
  end else
  if DockStyle = dsTabs then begin
    for I := 0 to FDockPanels.Count - 1 do begin
      TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Width := FDockSite.Width;
      TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Height := FDockSite.Height - TabControl.Height;
      //TCoolDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    end;
  end;
end;

procedure TCoolDockManager.TabControlChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FDockPanels.Count - 1 do begin
    TCoolDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := False;
  end;
  if (TabControl.TabIndex <> -1) and (FDockPanels.Count > TabControl.TabIndex) then begin
    with TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]), ClientAreaPanel do begin
      if AutoHide then begin
        Parent := nil;
        Visible := True;
        Width := 0;
        //TimerMoveForm :=
        //TimerIncrement := 1;
        Timer1.Interval := MoveDuration div 10;
        Timer1.Enabled := True;
      end else begin
        Parent := FDockSite;
        Visible := True;
        UpdateClientSize;
      end;
    end;
//  TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).Visible := True;
  end;
end;

procedure TCoolDockManager.PopupMenuTabsClick(Sender: TObject);
begin
  DockStyle := dsTabs;
end;

procedure TCoolDockManager.PopupMenuCloseClick(Sender: TObject);
begin
  TForm(TCoolDockManager(TControl(Sender).Parent.Parent.Parent.DockManager).FDockSite).Close;
end;

procedure TCoolDockManager.PopupMenuTabCloseClick(Sender: TObject);
begin
  if TabControl.TabIndex <> -1 then
    TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).Control.Hide;
end;

procedure TCoolDockManager.PopupMenuRenameClick(Sender: TObject);
begin

end;

procedure TCoolDockManager.PopupMenuPositionAutoClick(Sender: TObject);
begin
  TabsPos := hpAuto;
end;

procedure TCoolDockManager.PopupMenuPositionLeftClick(Sender: TObject);
begin
  TabsPos := hpLeft;
end;

procedure TCoolDockManager.PopupMenuPositionRightClick(Sender: TObject);
begin
  TabsPos := hpRight;
end;

procedure TCoolDockManager.PopupMenuPositionTopClick(Sender: TObject);
begin
  TabsPos := hpTop;
end;

procedure TCoolDockManager.PopupMenuPositionBottomClick(Sender: TObject);
begin
  TabsPos := hpBottom;
end;

procedure TCoolDockManager.PopupMenuUndockClick(Sender: TObject);
begin

end;

procedure TCoolDockManager.Timer1Timer(Sender: TObject);
begin
//  TimerMoveForm.Width := TimerMoveForm.Width
end;

procedure TCoolDockManager.TabControlMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (TabControl.TabIndex <> -1) then begin
    DragManager.DragStart(TCoolDockClientPanel(FDockPanels[TabControl.TabIndex]).Control, False, 1);
  end;
end;

procedure TCoolDockManager.PopupMenuListClick(Sender: TObject);
begin
  DockStyle := dsList;
end;

{ TCoolDockClientPanel }

procedure TCoolDockClientPanel.SetShowHeader(const AValue: Boolean);
begin
  if FShowHeader=AValue then exit;
  FShowHeader := AValue;
  DockPanelPaint(Self);
end;

procedure TCoolDockClientPanel.SetAutoHide(const AValue: Boolean);
begin
  if FAutoHide=AValue then exit;
  FAutoHide:=AValue;
end;

procedure TCoolDockClientPanel.SetHeaderPos(const AValue: THeaderPos);
begin
  if FHeaderPos=AValue then exit;
  FHeaderPos:=AValue;
end;

constructor TCoolDockClientPanel.Create(TheOwner: TComponent);
begin
  inherited;
  Header := TCoolDockHeader.Create(Self);
  with Header do begin
    Parent := Self;
    Visible := ShowHeader;
    Align := alTop;
    Height := GrabberSize;
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
  ShowHeader := True;
  AutoHide := False;
  HeaderPos := hpTop;
end;

destructor TCoolDockClientPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TCoolDockClientPanel.ResizeExecute(Sender: TObject);
begin
  Control.Top := GrabberSize;
  Control.Left := 0;
  Control.Width := Width;
  Control.Height := Height - GrabberSize;
  //Control.SetBounds(0, GrabberSize, Width - Control.Left,
  //  Height - Control.Top);
end;

procedure TCoolDockClientPanel.DockPanelPaint(Sender: TObject);
var
  I: Integer;
  R: TRect;
begin
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
    DragManager.DragStart(Control, False, 1);
  end;
end;

{ TCoolDockConjoinForm }

constructor TCoolDockConjoinForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Panel := TPanel.Create(Self);
  with Panel do begin
    Parent := Self;
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelOuter := bvNone;
    BevelInner := bvNone;
  //  Color := clYellow;
  end;
  DragKind := dkDock;
  DragMode := dmAutomatic;
end;

{ TCoolDockMaster }

procedure TCoolDockMaster.SetTabsEnabled(const AValue: Boolean);
begin
  if FTabsEnabled=AValue then exit;
  FTabsEnabled:=AValue;
end;

procedure TCoolDockMaster.SaveLayoutToStream(Stream: TStream);
var
  Doc: TXMLDocument;
  RootNode: TDOMNode;
  NewNode: TDOMNode;
  NewNode2: TDOMNode;
  I: Integer;
begin
  Doc := TXMLDocument.Create;
  with Doc do try
    RootNode := CreateElement('DockLayout');
    AppendChild(RootNode);
    with RootNode do begin
      for I := 0 to Application.ComponentCount - 1 do begin
        if Application.Components[I] is TForm then
        with Application.Components[I] as TForm do
        if Assigned(HostDockSite) then
        begin
          NewNode := OwnerDocument.CreateElement('Form');

          if HostDockSite.Parent is TForm then begin
            NewNode2 := OwnerDocument.CreateElement('ParentFormName');
            NewNode2.TextContent := UTF8Decode(HostDockSite.Parent.Name);
            NewNode.AppendChild(NewNode2);

            NewNode2 := OwnerDocument.CreateElement('ParentFormClassName');
            NewNode2.TextContent := UTF8Decode(HostDockSite.Parent.ClassName);
            NewNode.AppendChild(NewNode2);
          end;

          NewNode2 := OwnerDocument.CreateElement('Name');
          NewNode2.TextContent := UTF8Decode(Name);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('Caption');
          NewNode2.TextContent := UTF8Decode(Caption);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('Width');
          NewNode2.TextContent := IntToStr(Width);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('Height');
          NewNode2.TextContent := IntToStr(Height);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('UndockWidth');
          NewNode2.TextContent := IntToStr(UndockWidth);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('UndockHeight');
          NewNode2.TextContent := IntToStr(UndockHeight);
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('FormState');
          NewNode2.TextContent := IntToStr(Integer(FormState));
          NewNode.AppendChild(NewNode2);

          NewNode2 := OwnerDocument.CreateElement('Visible');
          NewNode2.TextContent := IntToStr(Integer(Visible));
          NewNode.AppendChild(NewNode2);

          AppendChild(NewNode);
        end;
      end;
    end;
    WriteXMLFile(Doc, Stream);
  finally
    Free;
  end;
end;

procedure TCoolDockMaster.LoadLayoutFromStream(Stream: TStream);
begin

end;

procedure TCoolDockMaster.SaveLayoutToFile(FileName: string);
var
  LayoutFile: TFileStream;
begin
  if FileExistsUTF8(FileName) then
  LayoutFile := TFileStream.Create(FileName, fmOpenReadWrite)
  else LayoutFile := TFileStream.Create(FileName, fmCreate);
  try
    SaveLayoutToStream(LayoutFile);
  finally
    Free;
  end;
end;

procedure TCoolDockMaster.LoadLayoutFromFile(FileName: string);
var
  LayoutFile: TFileStream;
begin
  LayoutFile := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadLayoutFromStream(LayoutFile);
  finally
    Free;
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
    Title.Caption := AControl.Caption;
  end;
end;

procedure TCoolDockHeader.CloseButtonClick(Sender: TObject);
begin
//  Control.Hide;
end;

initialization
  DefaultDockManagerClass := TCoolDockManager;

end.

