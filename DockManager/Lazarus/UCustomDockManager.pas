unit UCustomDockManager;

{$mode delphi}{$H+}

// Date: 2010-09-17

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms, ComCtrls, Dialogs, Menus, FileUtil;

const
  GrabberSize = 22;

type
  TDockDirection = (ddNone, ddHorizontal, ddVertical);
  THeaderPos = (hpAuto, hpLeft, hpTop, hpRight, hpBottom);

  TCustomDockManager = class;
  TDockClientPanel = class;

  { TConjoinDockForm }

  TConjoinDockForm = class(TForm)
    Panel: TPanel;
    constructor Create(TheOwner: TComponent); override;
  end;

  TDockStyle = (dsList, dsTabs);

  { TDockHeader }

  TDockHeader = class(TPanel)
    CloseButton: TSpeedButton;
    Title: TLabel;
    Icon: TIcon;
    ParentClientPanel: TDockClientPanel;
    Shape: TShape;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure CloseButtonClick(Sender: TObject);
    procedure DrawGrabber(Canvas: TCanvas; AControl: TControl);
  end;

  { TDockClientPanel }

  TDockClientPanel = class(TPanel)
  private
    FAutoHide: Boolean;
    FHeaderPos: THeaderPos;
    FShowHeader: Boolean;
    procedure SetAutoHide(const AValue: Boolean);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetShowHeader(const AValue: Boolean);
  public
    Header: TDockHeader;
    OwnerDockManager: TCustomDockManager;
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

  { TCustomDockManager }

  TCustomDockManager = class(TDockManager)
  private
    FMoveDuration: Integer;
    FTabsPos: THeaderPos;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    FDockStyle: TDockStyle;
    TabControl: TTabControl;
    FDockDirection: TDockDirection;
    FDockSite: TWinControl;
    FDockPanels: TObjectList; // of TDockClientPanel
    function FindControlInPanels(Control: TControl): TDockClientPanel;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
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
    procedure Timer1Timer(Sender: TObject);
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

    function CreateContainer(InsertAt: TAlign): TConjoinDockForm;
    property DockStyle: TDockStyle read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read FMoveDuration write SetMoveDuration;
    property TabsPos: THeaderPos read FTabsPos write SetTabsPos;
  end;

  { TCustomDockMaster }

  TCustomDockMaster = class(TComponent)
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

procedure Register;
begin
  RegisterComponents('CustomDocking', [TCustomDockMaster]);
end;


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

  TabControl := TTabControl.Create(FDockSite);
  with TabControl do begin
    Parent := FDockSite;
    Visible := False;
    Align := alTop;
    Height := 24;
    OnChange := TabControlChange;
    PopupMenu := PopupMenu1;
  end;
  TabsPos := hpTop;
  MoveDuration := 1000; // ms
end;

destructor TCustomDockManager.Destroy;
begin
  Timer1.Free;
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

procedure TCustomDockManager.InsertControlPanel(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewPanel: TDockClientPanel;
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
      with TDockClientPanel(FDockPanels.Last).Splitter do begin
        Parent := FDockSite;
        Visible := True;
        if FDockDirection = ddVertical then begin
          Align := alTop;
          Top := FDockSite.Height;
        end else
        if FDockDirection = ddHorizontal then begin
          Align := alLeft;
          Left := FDockSite.Width;
        end;
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
      Parent := FDockSite;
      OwnerDockManager := Self;
      if DockStyle = dsList then Visible := True;
      Align := alClient;
      PopupMenu := PopupMenu1;
    end;

    if DockStyle = dsTabs then begin
      TabControl.Tabs.Add(Control.Caption);
      TabControlChange(Self);
    end;
    NewPanel.Control := Control;
    Control.Parent := NewPanel.ClientAreaPanel;
    Control.Align := alClient;
    FDockPanels.Add(NewPanel);
    UpdateClientSize;
end;

procedure TCustomDockManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewDockPanel: TDockClientPanel;
  NewPanel: TPanel;
  I: Integer;
  NewConjoinDockForm: TConjoinDockForm;
  NewDockSite: TWinControl;
  NewForm: TForm;
begin
  if (FDockSite is TForm) then begin
    if (not Assigned(FDockSite.Parent)) then begin
      // Create conjointed form
      NewConjoinDockForm := TConjoinDockForm.Create(nil);
      NewConjoinDockForm.Visible := True;
      NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
      FDockSite.ManualDock(NewConjoinDockForm.Panel);
      Control.ManualDock(NewConjoinDockForm.Panel, nil, InsertAt);
    end else begin
      NewConjoinDockForm := TConjoinDockForm.Create(nil);
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
  if (FDockSite is TPanel) or (FDockSite is TDockClientPanel) then begin
    InsertControlPanel(Control, InsertAt, DropCtl);
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
    //if (FDockSite is TConjoinDockForm) and (FDockSite.DockClientCount = 1) then
    //  FDockSite.Free;
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

function TCustomDockManager.CreateContainer(InsertAt: TAlign): TConjoinDockForm;
var
  NewDockSite: TWinControl;
  NewConjoinDockForm: TConjoinDockForm;
begin
  NewConjoinDockForm := TConjoinDockForm.Create(nil);
  NewConjoinDockForm.Visible := True;
  NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
  NewConjoinDockForm.DragMode := dmAutomatic;
  NewConjoinDockForm.DragKind := dkDock;
  NewDockSite := FDockSite.HostDockSite;
  //      FDockSite.ManualFloat(FDockSite.BoundsRect);
  NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
  Result := NewConjoinDockForm;
end;

procedure TCustomDockManager.SetDockStyle(const AValue: TDockStyle);
var
  I: Integer;
begin
  FDockStyle := AValue;
  if AValue = dsTabs then begin
    TabControl.Visible := True;
    TabControl.Tabs.Clear;
    for I := 0 to FDockPanels.Count - 1 do begin
      TabControl.Tabs.Add(TDockClientPanel(FDockPanels[I]).Control.Caption);
      if Assigned(TDockClientPanel(FDockPanels[I]).Splitter) then
        TDockClientPanel(FDockPanels[I]).Splitter.Visible := False;
      TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := False;
      TDockClientPanel(FDockPanels[I]).Visible := False;
    end;
    TabControlChange(Self);
  end else
  if AValue = dsList then begin
    TabControl.Visible := False;
    TabControl.Tabs.Clear;
    for I := 0 to FDockPanels.Count - 1 do begin
      if Assigned(TDockClientPanel(FDockPanels[I]).Splitter) then
        TDockClientPanel(FDockPanels[I]).Splitter.Visible := True;
      TDockClientPanel(FDockPanels[I]).Visible := True;
      TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Parent := TDockClientPanel(FDockPanels[I]);
      TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := True;
    end;
  end;
  UpdateClientSize;
end;

procedure TCustomDockManager.SetMoveDuration(const AValue: Integer);
begin
  if FMoveDuration=AValue then exit;
  FMoveDuration := AValue;
  //Timer1.Interval := AValue;
end;

procedure TCustomDockManager.SetTabsPos(const AValue: THeaderPos);
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

procedure TCustomDockManager.UpdateClientSize;
var
  I: Integer;
begin
  if DockStyle = dsList then begin
    for I := 0 to FDockPanels.Count - 1 do begin
      TDockClientPanel(FDockPanels[I]).Height := FDockSite.Height div
        FDockSite.DockClientCount;
      TDockClientPanel(FDockPanels[I]).Width := FDockSite.Width div
        FDockSite.DockClientCount;
      //TDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    end;
  end else
  if DockStyle = dsTabs then begin
    for I := 0 to FDockPanels.Count - 1 do begin
      TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Width := FDockSite.Width;
      TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Height := FDockSite.Height - TabControl.Height;
      //TDockClientPanel(FDockPanels[I]).DockPanelPaint(Self);
    end;
  end;
end;

procedure TCustomDockManager.TabControlChange(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to FDockPanels.Count - 1 do begin
    TDockClientPanel(FDockPanels[I]).ClientAreaPanel.Visible := False;
  end;
  if (TabControl.TabIndex <> -1) and (FDockPanels.Count > TabControl.TabIndex) then begin
    with TDockClientPanel(FDockPanels[TabControl.TabIndex]), ClientAreaPanel do begin
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
//  TDockClientPanel(FDockPanels[TabControl.TabIndex]).Visible := True;
  end;
end;

procedure TCustomDockManager.PopupMenuTabsClick(Sender: TObject);
begin
  DockStyle := dsTabs;
end;

procedure TCustomDockManager.PopupMenuCloseClick(Sender: TObject);
begin
//  TForm(TCustomDockManager(TControl(Sender).Parent.Parent.Parent.DockManager).FDockSite).Close;
end;

procedure TCustomDockManager.PopupMenuRenameClick(Sender: TObject);
begin

end;

procedure TCustomDockManager.PopupMenuPositionAutoClick(Sender: TObject);
begin
  TabsPos := hpAuto;
end;

procedure TCustomDockManager.PopupMenuPositionLeftClick(Sender: TObject);
begin
  TabsPos := hpLeft;
end;

procedure TCustomDockManager.PopupMenuPositionRightClick(Sender: TObject);
begin
  TabsPos := hpRight;
end;

procedure TCustomDockManager.PopupMenuPositionTopClick(Sender: TObject);
begin
  TabsPos := hpTop;
end;

procedure TCustomDockManager.PopupMenuPositionBottomClick(Sender: TObject);
begin
  TabsPos := hpBottom;
end;

procedure TCustomDockManager.Timer1Timer(Sender: TObject);
begin
//  TimerMoveForm.Width := TimerMoveForm.Width
end;

procedure TCustomDockManager.PopupMenuListClick(Sender: TObject);
begin
  DockStyle := dsList;
end;

{ TDockClientPanel }

procedure TDockClientPanel.SetShowHeader(const AValue: Boolean);
begin
  if FShowHeader=AValue then exit;
  FShowHeader := AValue;
  DockPanelPaint(Self);
end;

procedure TDockClientPanel.SetAutoHide(const AValue: Boolean);
begin
  if FAutoHide=AValue then exit;
  FAutoHide:=AValue;
end;

procedure TDockClientPanel.SetHeaderPos(const AValue: THeaderPos);
begin
  if FHeaderPos=AValue then exit;
  FHeaderPos:=AValue;
end;

constructor TDockClientPanel.Create(TheOwner: TComponent);
begin
  inherited;
  Header := TDockHeader.Create(Self);
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
  OnMouseDown := DockPanelMouseDown;
  OnResize := ResizeExecute;
  BevelInner := bvNone;
  BevelOuter := bvNone;
  ShowHeader := True;
  AutoHide := False;
  HeaderPos := hpTop;
end;

destructor TDockClientPanel.Destroy;
begin
  inherited Destroy;
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

procedure TDockClientPanel.DockPanelMouseDown(Sender: TObject;
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
    BevelOuter := bvNone;
    BevelInner := bvNone;
  //  Color := clYellow;
  end;
  DragKind := dkDock;
  DragMode := dmAutomatic;
end;

{ TCustomDockMaster }

procedure TCustomDockMaster.SetTabsEnabled(const AValue: Boolean);
begin
  if FTabsEnabled=AValue then exit;
  FTabsEnabled:=AValue;
end;

procedure TCustomDockMaster.SaveLayoutToStream(Stream: TStream);
begin

end;

procedure TCustomDockMaster.LoadLayoutFromStream(Stream: TStream);
begin

end;

procedure TCustomDockMaster.SaveLayoutToFile(FileName: string);
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

procedure TCustomDockMaster.LoadLayoutFromFile(FileName: string);
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

{ TDockHeader }

constructor TDockHeader.Create(TheOwner: TComponent);
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

destructor TDockHeader.Destroy;
begin
  inherited Destroy;
end;

procedure TDockHeader.DrawGrabber(Canvas: TCanvas; AControl: TControl);
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

procedure TDockHeader.CloseButtonClick(Sender: TObject);
begin
//  Control.Hide;
end;

initialization
  DefaultDockManagerClass := TCustomDockManager;

end.

