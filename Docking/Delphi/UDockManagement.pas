unit UDockManagement;

interface

uses
  Forms, Classes, Types, Windows, Messages, SysUtils, Variants, Graphics,
  Controls, Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls, XPMan, Grids,
  FindFile, Math, Tabs, DockTabSet, ToolWin, ImgList, ActnList, URegistry;

const
  RegistryRootKey = HKEY_CURRENT_USER;

type
  TDockFormInitState = class
    DockSite: string;
    Width: Integer;
    Height: Integer;
    Form: TForm;
    Visible: Boolean;
  end;

  TDockPanelAction = (dpaUndock, dpaDock, dpaClose, dpaShow);

  TDDockForm = class(TForm)
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
  public
    FormInitStateList: TList;
    constructor Create(AOwner: TComponent); override;
  end;

  TDDockPanel = class(TPanel)
    procedure DoDockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
  end;

  TDDockTabSet = class(TDockTabSet)
    procedure DockDrop(Source: TDragDockObject; X, Y: Integer); override;
  private
    procedure TabRemoved(Sender: TObject);
    property OnTabRemoved;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TDBaseDockPanel = class(TPanel)
    PanelCenter: TDDockPanel;
    PanelLeft: TDDockPanel;
    PanelTop: TDDockPanel;
    PanelRight: TDDockPanel;
    PanelBottom: TDDockPanel;
    DockTabSetLeft: TDDockTabSet;
    DockTabSetTop: TDDockTabSet;
    DockTabSetRight: TDDockTabSet;
    DockTabSetBottom: TDDockTabSet;
    SplitterLeft: TSplitter;
    SplitterTop: TSplitter;
    SplitterRight: TSplitter;
    SplitterBottom: TSplitter;
  public
    DockPanelWidth: Integer;
    procedure DockPanelCloseForm(Source: TForm; Panel: TPanel; Action: TDockPanelAction);
    constructor Create(AOwner: TComponent); override;
  end;

  TDDockManager = class
    BaseDockPanel: TDBaseDockPanel;
    RegistryKey: string;
    DockFormList: TList;
    DockFormInitStateList: TList;
    Owner: TComponent;
    procedure SaveToRegistry;
    procedure LoadFromRegistry;
    procedure RegisterDockForm(AForm: TForm; InitWidth, InitHeight: Integer; InitDockSite: string; InitVisible: Boolean);
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;


implementation

uses Registry;


{ TDDockManager }

procedure TDDockManager.RegisterDockForm(AForm: TForm; InitWidth, InitHeight: Integer; InitDockSite: string; InitVisible: Boolean);
begin
  with TDockFormInitState(DockFormInitStateList[DockFormInitStateList.Add(TDockFormInitState.Create)]) do begin
    DockSite := InitDockSite;
    Visible := InitVisible;
    Width := InitWidth;
    Height := InitHeight;
    Form := AForm;
    Owner.InsertComponent(Form);
  end;
end;

procedure TDDockManager.SaveToRegistry;
var
  I: Integer;
  DockData: TMemoryStream;
  Buffer: array of Byte;
begin
  with TRegistryEx.Create do
  try
    RootKey := RegistryRootKey;
    for I := 0 to DockFormInitStateList.Count - 1 do
    with TDockFormInitState(DockFormInitStateList[I]).Form do begin
      OpenKey(RegistryKey + '\Dock\' + Name, True);
      WriteBool('Visible', Visible);
      WriteInteger('LRDockWidth', LRDockWidth);
      WriteInteger('TBDockHeight', TBDockHeight);
      WriteInteger('UndockWidth', UndockWidth);
      WriteInteger('UndockHeight', UndockHeight);
      WriteInteger('Width', Width);
      WriteInteger('Height', Height);
      WriteInteger('Top', Top);
      WriteInteger('Left', Left);
      WriteInteger('Orientation', Integer(DockOrientation));
      WriteBool('Docked', not Floating);
      if not Floating then WriteString('TargetDockSite', HostDockSite.Name);
      WriteInteger('Align', Integer(alLeft));
    end;
    OpenKey(RegistryKey + '\Dock', True);
    if BaseDockPanel.PanelTop.Height = 0 then WriteInteger('PanelTopHeight', BaseDockPanel.PanelTop.Tag)
      else WriteInteger('PanelTopHeight', BaseDockPanel.PanelTop.Height);
    if BaseDockPanel.PanelLeft.Width = 0 then WriteInteger('PanelLeftWidth', BaseDockPanel.PanelLeft.Tag)
      else WriteInteger('PanelLeftWidth', BaseDockPanel.PanelLeft.Width);
    if BaseDockPanel.PanelRight.Width = 0 then WriteInteger('PanelRightWidth', BaseDockPanel.PanelRight.Tag)
      else WriteInteger('PanelRightWidth', BaseDockPanel.PanelRight.Width);
    if BaseDockPanel.PanelBottom.Height = 0 then WriteInteger('PanelBottomHeight', BaseDockPanel.PanelBottom.Tag)
      else WriteInteger('PanelBottomHeight', BaseDockPanel.PanelBottom.Height);

    with BaseDockPanel do begin
      DockData := TMemoryStream.Create;
      PanelTop.DockManager.SaveToStream(DockData);
      SetLength(Buffer, DockData.Size);
      DockData.Position := 0;
      DockData.Read(Buffer[0], Length(Buffer));
      WriteBinaryData('PanelTopDockZone', Buffer[0], Length(Buffer));

      DockData.Clear;
      PanelLeft.DockManager.SaveToStream(DockData);
      SetLength(Buffer, DockData.Size);
      DockData.Position := 0;
      DockData.Read(Buffer[0], Length(Buffer));
      WriteBinaryData('PanelLeftDockZone', Buffer[0], Length(Buffer));

      DockData.Clear;
      Panelright.DockManager.SaveToStream(DockData);
      SetLength(Buffer, DockData.Size);
      DockData.Position := 0;
      DockData.Read(Buffer[0], Length(Buffer));
      WriteBinaryData('PanelRightDockZone', Buffer[0], Length(Buffer));

      DockData.Clear;
      PanelBottom.DockManager.SaveToStream(DockData);
      SetLength(Buffer, DockData.Size);
      DockData.Position := 0;
      DockData.Read(Buffer[0], Length(Buffer));
      WriteBinaryData('PanelBottomDockZone', Buffer[0], Length(Buffer));

      DockData.Clear;
      PanelCenter.DockManager.SaveToStream(DockData);
      SetLength(Buffer, DockData.Size);
      DockData.Position := 0;
      DockData.Read(Buffer[0], Length(Buffer));
      WriteBinaryData('PanelCenterDockZone', Buffer[0], Length(Buffer));

      DockData.Free;
    end;
  finally
    Free;
  end;
end;

constructor TDDockManager.Create(AOwner: TComponent);
begin
  DockFormInitStateList := TList.Create;
  DockFormList := TList.Create;
  Owner := AOwner;
end;

destructor TDDockManager.Destroy;
var
  I: Integer;
begin
  inherited;
  DockFormList.Destroy;
  for I := 0 to DockFormInitStateList.Count - 1 do
    TDockFormInitState(DockFormInitStateList[I]).Destroy;
  DockFormInitStateList.Destroy;
end;

procedure TDDockManager.LoadFromRegistry;
const
  ReadBufferSite = 100000;
var
  I: Integer;
  DockData: TMemoryStream;
  Buffer: array of Byte;
begin
  with TRegistryEx.Create do
  try
    RootKey := RegistryRootKey;
    OpenKey(RegistryKey + '\Dock', True);

    with BaseDockPanel do begin
      PanelTop.Tag := ReadIntegerWithDefault('PanelTopHeight', PanelTop.Tag);
      if PanelTop.Tag > Height then PanelTop.Tag := Height div 2;
      if PanelTop.Tag = 0 then PanelTop.Tag := 10;

      PanelLeft.Tag := ReadIntegerWithDefault('PanelLeftWidth', PanelLeft.Tag);
      if PanelLeft.Tag > Width then PanelLeft.Tag := Width div 2;
      if PanelLeft.Tag = 0 then PanelLeft.Tag := 10;

      PanelRight.Tag := ReadIntegerWithDefault('PanelRightWidth', PanelRight.Tag);
      if PanelRight.Tag > Width then PanelRight.Tag := Width div 2;
      if PanelRight.Tag = 0 then PanelRight.Tag := 10;

      PanelBottom.Tag := ReadIntegerWithDefault('PanelBottomHeight', PanelBottom.Tag);
      if PanelBottom.Tag > Height then PanelBottom.Tag := Height div 2;
      if PanelBottom.Tag = 0 then PanelBottom.Tag := 10;
    end;

  for I := 0 to DockFormInitStateList.Count - 1 do
    with TDockFormInitState(DockFormInitStateList[I]) do begin
      OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
      Form.DockOrientation := TDockOrientation(ReadIntegerWithDefault('Orientation', 0));
      Form.TBDockHeight := ReadIntegerWithDefault('TBDockHeight', Height);
      Form.LRDockWidth := ReadIntegerWithDefault('LRDockWidth', Width);
      Form.UndockHeight := ReadIntegerWithDefault('UndockHeight', Height);
      Form.UndockWidth := ReadIntegerWithDefault('UndockWidth', Width);
      Form.Width := ReadIntegerWithDefault('Width', Width);
      Form.Height := ReadIntegerWithDefault('Height', Height);
      Form.Top := ReadIntegerWithDefault('Top', (Screen.Width - Width) div 2);
      Form.Left := ReadIntegerWithDefault('Left', (Screen.Width - Width) div 2);
      if ReadBoolWithDefault('Docked', True) then begin
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'DockTabSetLeft' then begin
          Visible := True;
          Form.Visible := True;
          BaseDockPanel.DockTabSetLeft.Visible := True;
          Form.ManualDock(BaseDockPanel.DockTabSetLeft, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'DockTabSetTop' then begin
          Visible := True;
          Form.Visible := True;
          BaseDockPanel.DockTabSetTop.Visible := True;
          Form.ManualDock(BaseDockPanel.DockTabSetTop, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'DockTabSetRight' then begin
          Visible := True;
          Form.Visible := True;
          BaseDockPanel.DockTabSetRight.Visible := True;
          Form.ManualDock(BaseDockPanel.DockTabSetRight, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'DockTabSetBottom' then begin
          Visible := True;
          Form.Visible := True;
          BaseDockPanel.DockTabSetBottom.Visible := True;
          Form.ManualDock(BaseDockPanel.DockTabSetBottom, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'PanelTop' then begin
          OpenKey(RegistryKey + '\Dock', True);
          if not ValueExists('PanelTopDockZone') then Form.ManualDock(BaseDockPanel.PanelTop, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
          OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
          Form.Visible := ReadBoolWithDefault('Visible', Visible);
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'PanelLeft' then begin
          OpenKey(RegistryKey + '\Dock', True);
          if not ValueExists('PanelLeftDockZone') then Form.ManualDock(BaseDockPanel.PanelLeft, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
          OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
          Form.Visible := ReadBoolWithDefault('Visible', Visible);
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'PanelRight' then begin
          OpenKey(RegistryKey + '\Dock', True);
          if not ValueExists('PanelRightDockZone') then Form.ManualDock(BaseDockPanel.PanelRight, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
          OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
          Form.Visible := ReadBoolWithDefault('Visible', Visible);
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'PanelBottom' then begin
          OpenKey(RegistryKey + '\Dock', True);
          if not ValueExists('PanelBottomDockZone') then Form.ManualDock(BaseDockPanel.PanelBottom, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
          OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
          Form.Visible := ReadBoolWithDefault('Visible', Visible);
        end;
        if ReadStringWithDefault('TargetDockSite', DockSite) = 'PanelCenter' then begin
          OpenKey(RegistryKey + '\Dock', True);
          if not ValueExists('PanelCenterDockZone') then Form.ManualDock(BaseDockPanel.PanelCenter, Form, TAlign(ReadIntegerWithDefault('Align', 0)));
          OpenKey(RegistryKey + '\Dock\' + Form.Name, True);
          Form.Visible := ReadBoolWithDefault('Visible', Visible);
        end;
      end;
    end;

    OpenKey(RegistryKey + '\Dock', True);
    with BaseDockPanel do begin
      DockData := TMemoryStream.Create;
      if ValueExists('PanelTopDockZone') then begin
        SetLength(Buffer, ReadBufferSite);
        SetLength(Buffer, ReadBinaryData('PanelTopDockZone', Buffer[0], Length(Buffer)));
        DockData.Clear;
        DockData.Write(Buffer[0], Length(Buffer));
        DockData.Position := 0;
        PanelTop.DockManager.LoadFromStream(DockData);
      end;
      if ValueExists('PanelLeftDockZone') then begin
        SetLength(Buffer, ReadBufferSite);
        SetLength(Buffer, ReadBinaryData('PanelLeftDockZone', Buffer[0], Length(Buffer)));
        DockData.Clear;
        DockData.Write(Buffer[0], Length(Buffer));
        DockData.Position := 0;
        PanelLeft.DockManager.LoadFromStream(DockData);
        //DockTree(PanelLeft.DockManager).
      end;
      if ValueExists('PanelRightDockZone') then begin
        SetLength(Buffer, ReadBufferSite);
        SetLength(Buffer, ReadBinaryData('PanelRightDockZone', Buffer[0], Length(Buffer)));
        DockData.Clear;
        DockData.Write(Buffer[0], Length(Buffer));
        DockData.Position := 0;
        PanelRight.DockManager.LoadFromStream(DockData);
      end;
      if ValueExists('PanelBottomDockZone') then begin
        SetLength(Buffer, ReadBufferSite);
        SetLength(Buffer, ReadBinaryData('PanelBottomDockZone', Buffer[0], Length(Buffer)));
        DockData.Clear;
        DockData.Write(Buffer[0], Length(Buffer));
        DockData.Position := 0;
        PanelBottom.DockManager.LoadFromStream(DockData);
      end;
      if ValueExists('PanelCenterDockZone') then begin
        SetLength(Buffer, ReadBufferSite);
        SetLength(Buffer, ReadBinaryData('PanelCenterDockZone', Buffer[0], Length(Buffer)));
        DockData.Clear;
        DockData.Write(Buffer[0], Length(Buffer));
        DockData.Position := 0;
        PanelCenter.DockManager.LoadFromStream(DockData);
      end;
      DockData.Free;
    end;

  finally
    Free;
  end;
end;


{ TDDockForm }

constructor TDDockForm.Create(AOwner: TComponent);
begin
  inherited;
  DragKind := dkDock;
  DragMode := dmAutomatic;
end;

procedure TDBaseDockPanel.DockPanelCloseForm(Source: TForm; Panel: TPanel;
  Action: TDockPanelAction);
var
  I: Integer;
  VisibleDockClientCount: Integer;
begin
  if not Assigned(Panel) then Exit;

  VisibleDockClientCount := 0;
  for I := 0 to Panel.DockClientCount - 1 do
    if Panel.DockClients[I].Visible then Inc(VisibleDockClientCount);

  if ((Action = dpaClose) and (VisibleDockClientCount = 0)) or
  ((Action = dpaUndock) and (VisibleDockClientCount = 1)) then
  begin
    if Panel = PanelLeft then begin
      PanelLeft.Tag := PanelLeft.Width;
      PanelLeft.Width := DockPanelWidth;
      SplitterLeft.Visible := False;
    end else
    if Panel = PanelRight then begin
      PanelRight.Tag := PanelRight.Width;
      PanelRight.Width := DockPanelWidth;
      SplitterRight.Visible := False;
    end;
    if Panel = PanelTop then begin
      PanelTop.Tag := PanelTop.Height;
      PanelTop.Height := DockPanelWidth;
      SplitterTop.Visible := False;
    end;
    if Panel = PanelBottom then begin
      PanelBottom.Tag := PanelBottom.Height;
      PanelBottom.Height := DockPanelWidth;
      SplitterBottom.Visible := False;
    end;
  end else
    if Assigned(Source) then begin
    if Panel = PanelLeft then begin
      if PanelLeft.Width <= DockPanelWidth then
        PanelLeft.Width := PanelLeft.Tag;
      SplitterLeft.Visible := True;
      SplitterLeft.Left := 0;
      PanelLeft.Left := 0;
      DockTabSetLeft.Left := 0;
    end else
    if Panel = PanelRight then begin
      if PanelRight.Width <= DockPanelWidth then begin
       PanelRight.Width := PanelRight.Tag;
        PanelRight.Left := Width - PanelRight.Width;
      end;
      SplitterRight.Visible := True;
      SplitterRight.Left := Width;
      PanelRight.Left := Width;
      DockTabSetRight.Left := Width;
    end else
    if Panel = PanelTop then begin
      if PanelTop.Height <= DockPanelWidth then
      PanelTop.Height := PanelTop.Tag;
      SplitterTop.Visible := True;
      SplitterTop.Top := 0;
      PanelTop.Top := 0;
      DockTabSetTop.Top := 0;
    end;
    if Panel = PanelBottom then begin
      if PanelBottom.Height <= DockPanelWidth then
      PanelBottom.Height := PanelBottom.Tag;
      SplitterBottom.Visible := True;
      SplitterBottom.Top := Height;
      PanelBottom.Top := Height;
      DockTabSetBottom.Top := Height;
    end;
  end;
end;

procedure TDDockForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  if Parent is TDDockPanel then
    with TDDockPanel(Parent) do
     if Parent is TDBaseDockPanel then
       with TDBaseDockPanel(Parent) do
         DockPanelCloseForm(Self, TDDockPanel(Self.Parent), dpaClose);
end;

procedure TDDockForm.DoShow;
begin
  inherited;
  if Parent is TDDockPanel then
    with TDDockPanel(Parent) do
     if Parent is TDBaseDockPanel then
       with TDBaseDockPanel(Parent) do
         DockPanelCloseForm(Self, TDDockPanel(Self.Parent), dpaShow);
end;

procedure TDDockPanel.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  inherited;
  if Parent is TDBaseDockPanel then
    with TDBaseDockPanel(Parent) do
      DockPanelCloseForm(nil, Self, dpaDock);
end;

procedure TDDockPanel.DoDockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
const
  Border = 0.1;
  FrameWidth = 30;
var
  DockRectangle: TRect;
  CenterRectagle: TRect;
begin
  inherited;
  Accept := Source.Control is TDDockForm;
  if Accept then
  begin
    CenterRectagle := Rect(Round(Width * Border), Round(Height * Border),
      Width - Round(Width * Border), Height - Round(Height * Border));

    if (Height > 0) and (Width > 0) then begin
    if (X > CenterRectagle.Left) and (Y > CenterRectagle.Top) and
    (X < CenterRectagle.Right) and (Y < CenterRectagle.Bottom) then begin
      DockRectangle.TopLeft := ClientToScreen(CenterRectagle.TopLeft);
      DockRectangle.BottomRight := ClientToScreen(CenterRectagle.BottomRight);
    end else
    if (X < CenterRectagle.Left) and (Round(Y * (Width / Height)) > X) and
    (Round(Y * (Width / Height)) < (Width - X)) then begin
      DockRectangle.TopLeft := ClientToScreen(Point(0, 0));
      DockRectangle.BottomRight := ClientToScreen(Point(Width div 2, Height));
    end else
    if (Y < CenterRectagle.Top) and (Round(X * (Height / Width)) > Y) and
    (Round(X * (Height / Width)) < (Height - Y)) then begin
      DockRectangle.TopLeft := ClientToScreen(Point(0, 0));
      DockRectangle.BottomRight := ClientToScreen(Point(Width, Height div 2));
    end else
    if (X > CenterRectagle.Right) and (Round(Y * (Width / Height)) > (Width - X)) and
    (Round(Y * (Width / Height)) < X) then begin
      DockRectangle.TopLeft := ClientToScreen(Point(Width div 2, 0));
      DockRectangle.BottomRight := ClientToScreen(Point(Width, Height));
    end else
    if (Y > CenterRectagle.Bottom) and (Round(X * (Height / Width)) > (Height - Y)) and
    (Round(X * (Height / Width)) < Y) then begin
      DockRectangle.TopLeft := ClientToScreen(Point(0, Height div 2));
      DockRectangle.BottomRight := ClientToScreen(Point(Width, Height));
    end else
      Accept := False;
    end;

    case Align of
      alLeft: if Width < FrameWidth then begin
        DockRectangle.TopLeft := ClientToScreen(Point(0, 0));
        DockRectangle.BottomRight := ClientToScreen(Point(FrameWidth, Height));
      end;
      alTop: if Height < FrameWidth then begin
        DockRectangle.TopLeft := ClientToScreen(Point(0, 0));
        DockRectangle.BottomRight := ClientToScreen(Point(Width, FrameWidth));
      end;
      alRight: if Width < FrameWidth then begin
        DockRectangle.TopLeft := ClientToScreen(Point(-FrameWidth, 0));
        DockRectangle.BottomRight := ClientToScreen(Point(Width, Height));
      end;
      alBottom: if Height < FrameWidth then begin
        DockRectangle.TopLeft := ClientToScreen(Point(0, -FrameWidth));
        DockRectangle.BottomRight := ClientToScreen(Point(Width, Height));
      end;
    end;
    Source.DockRect := DockRectangle;
  end;
end;

function TDDockPanel.DoUnDock(NewTarget: TWinControl;
  Client: TControl): Boolean;
begin
  Result := inherited DoUnDock(NewTarget, Client);
  if Parent is TDBaseDockPanel then
    with TDBaseDockPanel(Parent) do
      DockPanelCloseForm(nil, Self, dpaUnDock);
end;

{ TDDockTabSet }

constructor TDDockTabSet.Create(AOwner: TComponent);
begin
  inherited;
  OnTabRemoved := TabRemoved;
end;

procedure TDDockTabSet.DockDrop(Source: TDragDockObject; X, Y: Integer);
begin
  inherited;
  Visible := True;
end;

procedure TDDockTabSet.TabRemoved(Sender: TObject);
begin
  inherited;
  Visible := Tabs.Count > 0;
end;

{ TDBaseDockPanel }

constructor TDBaseDockPanel.Create(AOwner: TComponent);
begin
  inherited;

  PanelCenter := TDDockPanel.Create(Owner);
  with PanelCenter do begin
    Align := alClient;
    AutoSize := True;
    DockSite := True;
    Parent := Self;
    Name := 'PanelCenter';
    Caption := '';
  end;

  PanelLeft := TDDockPanel.Create(Owner);
  with PanelLeft do begin
    DockSite := True;
    Align := alLeft;
    Parent := Self;
    Name := 'PanelLeft';
    Caption := '';
    Width := 0;
  end;
  DockTabSetLeft := TDDockTabSet.Create(Owner);
  with DockTabSetLeft do begin
    Align := alLeft;
    Style := tsModernTabs;
    TabPosition := tpLeft;
    DestinationDockSite := PanelLeft;
    Visible := False;
    DockSite := False;
    ShrinkToFit := True;
    Parent := Self;
    Name := 'DockTabSetLeft';
    Caption := '';
    Width := 24;
  end;
  PanelLeft.Left := DockTabSetLeft.Width;
  SplitterLeft := TSplitter.Create(Owner);
  with SplitterLeft do begin
    Align := alLeft;
    Parent := Self;
    Name := 'SplitterLeft';
    Visible := False;
  end;

  PanelRight := TDDockPanel.Create(Owner);
  with PanelRight do begin
    DockSite := True;
    Align := alRight;
    Parent := Self;
    Name := 'PanelRight';
    Caption := '';
    Width := 0;
  end;
  DockTabSetRight := TDDockTabSet.Create(Owner);
  with DockTabSetRight do begin
    Align := alRight;
    Style := tsModernTabs;
    TabPosition := tpRight;
    DestinationDockSite := PanelRight;
    Visible := False;
    DockSite := False;
    ShrinkToFit := True;
    Parent := Self;
    Name := 'DockTabSetRight';
    Caption := '';
    Width := 24;
  end;
  SplitterRight := TSplitter.Create(Owner);
  with SplitterRight do begin
    Align := alRight;
    Parent := Self;
    Visible := False;
    Name := 'SplitterRight';
  end;

  PanelTop := TDDockPanel.Create(Owner);
  with PanelTop do begin
    DockSite := True;
    Align := alTop;
    Parent := Self;
    Name := 'PanelTop';
    Caption := '';
    Height := 0;
  end;
  DockTabSetTop := TDDockTabSet.Create(Owner);
  with DockTabSetTop do begin
    Align := alTop;
    Style := tsModernTabs;
    TabPosition := tpTop;
    DestinationDockSite := PanelTop;
    Visible := False;
    DockSite := False;
    ShrinkToFit := True;
    Parent := Self;
    Name := 'DockTabSetTop';
    Caption := '';
    Height := 24;
  end;
  PanelTop.Top := DockTabSetTop.Height;
  SplitterTop := TSplitter.Create(Owner);
  with SplitterTop do begin
    Align := alTop;
    Parent := Self;
    Name := 'SplitterTop';
    Visible := False;
  end;

  PanelBottom := TDDockPanel.Create(Owner);
  with PanelBottom do begin
    DockSite := True;
    Parent := Self;
    Name := 'PanelBottom';
    Caption := '';
    Height := 0;
    Align := alBottom;
  end;
  DockTabSetBottom := TDDockTabSet.Create(Owner);
  with DockTabSetBottom do begin
    Style := tsModernTabs;
    TabPosition := tpBottom;
    DestinationDockSite := PanelBottom;
    Visible := False;
    DockSite := False;
    ShrinkToFit := True;
    Parent := Self;
    Name := 'DockTabSetBottom';
    Caption := '';
    Align := alBottom;
    Height := 24;
  end;
  SplitterBottom := TSplitter.Create(Owner);
  with SplitterBottom do begin
    Name := 'SplitterBottom';
    Parent := Self;
    Visible := False;
    Align := alBottom;
  end;
end;

end.
