unit UCoolDocking;

{$mode delphi}{$H+}

// Date: 2010-09-17

interface

uses
  Classes, SysUtils, Controls, LCLType, LMessages, Graphics, StdCtrls,
  Buttons, ExtCtrls, Contnrs, Forms, ComCtrls, Dialogs, Menus, FileUtil,
  UCoolDockCustomize, DOM, XMLWrite, XMLRead, UCoolDockCommon,
  DateUtils, UCoolDockStyleTabs, UCoolDockStyleRegions, UCoolDockStylePopupTabs,
  UCoolDockStylePopupRegions, UCoolDockStyle, UCoolDockClientPanel,
  UCoolDockPopupMenu;

const
  GrabberSize = 22;

type
  TCoolDockManager = class;
  TCoolDockClient = class;
  TCoolDockMaster = class;

  { TCoolDockConjoinForm }

  TCoolDockConjoinForm = class(TCoolDockConjoinFormBase)
  public
    Panel: TPanel;
    CoolDockClient: TCoolDockClient;
    procedure FormShow(Sender : TObject);
    procedure FormHide(Sender : TObject);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  private
    procedure SetName(const NewName: TComponentName); override;
    procedure PanelVisibleChange(Sender: TObject);
  end;


  { TCoolDockPanels }

  // TCoolDockPanels = TObjectList<TCoolDockClientPanel>
  TCoolDockPanels = class(TObjectList)
    destructor Destroy; override;
  end;

  { TCoolDockManager }

  TCoolDockManager = class(TCoolDockManagerBase)
  private
    FMaster: TCoolDockMaster;
    FDockStyle: TDockStyle;
    FDockSite: TWinControl;
    FDockPanels: TCoolDockPanels;
    function GetDockSite: TWinControl;
    function GetHeaderPos: THeaderPos;
    function GetMoveDuration: Integer;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
    procedure SetDockStyle(const AValue: TDockStyle);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetMaster(const AValue: TCoolDockMaster);
    procedure SetMoveDuration(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    procedure UpdateClientSize;
  public
    DockStyleHandler: TCoolDockStyle;
    PopupMenu: TCoolDockPopupMenu;
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

    function FindControlInPanels(Control: TControl): TCoolDockClientPanel;
    function CreateContainer(InsertAt: TAlign): TCoolDockConjoinForm;
    property DockPanels: TCoolDockPanels read FDockPanels write FDockPanels;
    property DockStyle: TDockStyle read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read GetMoveDuration write SetMoveDuration;
    property Master: TCoolDockMaster read FMaster write SetMaster;
    property DockSite: TWinControl read GetDockSite;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property Visible: Boolean write SetVisible;
  end;

  { TCoolDockMaster }

  TCoolDockMaster = class(TCoolDockMasterBase)
  private
    FDefaultHeaderPos: THeaderPos;
    FDefaultMoveSpeed: Integer;
    FDefaultTabsPos: THeaderPos;
    FShowIcons: Boolean;
    FTabsEnabled: Boolean;
    FClients: TObjectList;
    function GetClient(Index: Integer): TCoolDockClient;
    procedure SetShowIcons(const AValue: Boolean);
    procedure SetTabsEnabled(const AValue: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterClient(Client: TCoolDockClient);
    procedure UnRegisterClient(Client: TCoolDockClient);
    property Clients[Index: Integer]: TCoolDockClient read GetClient;
  published
    property TabsEnabled: Boolean read FTabsEnabled write SetTabsEnabled;
    property DefaultTabsPos: THeaderPos read FDefaultTabsPos
      write FDefaultTabsPos;
    property DefaultHeaderPos: THeaderPos read FDefaultHeaderPos
      write FDefaultHeaderPos;
    property DefaultMoveSpeed: Integer read FDefaultMoveSpeed
      write FDefaultMoveSpeed;
    property ShowIcons: Boolean read FShowIcons
      write SetShowIcons;
  end;

  TCoolDockClient = class(TComponent)
  private
    FDockable: Boolean;
    FFloatable: Boolean;
    FMaster: TCoolDockMaster;
    FPanel: TPanel;
    procedure SetDockable(const AValue: Boolean);
    procedure SetFloatable(const AValue: Boolean);
    procedure SetMaster(const AValue: TCoolDockMaster);
    procedure SetPanel(const AValue: TPanel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Dockable: Boolean read FDockable
      write SetDockable default True;
    property Floatable: Boolean read FFloatable
      write SetFloatable default True;
    property Master: TCoolDockMaster read FMaster
      write SetMaster;
    property Panel: TPanel read FPanel
      write SetPanel;
  end;


procedure Register;

resourcestring
  SWrongOwner = 'Owner of TCoolDockClient have to be TForm';

implementation

procedure Register;
begin
  RegisterComponents('CoolDocking', [TCoolDockMaster]);
  RegisterComponents('CoolDocking', [TCoolDockClient]);
  RegisterComponents('CoolDocking', [TCoolDockCustomize]);
end;

function GetUniqueName(BaseName: string): string;
var
  I: Integer;
begin
  I := 1;
  while Assigned(FindGlobalComponent(BaseName + IntToStr(I))) do Inc(I);
  Result := BaseName + IntToStr(I);
end;

{ TCoolDockPanels }

destructor TCoolDockPanels.Destroy;
var
  Temp: Integer;
begin
  Temp := Count;
  inherited Destroy;
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

function TCoolDockManager.GetDockSite: TWinControl;
begin
  Result := FDockSite;
end;

function TCoolDockManager.GetHeaderPos: THeaderPos;
begin

end;

function TCoolDockManager.GetMoveDuration: Integer;
begin

end;

constructor TCoolDockManager.Create(ADockSite: TWinControl);
var
  NewMenuItem: TMenuItem;
  NewMenuItem2: TMenuItem;
begin
  inherited Create(ADockSite);
  FDockSite := ADockSite;
  FDockPanels := TCoolDockPanels.Create;

  FDockStyle := dsTabs; // To initialize style value have to be different
  DockStyle := dsList;
  PopupMenu := TCoolDockPopupMenu.Create(Self);
  PopupMenu.Parent := ADockSite;
end;

destructor TCoolDockManager.Destroy;
begin
  PopupMenu.Free;
  DockStyleHandler.Free;
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
begin
  DockStyleHandler.InsertControl(Control, InsertAt);
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
      NewConjoinDockForm := CreateContainer(InsertAt);
      FDockSite.ManualDock(NewConjoinDockForm.Panel);
      Control.ManualDock(NewConjoinDockForm.Panel, nil, InsertAt);
    end else begin
      NewConjoinDockForm := CreateContainer(InsertAt);
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
  //inherited;
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
  //inherited;
  if Control.HostDockSite = Self.FDockSite then begin
    ClientPanel := FindControlInPanels(Control);

    //if Assigned(ClientPanel) then ClientPanel.Splitter.Free;
    //Control.RemoveHandlerOnVisibleChanged(ClientPanel.VisibleChange);
    FDockPanels.Remove(ClientPanel);
    DockStyleHandler.RemoveControl(Control);
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
  NewConjoinDockForm.Name := GetUniqueName('ConjoinForm');
  NewConjoinDockForm.Visible := True;
  NewConjoinDockForm.BoundsRect := FDockSite.BoundsRect;
  NewConjoinDockForm.CoolDockClient.Master := Self.Master;
  NewDockSite := FDockSite.HostDockSite;
  //      FDockSite.ManualFloat(FDockSite.BoundsRect);
  NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
  Result := NewConjoinDockForm;
end;

procedure TCoolDockManager.SetDockStyle(const AValue: TDockStyle);
var
  I: Integer;
begin
  if FDockStyle <> AValue then begin
    FDockStyle := AValue;
    FreeAndNil(DockStyleHandler);
    if AValue = dsTabs then begin
      DockStyleHandler := TCoolDockStyleTabs.Create(Self);
      TCoolDockStyleTabs(DockStyleHandler).TabControlChange(Self);
    end else
    if AValue = dsList then begin
      DockStyleHandler := TCoolDockStyleRegions.Create(Self);
    end else
    if AValue = dsPopupList then begin
      DockStyleHandler := TCoolDockStylePopupRegions.Create(Self);
    end else
    if AValue = dsPopupTabs then begin
      DockStyleHandler := TCoolDockStylePopupTabs.Create(Self);
    end;
  end;
  UpdateClientSize;
end;

procedure TCoolDockManager.SetHeaderPos(const AValue: THeaderPos);
begin

end;

procedure TCoolDockManager.SetMaster(const AValue: TCoolDockMaster);
begin
  if FMaster = AValue then Exit;
  FMaster := AValue;
end;

procedure TCoolDockManager.SetMoveDuration(const AValue: Integer);
begin
end;

procedure TCoolDockManager.SetVisible(const AValue: Boolean);
var
  I: Integer;
begin
  DockStyleHandler.Visible := AValue;
//  for I := 0 to DockPanels.Count - 1 do
//    TCoolDockClientPanel(DockPanels[I]).Visible := AValue;
end;

procedure TCoolDockManager.UpdateClientSize;
begin
  DockStyleHandler.UpdateClientSize;
end;

{ TCoolDockConjoinForm }

procedure TCoolDockConjoinForm.FormShow(Sender: TObject);
begin
  Panel.Show;
  TCoolDockManager(Panel.DockManager).Visible := True;
end;

procedure TCoolDockConjoinForm.FormHide(Sender: TObject);
var
  I: Integer;
begin
  Panel.Hide;
  TCoolDockManager(Panel.DockManager).Visible := False;
  // Hide all docked childs
  with TCoolDockManager(Panel.DockManager) do
  for I := 0 to DockPanels.Count - 1 do
    if Assigned(TCoolDockClientPanel(DockPanels[I]).Control) then begin
      TCoolDockClientPanel(DockPanels[I]).Control.Tag := Integer(dhtTemporal);
      TCoolDockClientPanel(DockPanels[I]).Control.Hide;
    end;
end;

constructor TCoolDockConjoinForm.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);
  Panel := TPanel.Create(Self);
  with Panel do begin
    Parent := Self;
    Caption := '';
    DockSite := True;
    UseDockManager := True;
    Align := alClient;
    BevelOuter := bvNone;
    BevelInner := bvNone;
  //  Color := clYellow;
  end;
  CoolDockClient := TCoolDockClient.Create(Self);
  with CoolDockClient do begin
    Panel := Self.Panel;
  end;
  OnShow := FormShow;
  OnHide := FormHide;

  Panel.AddHandlerOnVisibleChanged(PanelVisibleChange);
end;

destructor TCoolDockConjoinForm.Destroy;
begin
  Panel.RemoveHandlerOnVisibleChanged(PanelVisibleChange);
  inherited;
end;

procedure TCoolDockConjoinForm.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  Panel.Name := Name + '_Panel';
  Panel.Caption := '';
  CoolDockClient.Name := Name + '_CoolDockClient';
end;

procedure TCoolDockConjoinForm.PanelVisibleChange(Sender: TObject);
begin
  Visible := Panel.Visible;
end;

{ TCoolDockMaster }

procedure TCoolDockMaster.SetTabsEnabled(const AValue: Boolean);
begin
  if FTabsEnabled = AValue then Exit;
  FTabsEnabled := AValue;
end;

function TCoolDockMaster.GetClient(Index: Integer): TCoolDockClient;
begin
  Result := TCoolDockClient(FClients[Index]);
end;

procedure TCoolDockMaster.SetShowIcons(const AValue: Boolean);
begin
  if FShowIcons = AValue then Exit;
  FShowIcons := AValue;
end;

constructor TCoolDockMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClients := TObjectList.Create;
  FClients.OwnsObjects := False;
end;

destructor TCoolDockMaster.Destroy;
var
  I: Integer;
begin
  // Assigning nil to Client Master property cause unregistring client from list
  for I := FClients.Count - 1 downto 0 do
    TCoolDockClient(FClients[I]).Master := nil;
  FClients.Free;
  Customize := nil;
  inherited Destroy;
end;

procedure TCoolDockMaster.RegisterClient(Client: TCoolDockClient);
begin
  if Assigned(Client) then
    if FClients.IndexOf(Client) = -1 then begin
      FClients.Add(Client);
      Client.Master := Self;
    end;
end;

procedure TCoolDockMaster.UnRegisterClient(Client: TCoolDockClient);
begin
  if Assigned(Client) then begin
    Client.Master := nil;
    FClients.Remove(Client);
  end;
end;

{ TCoolDockClient }

procedure TCoolDockClient.SetMaster(const AValue: TCoolDockMaster);
var
  FOldMaster: TCoolDockMaster;
begin
  if FMaster = AValue then Exit;
  FOldMaster := FMaster;
  FMaster := AValue;
  if Assigned(FOldMaster) then
    FOldMaster.UnregisterClient(Self);
  if Assigned(FMaster) then begin
    FMaster.RegisterClient(Self);
    if not (csDesigning in ComponentState) then begin
      if Assigned(TWinControl(Owner).DockManager) then
        TCoolDockManager(TWinControl(Owner).DockManager).Master := FMaster;
      if Assigned(Panel) then
        TCoolDockManager(Panel.DockManager).Master := FMaster;
    end;
  end;
end;

procedure TCoolDockClient.SetDockable(const AValue: Boolean);
begin
  if FDockable = AValue then Exit;
  FDockable := AValue;
  if (Owner is TForm) then
  with (Owner as TForm) do
  if AValue then begin
    DragKind := dkDock;
    DragMode := dmAutomatic;
    DockSite := True;
  end else begin
    DragKind := dkDrag;
    DragMode := dmManual;
    DockSite := False;
  end;
end;

procedure TCoolDockClient.SetFloatable(const AValue: Boolean);
begin
  if FFloatable = AValue then Exit;
  FFloatable := AValue;
end;

constructor TCoolDockClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDockable := True;
  if not (AOwner is TForm) then
    raise Exception.Create(SWrongOwner);
  with (AOwner as TForm) do begin
    if not (csDesigning in ComponentState) then begin
      if Dockable then begin
        DragKind := dkDock;
        DragMode := dmAutomatic;
        DockSite := True;
      end;
      UseDockManager := True;
      DockManager := TCoolDockManager.Create(TWinControl(AOwner));
    end;
  end;
end;

destructor TCoolDockClient.Destroy;
begin
  inherited Destroy;
  Master := nil;
end;

procedure TCoolDockClient.SetPanel(const AValue: TPanel);
var
  OldPanel: TPanel;
begin
  if FPanel = AValue then exit;
  OldPanel := FPanel;
  FPanel := AValue;
  if not (csDesigning in ComponentState) then begin
    if Assigned(FPanel) then
    with FPanel do begin
      DockSite := True;
      UseDockManager := True;
      DockManager := TCoolDockManager.Create(FPanel);
    end else begin
      OldPanel.DockSite := False;
    end;
  end;
end;

initialization

RegisterClass(TCoolDockConjoinForm);

finalization

UnRegisterClass(TCoolDockConjoinForm);


end.

