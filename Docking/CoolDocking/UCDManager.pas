unit UCDManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCDCommon, Controls, Contnrs, UCDClientPanel,
  UCDStyle, UCDPopupMenu, LCLType, LMessages, Graphics,
  UCDConjoinForm, Menus, StdCtrls, ExtCtrls, Forms,
  UCDStyleRegions, UCDStyleTabs, UCDStylePopupRegions,
  UCDStylePopupTabs;

type
  { TCoolDockPanels }

  // TCoolDockPanels = TObjectList<TCDClientPanel>
  TCDPanels = class(TObjectList)
    destructor Destroy; override;
  end;

  { TCDManager }

  TCDManager = class(TCDManagerBase)
  private
    FDockStyle: TCDStyleType;
    FDockSite: TWinControl;
    FDockPanels: TCDPanels;
    function GetDockSite: TWinControl;
    function GetHeaderPos: THeaderPos;
    function GetMoveDuration: Integer;
    procedure InsertControlPanel(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl);
    procedure SetDockStyle(const AValue: TCDStyleType);
    procedure SetHeaderPos(const AValue: THeaderPos);
    procedure SetMoveDuration(const AValue: Integer);
    procedure SetVisible(const AValue: Boolean);
    procedure UpdateClientSize;
  public
    DockStyleHandler: TCDStyle;
    PopupMenu: TCDPopupMenu;
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

    function FindControlInPanels(Control: TControl): TCDClientPanel;
    function CreateContainer(InsertAt: TAlign): TCDConjoinForm;
    property DockPanels: TCDPanels read FDockPanels write FDockPanels;
    property DockStyle: TCDStyleType read FDockStyle write SetDockStyle;
    property MoveDuration: Integer read GetMoveDuration write SetMoveDuration;
    property DockSite: TWinControl read GetDockSite;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property Visible: Boolean write SetVisible;
  end;


implementation

{ TCDManager }

function TCDManager.FindControlInPanels(Control: TControl
  ): TCDClientPanel;
var
  I: Integer;
begin
  I := 0;
  while (I < FDockPanels.Count) and
    (TCDClientPanel(FDockPanels[I]).Control <> Control) do Inc(I);
  if I < FDockPanels.Count then Result := TCDClientPanel(FDockPanels[I])
    else Result := nil;
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
  FDockPanels := TCDPanels.Create;

  FDockStyle := dsTabs; // To initialize style value have to be different
  DockStyle := dsList;
  PopupMenu := TCDPopupMenu.Create(Self);
  PopupMenu.Parent := ADockSite;
end;

destructor TCDManager.Destroy;
begin
  PopupMenu.Free;
  DockStyleHandler.Free;
  FDockPanels.Free;
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
  DockStyleHandler.InsertControl(Control, InsertAt);
end;

procedure TCDManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
var
  NewSplitter: TSplitter;
  NewDockPanel: TCDClientPanel;
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
  if (FDockSite is TCDConjoinForm) or (FDockSite is TPanel) or (FDockSite is TCDClientPanel) then begin
    InsertControlPanel(Control, InsertAt, DropCtl);
  end;

//  FDockPanel.Invalidate;
  //inherited;
end;

procedure TCDManager.LoadFromStream(Stream: TStream);
begin
end;

procedure TCDManager.PaintSite(DC: HDC);
var
  Canvas: TControlCanvas;
  Control: TControl;
  I: Integer;
  R: TRect;
begin
  for I := 0 to FDockPanels.Count - 1 do
    with TCDClientPanel(FDockPanels[I]) do begin
      Invalidate;
    end;
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
var
  ClientPanel: TCDClientPanel;
begin
  DockStyleHandler.RemoveControl(Control);
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
begin
  if FDockStyle <> AValue then begin
    FDockStyle := AValue;
    FreeAndNil(DockStyleHandler);
    if AValue = dsTabs then begin
      DockStyleHandler := TCDStyleTabs.Create(Self);
      TCDStyleTabs(DockStyleHandler).TabControlChange(Self);
    end else
    if AValue = dsList then begin
      DockStyleHandler := TCDStyleRegions.Create(Self);
    end else
    if AValue = dsPopupList then begin
      DockStyleHandler := TCDStylePopupRegions.Create(Self);
    end else
    if AValue = dsPopupTabs then begin
      DockStyleHandler := TCDStylePopupTabs.Create(Self);
    end;
  end;
  UpdateClientSize;
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
  DockStyleHandler.Visible := AValue;
//  for I := 0 to DockPanels.Count - 1 do
//    TCDClientPanel(DockPanels[I]).Visible := AValue;
end;

procedure TCDManager.UpdateClientSize;
begin
  DockStyleHandler.UpdateClientSize;
end;

{ TCDPanels }

destructor TCDPanels.Destroy;
var
  Temp: Integer;
begin
  Temp := Count;
  inherited Destroy;
end;



end.

