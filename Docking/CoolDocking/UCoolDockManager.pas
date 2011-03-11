unit UCoolDockManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UCoolDockCommon, Controls, Contnrs, UCoolDockClientPanel,
  UCoolDockStyle, UCoolDockPopupMenu, LCLType, LMessages, Graphics,
  UCoolDockConjoinForm, Menus, StdCtrls, ExtCtrls, Forms,
  UCoolDockStyleRegions, UCoolDockStyleTabs, UCoolDockStylePopupRegions,
  UCoolDockStylePopupTabs;

type
  { TCoolDockPanels }

  // TCoolDockPanels = TObjectList<TCoolDockClientPanel>
  TCoolDockPanels = class(TObjectList)
    destructor Destroy; override;
  end;

  { TCoolDockManager }

  TCoolDockManager = class(TCoolDockManagerBase)
  private
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
    property DockSite: TWinControl read GetDockSite;
    property HeaderPos: THeaderPos read GetHeaderPos write SetHeaderPos;
    property Visible: Boolean write SetVisible;
  end;


implementation

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
  if (FDockSite is TForm) and (not (FDockSite is TCoolDockConjoinForm)) then begin
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
  if (FDockSite is TCoolDockConjoinForm) or (FDockSite is TPanel) or (FDockSite is TCoolDockClientPanel) then begin
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
    //if (FDockSite is TCoolDockConjoinForm) and (FDockSite.DockClientCount = 1) then
    //  FDockSite.Free;
    DockStyle := DockStyle;
  end;
  *)
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
  // FDockSite.ManualFloat(FDockSite.BoundsRect);
  //NewConjoinDockForm.ManualDock(NewDockSite, nil, InsertAt);
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

{ TCoolDockPanels }

destructor TCoolDockPanels.Destroy;
var
  Temp: Integer;
begin
  Temp := Count;
  inherited Destroy;
end;



end.

