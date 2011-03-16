unit UCDManagerTabsPopup;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, ComCtrls, ExtCtrls, UCDCommon, UCDManager,
  UCDManagerTabs, Forms;

type
  { TCDAutoHide }

  TCDAutoHide = class
  private
    FDuration: Real;
    FStepCount: Integer;
    FTabPosition: TTabPosition;
    StartBounds: TRect;
    procedure SetDuration(const AValue: Real);
    procedure SetStepCount(const AValue: Integer);
    procedure UpdateBounds;
    procedure UpdateTimerInterval;
  public
    Position: Real;
    Direction: Integer;
    Enable: Boolean;
    Timer: TTimer;
    Control: TControl;
    ControlVisible: Boolean;
    procedure Hide;
    procedure Show;
    constructor Create;
    destructor Destroy; override;
    procedure TimerExecute(Sender: TObject);
    property TabPosition: TTabPosition read FTabPosition write FTabPosition;
    property Duration: Real read FDuration write SetDuration;
    property StepCount: Integer read FStepCount write SetStepCount;
  end;

  { TCDManagerTabsPopupItem }

  TCDManagerTabsPopupItem = class(TCDManagerTabsItem)
    Hidden: Boolean;
    constructor Create; override;
  end;

  { TCDManagerTabsPopup }

  TCDManagerTabsPopup = class(TCDManagerTabs)
  private
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign); override;
  public
    AutoHideEnabled: Boolean;
    AutoHide: TCDAutoHide;
    PopupForm: TForm;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    procedure PinShowButtonClick(Sender: TObject);
    procedure PinHideButtonClick(Sender: TObject);
    procedure TabControlChange(Sender: TObject); override;
    constructor Create(ADockSite: TWinControl); override;
    destructor Destroy; override;
  end;


implementation

uses
  UCDClient, UCDManagerRegions;

{ TCDManagerTabsPopupItem }

constructor TCDManagerTabsPopupItem.Create;
begin
  inherited;
end;

{ TCDAutoHide }

procedure TCDAutoHide.UpdateBounds;
begin
  case TabPosition of
    tpBottom: begin
      Control.Height := Round((StartBounds.Bottom - StartBounds.Top) * Position);
      Control.Top := StartBounds.Bottom - Control.Height;
    end;
    tpTop: begin
      Control.Height := Round((StartBounds.Bottom - StartBounds.Top) * Position);
    end;
    tpLeft: begin
      Control.Width := Round((StartBounds.Right - StartBounds.Left) * Position);
    end;
    tpRight: begin
      Control.Width := Round((StartBounds.Right - StartBounds.Left) * Position);
      Control.Left := StartBounds.Right - Control.Width;
    end;
  end;
end;

procedure TCDAutoHide.UpdateTimerInterval;
begin
  Timer.Interval := Round(FDuration * 1000 / FStepCount);
end;

procedure TCDAutoHide.SetDuration(const AValue: Real);
begin
  if FDuration = AValue then Exit;
  FDuration := AValue;
  UpdateTimerInterval;
end;

procedure TCDAutoHide.SetStepCount(const AValue: Integer);
begin
  if FStepCount = AValue then Exit;
  FStepCount := AValue;
  UpdateTimerInterval;
end;

procedure TCDAutoHide.Hide;
begin
  StartBounds := Control.BoundsRect;
  Control.Show;
  Control.BringToFront;
  Direction := -1;
  Position := 1;
  Timer.Enabled := True;
  UpdateBounds;
end;

procedure TCDAutoHide.Show;
begin
  StartBounds := Control.BoundsRect;
  //StartBounds := Bounds(0, 0, Control.UndockWidth, Control.UndockHeight);
  Control.Show;
  Control.BringToFront;
  Control.Align := alClient;
  Direction := 1;
  Position := 0;
  Timer.Enabled := True;
  UpdateBounds;
end;

constructor TCDAutoHide.Create;
begin
  inherited;
  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.OnTimer := TimerExecute;
  StepCount := 10;
  Duration := 0.05;
end;

destructor TCDAutoHide.Destroy;
begin
  Timer.Free;
  inherited Destroy;
end;

procedure TCDAutoHide.TimerExecute(Sender: TObject);
begin
  if Direction = 1 then begin
    Position := Position + 1 / StepCount;
    if Position > 1 then begin
      Position := 1;
      Timer.Enabled := False;
      ControlVisible := True;
    end;
  end else
  if Direction = -1 then begin
    Position := Position - 1 / StepCount;
    if Position < 0 then begin
      Position := 0;
      Timer.Enabled := False;
      ControlVisible := False;
    end;
  end;
  UpdateBounds;
end;

{ TCDManagerTabsPopup }

procedure TCDManagerTabsPopup.PinShowButtonClick(Sender: TObject);
begin

end;

procedure TCDManagerTabsPopup.PinHideButtonClick(Sender: TObject);
begin

end;

procedure TCDManagerTabsPopup.TabControlChange(Sender: TObject);
var
  Pos: TPoint;
  C: TControl;
begin
  inherited TabControlChange(Sender);
  MouseDownSkip := True;
  if PopupForm.ControlCount > 0 then
    PopupForm.Controls[0].Parent := nil;
  AutoHide.Hide;
  while AutoHide.Position > 0 do begin
    Application.ProcessMessages;
    Sleep(1);
  end;
  if PageControl.TabIndex >= 0 then begin
    C := TCDManagerTabsPopupItem(DockItems[PageControl.TabIndex]).Control;
    C.Parent := PopupForm;
    //AutoHide.Control.Align := alCustom;
    Pos := Point(PageControl.Left, PageControl.Top);
    Pos := DockSite.ClientToScreen(Pos);
    with AutoHide.Control do
    case AutoHide.TabPosition of
      tpTop: begin
        SetBounds(Pos.X, Pos.Y + PageControl.Height,
          PageControl.Width, C.TBDockHeight);
      end;
      tpLeft: begin
        SetBounds(Pos.X + PageControl.Width, Pos.Y,
          C.LRDockWidth, PageControl.Height);
      end;
      tpBottom: begin
        SetBounds(Pos.X, Pos.Y - C.TBDockHeight,
          PageControl.Width, C.TBDockHeight);
      end;
      tpRight: begin
        SetBounds(Pos.X - C.LRDockWidth, Pos.Y,
          C.LRDockWidth, PageControl.Height);
      end;
    end;
    //AutoHide.Control.SetBounds(0, 0, 100, 100);
    AutoHide.Show;
  end;
end;

constructor TCDManagerTabsPopup.Create(ADockSite: TWinControl);
var
  I: Integer;
begin
  inherited;
  FDockStyle := dsPopupTabs;
  PopupForm := TForm.Create(nil);
  PopupForm.DockManager := TCDManagerRegions.Create(PopupForm);
  PopupForm.Visible := True;
  //PopupForm.Parent := ADockSite;
  PopupForm.BorderStyle := bsNone;
  AutoHide := TCDAutoHide.Create;
  AutoHide.Control := PopupForm;

  for I := 0 to DockItems.Count - 1 do begin
//    if TCDManagerTabsPopupItem(DockItems[I]).Hidden then
//      if
  end;
  HeaderPos := HeaderPos; // Reset position
end;

destructor TCDManagerTabsPopup.Destroy;
begin
  AutoHide.Free;
  PopupForm.Free;
  inherited Destroy;
end;

procedure TCDManagerTabsPopup.InsertControlNoUpdate(Control: TControl; InsertAt: TAlign);
var
  NewTabSheet: TTabSheet;
  NewItem: TCDManagerTabsItem;
begin
  //inherited;
  begin
    NewItem := TCDManagerTabsPopupItem.Create;
    with NewItem do begin
      //Panel.Parent := Self.DockSite;
      Manager := Self;
      //if DockStyle = dsList then Visible := True;
      //Align := alClient;
      //Header.PopupMenu := Self.PopupMenu;
      //PopupMenu.Parent := Self.DockSite;
    end;
    if (Control is TForm) and Assigned((Control as TForm).Icon) then
      NewItem.Icon.Picture.Assign((Control as TForm).Icon);

    NewItem.Control := Control;
    Control.AddHandlerOnVisibleChanged(NewItem.VisibleChange);
    //AControl.Parent := NewItem.ClientAreaPanel;
    Control.Align := alClient;
    if (InsertAt = alTop) or (InsertAt = alLeft) then
      DockItems.Insert(0, NewItem)
      else DockItems.Add(NewItem);

  end;

    NewTabSheet := TTabSheet.Create(PageControl);
    NewTabSheet.PageControl := PageControl;
    NewTabSheet.Caption := Control.Caption;
    NewTabSheet.ImageIndex := TabImageList.Count;
    NewTabSheet.TabVisible := Control.Visible;
    Control.Parent := NewTabSheet;
    TabImageList.Add(NewItem.Icon.Picture.Bitmap, nil);
//    if Assigned(NewItem.Splitter) then
//      NewItem.Splitter.Visible := False;
//    NewItem.ClientAreaPanel.Visible := False;
//    NewItem.Visible := False;
    //NewItem.Parent := NewTabSheet;
end;

procedure TCDManagerTabsPopup.SetHeaderPos(const AValue: THeaderPos);
begin
  inherited SetHeaderPos(AValue);
  AutoHide.TabPosition := HeaderPosToTabPos(AValue);
  with PageControl do
  case AValue of
    hpTop, hpAuto: begin
      Align := alTop;
      Height := 24;
    end;
    hpBottom: begin
      Align := alBottom;
      Height := 24;
    end;
    hpLeft: begin
      Align := alLeft;
      Width := 24;
    end;
    hpRight: begin
      Align := alRight;
      Width := 24;
    end;
  end;
end;

end.

