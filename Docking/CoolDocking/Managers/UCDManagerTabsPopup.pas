unit UCDManagerTabsPopup;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, ComCtrls, ExtCtrls, UCDCommon, UCDManager,
  UCDManagerTabs, Forms, URectangle, UCDConjoinForm;

type
  { TCDAutoHide }

  TCDAutoHide = class
  private
    FDuration: Real;
    FStepCount: Integer;
    FTabPosition: TTabPosition;
    ControlBounds: TRectangle;
    HideBounds: TRectangle;
    ShowBounds: TRectangle;
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
    DoShow: Boolean;
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
    SplitterMouseDrag: Boolean;
    SplitterMousePos: TPoint;
    procedure PageControlResize(Sender: TObject);
    procedure InsertControlNoUpdate(Control: TControl; InsertAt: TAlign); override;
    procedure SplitterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SplitterMouseMove(Sender: TObject; Shift: TShiftState;
                              X, Y: Integer);
    procedure SplitterMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpdatePopupFormBounds;
    procedure TabControlChange(Sender: TObject); override;
  public
    AutoHideEnabled: Boolean;
    AutoHide: TCDAutoHide;
    PopupForm: TForm;
    HeaderPanel: TCDPanelHeader;
    Splitter: TPanel;
    procedure SetHeaderPos(const AValue: THeaderPos); override;
    procedure PinShowButtonClick(Sender: TObject);
    procedure PinHideButtonClick(Sender: TObject);
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
    tpTop: begin
      Control.SetBounds(ControlBounds.Left, ControlBounds.Top,
        ControlBounds.Width, Round(ControlBounds.Height * Position));
    end;
    tpLeft: begin
      Control.SetBounds(ControlBounds.Left, ControlBounds.Top,
        Round(ControlBounds.Width * Position), ControlBounds.Height);
    end;
    tpRight: begin
      Control.SetBounds(ControlBounds.Right -
        Round(ControlBounds.Width * Position), ControlBounds.Top,
        Round(ControlBounds.Width * Position), ControlBounds.Height);
    end;
    tpBottom: begin
      Control.SetBounds(ControlBounds.Left,
        ControlBounds.Bottom - Round(ControlBounds.Height * Position),
        ControlBounds.Width, Round(ControlBounds.Height * Position));
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
  HideBounds.AsTRect := Control.BoundsRect;
  ControlBounds.Assign(HideBounds);
  Control.Show;
  Control.BringToFront;
  Application.ProcessMessages;
  Direction := -1;
  Position := 1;
  Timer.Enabled := True;
  UpdateBounds;
end;

procedure TCDAutoHide.Show;
begin
  ShowBounds.AsTRect := Control.BoundsRect;
  ControlBounds.Assign(HideBounds);
  if Position > 0 then begin
    DoShow := True;
    Hide;
  end else begin
    //StartBounds := Bounds(0, 0, Control.UndockWidth, Control.UndockHeight);
    Control.Show;
    Control.BringToFront;
    //Control.Align := alClient;
    Direction := 1;
    Position := 0;
    Timer.Enabled := True;
    UpdateBounds;
  end;
end;

constructor TCDAutoHide.Create;
begin
  inherited;
  ShowBounds := TRectangle.Create;
  HideBounds := TRectangle.Create;
  ControlBounds := TRectangle.Create;
  Timer := TTimer.Create(nil);
  Timer.Enabled := False;
  Timer.OnTimer := TimerExecute;
  StepCount := 10;
  Duration := 0.05;
  ShowBounds := TRectangle.Create;
end;

destructor TCDAutoHide.Destroy;
begin
  ShowBounds.Free;
  HideBounds.Free;
  ControlBounds.Free;
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
      DoShow := False;
      HideBounds.Assign(ShowBounds);
    end;
  end else
  if Direction = -1 then begin
    Position := Position - 1 / StepCount;
    if Position < 0 then begin
      Position := 0;
      if DoShow then begin
        Direction := 1;
        ControlBounds.Assign(ShowBounds);
      end else begin
        Timer.Enabled := False;
        ControlVisible := False;
      end;
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
  TopParent: TWinControl;
begin
  inherited TabControlChange(Sender);
  MouseDownSkip := True;

  if PageControl.TabIndex >= 0 then begin
    C := TCDManagerTabsPopupItem(DockItems[PageControl.TabIndex]).Control;
    C.Align := alClient;
    C.Parent := HeaderPanel.ControlPanel;
    HeaderPanel.Header.Title.Caption := C.Caption;
    //AutoHide.Control.Align := alCustom;
    //Pos := DockSite.ClientToScreen(Pos);
    //AutoHide.Control.SetBounds(0, 0, 100, 100);
    UpdatePopupFormBounds;
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
  PopupForm.BorderStyle := bsNone;
  HeaderPanel := TCDPanelHeader.Create(nil);
  HeaderPanel.Parent := PopupForm;
  HeaderPanel.Align := alClient;
  HeaderPanel.Visible := True;
  Splitter := TPanel.Create(nil);
  Splitter.Visible := True;
  Splitter.Parent := PopupForm;
  Splitter.OnMouseDown := SplitterMouseDown;
  Splitter.OnMouseMove := SplitterMouseMove;
  Splitter.OnMouseUp := SplitterMouseUp;
  AutoHide := TCDAutoHide.Create;
  AutoHide.Control := PopupForm;
  PageControl.OnResize := PageControlResize;

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
  HeaderPanel.Free;
  inherited Destroy;
end;

procedure TCDManagerTabsPopup.PageControlResize(Sender: TObject);
begin
  UpdatePopupFormBounds;
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

procedure TCDManagerTabsPopup.SplitterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    SplitterMousePos := Point(X, Y);
    SplitterMouseDrag := True;
  end;
end;

procedure TCDManagerTabsPopup.SplitterMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if SplitterMouseDrag then begin
    case Splitter.Align of
      alLeft: begin
        PopupForm.SetBounds(PopupForm.Left + (X - SplitterMousePos.X),
          PopupForm.Top, PopupForm.Width - (X - SplitterMousePos.X),
          PopupForm.Height);
      end;
      alRight: begin
        PopupForm.SetBounds(PopupForm.Left, PopupForm.Top,
          PopupForm.Width + (X - SplitterMousePos.X), PopupForm.Height);
      end;
      alTop: begin
        PopupForm.SetBounds(PopupForm.Left, PopupForm.Top + (Y - SplitterMousePos.Y),
          PopupForm.Width, PopupForm.Height - (Y - SplitterMousePos.Y));
      end;
      alBottom: begin
        PopupForm.SetBounds(PopupForm.Left, PopupForm.Top,
          PopupForm.Width, PopupForm.Height + (Y - SplitterMousePos.Y));
      end;
    end;
  end;
end;

procedure TCDManagerTabsPopup.SplitterMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SplitterMouseDrag := False;
end;

procedure TCDManagerTabsPopup.UpdatePopupFormBounds;
var
  Pos: TPoint;
  C: TControl;
  TopParent: TWinControl;
begin
  if PageControl.TabIndex <> - 1 then begin
    Pos := Point(PageControl.Left, PageControl.Top);
    TopParent := DockSite;
    while Assigned(TopParent.Parent) do begin
      Pos.X := Pos.X + TopParent.Left;;
      Pos.Y := Pos.Y + TopParent.Top;
      TopParent := TopParent.Parent;
    end;
    PopupForm.Parent := TopParent;

    C := TCDManagerTabsPopupItem(DockItems[PageControl.TabIndex]).Control;
    C.TBDockHeight := 100;
    C.LRDockWidth := 100;
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
  end;
end;

procedure TCDManagerTabsPopup.SetHeaderPos(const AValue: THeaderPos);
const
  SplitterSize: Integer = 4;
begin
  inherited SetHeaderPos(AValue);
  AutoHide.TabPosition := HeaderPosToTabPos(AValue);
  with PageControl do
  case AValue of
    hpTop, hpAuto: begin
      //Align := alTop;
      //Height := 24;
      Splitter.Align := alBottom;
      Splitter.Height := SplitterSize;
      Splitter.Cursor := crSizeNS;
    end;
    hpBottom: begin
      //Align := alBottom;
      //Height := 24;
      Splitter.Align := alTop;
      Splitter.Height := SplitterSize;
      Splitter.Cursor := crSizeNS;
    end;
    hpLeft: begin
      //Align := alLeft;
      //Width := 24;
      Splitter.Align := alRight;
      Splitter.Width := SplitterSize;
      Splitter.Cursor := crSizeWE;
    end;
    hpRight: begin
      //Align := alRight;
      //Width := 24;
      Splitter.Align := alLeft;
      Splitter.Width := SplitterSize;
      Splitter.Cursor := crSizeWE;
    end;
  end;
end;

end.

