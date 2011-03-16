unit UCDManagerTabsPopup;

{$mode Delphi}{$H+}

interface

uses
  Classes, Controls, SysUtils, ComCtrls, ExtCtrls, UCDCommon,
  UCDManagerTabs;

type
  { TCDAutoHide }

  TCDAutoHide = class
  private
    FDuration: Real;
    FStepCount: Integer;
    StartBounds: TRect;
    procedure SetDuration(const AValue: Real);
    procedure SetStepCount(const AValue: Integer);
    procedure UpdateBounds;
    procedure UpdateTimerInterval;
  public
    Position: Real;
    Direction: Integer;
    TabPosition: TTabPosition;
    Enable: Boolean;
    Timer: TTimer;
    Control: TControl;
    ControlVisible: Boolean;
    procedure Hide;
    procedure Show;
    constructor Create;
    destructor Destroy; override;
    procedure TimerExecute(Sender: TObject);
    property Duration: Real read FDuration write SetDuration;
    property StepCount: Integer read FStepCount write SetStepCount;
  end;

  { TCDManagerTabsPopupItem }

  TCDManagerTabsPopupItem = class
    constructor Create;
  end;

  { TCDStylePopupTabs }

  TCDStylePopupTabs = class(TCDManagerTabs)
  public
    AutoHideEnabled: Boolean;
    AutoHide: TCDAutoHide;
    PopupPanel: TPanel;
    constructor Create(ADockSite: TWinControl);
    destructor Destroy; override;
  private
  end;


implementation

uses
  UCDClient;

{ TCDManagerTabsPopupItem }

constructor TCDManagerTabsPopupItem.Create;
begin
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
    tpRight: begin
      Control.Width := Round((StartBounds.Right - StartBounds.Left) * Position);
    end;
    tpLeft: begin
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
  Direction := -1;
  Position := 1;
  Timer.Enabled := True;
  UpdateBounds;
end;

procedure TCDAutoHide.Show;
begin
  StartBounds := Control.BoundsRect;
  Control.Align := alCustom;
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
  Duration := 0.5;
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
    if Position < 1 then begin
      Position := 0;
      Timer.Enabled := False;
      ControlVisible := False;
    end;
  end;
  UpdateBounds;
end;

{ TCDStylePopupTabs }


constructor TCDStylePopupTabs.Create(ADockSite: TWinControl);
var
  I: Integer;
begin
  inherited;
  FDockStyle := dsPopupTabs;
  AutoHide := TCDAutoHide.Create;
  PopupPanel := TPanel.Create(nil);
end;

destructor TCDStylePopupTabs.Destroy;
begin
  AutoHide.Free;
  PopupPanel.Free;
  inherited Destroy;
end;

end.

