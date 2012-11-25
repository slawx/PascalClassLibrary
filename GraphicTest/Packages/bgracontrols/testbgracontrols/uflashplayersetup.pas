unit uflashplayersetup;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ExtCtrls, BGRABitmap, BGRAVirtualScreen, BGRAButton,
  BGRAFlashProgressBar, Classes, SysUtils;

type

  { TfrmFlashPlayerSetup }

  TfrmFlashPlayerSetup = class(TForm)
    btnQuit:       TBGRAButton;
    btnInstall:    TBGRAButton;
    FlashProgressbar: TBGRAFlashProgressBar;
    LabelPercent: TLabel;
    LabelPercentShadow: TLabel;
    LabelPos: TLabel;
    LabelPosShadow: TLabel;
    Timer1: TTimer;
    vsBody:        TBGRAVirtualScreen;
    vsButtonPanel: TBGRAVirtualScreen;
    procedure Quit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsButtonPanelRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
    procedure SetStatus(AText: string);
  end;

var
  frmFlashPlayerSetup: TfrmFlashPlayerSetup;

implementation

{$R *.lfm}

uses
  bgrasamples;

{ TfrmFlashPlayerSetup }

procedure TfrmFlashPlayerSetup.Quit(Sender: TObject);
begin
  Close;
end;

procedure TfrmFlashPlayerSetup.FormCreate(Sender: TObject);
begin
  StyleButtonsSample(vsButtonPanel, ssFlashPlayer);
end;

procedure TfrmFlashPlayerSetup.vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawFlashPlayerBody(Bitmap);
end;

procedure TfrmFlashPlayerSetup.vsButtonPanelRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  DrawFlashPlayerButtonPanel(Bitmap);
end;

procedure TfrmFlashPlayerSetup.Button1Click(Sender: TObject);
begin
  Timer1.Enabled  := False;
  FlashProgressbar.Value := 0;
  Timer1.Enabled  := True;
  btnInstall.Enabled := False;
  SetStatus('Initializing...');
end;

procedure TfrmFlashPlayerSetup.Timer1Timer(Sender: TObject);
begin
  if FlashProgressbar.Value < FlashProgressbar.MaxValue then
  begin
    FlashProgressbar.Value := FlashProgressbar.Value + 128;
    SetStatus(IntToStr(FlashProgressbar.Value) + 'k/' + IntToStr(
      FlashProgressbar.MaxValue) + 'k');
  end
  else
  begin
    Timer1.Enabled  := False;
    btnInstall.Enabled := True;
    SetStatus('Done');
  end;
end;

procedure TfrmFlashPlayerSetup.SetStatus(AText: string);
var
  percent: string;
begin
  LabelPos.Caption := AText;
  LabelPosShadow.Caption := AText;
  percent := IntToStr(round(FlashProgressbar.Value / FlashProgressbar.MaxValue *
    100)) + '%';
  LabelPercent.Caption := percent;
  LabelPercentShadow.Caption := percent;
end;

end.
