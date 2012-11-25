unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Graphics, Dialogs, Menus, StdCtrls, BGRAButton,
  BGRABitmap, BGRABitmapTypes;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnAllOfThem: TBGRAButton;
    btnStyle: TBGRAButton;
    btnImageButton: TBGRAButton;
    btnAnimation: TBGRAButton;
    btnMathGame: TBGRAButton;
    btniOSElements: TBGRAButton;
    btnNiceButtons: TBGRAButton;
    btnRibbon: TBGRAButton;
    btnGoGlassMenu: TBGRAButton;
    btnFont: TBGRAButton;
    btnGoFlashPlayerSetup: TBGRAButton;
    btnGoWin7ToolBar: TBGRAButton;
    btnMetroUI: TBGRAButton;
    fntDialog: TFontDialog;
    ListBox1: TListBox;
    miWin7ToolBarSmooth: TMenuItem;
    miRibbonTab: TMenuItem;
    miRibbonButton: TMenuItem;
    miRibbonMenu: TMenuItem;
    miWin7Cristal: TMenuItem;
    miFlashPlayer: TMenuItem;
    miWin7ToolBar: TMenuItem;
    miWin7: TMenuItem;
    miOffice2010: TMenuItem;
    miMacOSXLion: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure ShowSampleForm(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnStyleClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Bitmap: TBGRABitmap;
    procedure DrawBackground;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  bgrasamples, uflashplayersetup, uwin7toolbar, uglassmenu,
  uribbon, umetroui, umathgame, unice, uioselements, ubgraimagebuttontest, uanim,
  uallofthem;

{ TfrmMain }

procedure TfrmMain.FormShow(Sender: TObject);
begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
end;

procedure TfrmMain.btnStyleClick(Sender: TObject);
begin
  ListBox1.Visible := not ListBox1.Visible;
end;

procedure TfrmMain.btnFontClick(Sender: TObject);
begin
  fntDialog.Font := btnFont.BodyNormal.Font;
  if fntDialog.Execute then
    SetAllBGRAButtonFont(Self, fntDialog.Font);
end;

procedure TfrmMain.ShowSampleForm(Sender: TObject);
begin
  if Sender = btnGoFlashPlayerSetup then
    frmFlashPlayerSetup.ShowModal
  else if Sender = btnGoWin7ToolBar then
    frmWin7ToolBar.ShowModal
  else if Sender = btnGoGlassMenu then
    frmGlassMenu.ShowModal
  else if Sender = btnRibbon then
    frmRibbon.ShowModal
  else if Sender = btnMetroUI then
    frmMetroUI.ShowModal
  else if Sender = btnMathGame then
    frmJuego.ShowModal
  else if Sender = btnNiceButtons then
    frmNice.ShowModal
  else if Sender = btniOSElements then
    frmiOSElements.ShowModal
  else if Sender = btnImageButton then
    frmibtest.ShowModal
  else if Sender = btnAnimation then
    frmAnimation.ShowModal
  else if Sender = btnAllOfThem then
    frmAllOfThem.ShowModal;
  //else if Sender = btnBGRAControls then
  //  ShowMessage('http://wiki.lazarus.freepascal.org/BGRAControls');
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ss: TBGRASampleStyle;
begin
  Bitmap := TBGRABitmap.Create(Width, Height);

  for ss := low(TBGRASampleStyle) to high(TBGRASampleStyle) do
    ListBox1.Items.Add(TBGRASampleStyleStr[ss]);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  Bitmap.Draw(Canvas, ClientRect, False);
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  DrawBackground;
end;

procedure TfrmMain.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  StyleButtonsSample(Self, StrToTBGRASampleStyle(ListBox1.GetSelectedText));
  DrawBackground;
end;

procedure TfrmMain.DrawBackground;
begin
  Bitmap.SetSize(Width, Height);

  case StrToTBGRASampleStyle(ListBox1.GetSelectedText) of
    ssWin7ToolBar: DrawWin7ToolBar(Bitmap, Align);
    ssWin7ToolBarSmooth: DrawWin7ToolBar(Bitmap, Align, True);
    ssFlashPlayer: DrawFlashPlayerButtonPanel(Bitmap);
    ssiOSBar: DrawiOSBar(Bitmap);
    ssiOSToolBar: DrawiOSToolBar(Bitmap, False);
    ssiOSBackground: DrawiOSBackground(Bitmap);
    ssBlack: Bitmap.Fill(BGRA(23, 23, 23, 255));
    ssSilverSquared, ssGreen, ssBlue: Bitmap.Fill(BGRA(223, 226, 230, 255));
    ssSilver: DrawSilverToolBar(Bitmap);
    else
      Bitmap.Fill(clWhite);
  end;

  Invalidate;
end;

end.

