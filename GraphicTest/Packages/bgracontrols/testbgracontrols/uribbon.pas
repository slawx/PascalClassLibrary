unit uribbon;

{$mode objfpc}{$H+}

interface

uses
  Forms, BGRAVirtualScreen, BGRAButton, BGRABitmap;

type

  { TfrmMain }

  TfrmRibbon = class(TForm)
    btnMini1:      TBGRAButton;
    btnMini2:      TBGRAButton;
    btnMini3:      TBGRAButton;
    btnBig:        TBGRAButton;
    btnList:       TBGRAButton;
    btnSeparator2: TBGRAButton;
    btnTab:        TBGRAButton;
    btnHelp:       TBGRAButton;
    btnSeparator1: TBGRAButton;
    btnMenu:       TBGRAButton;
    vsBody:        TBGRAVirtualScreen;
    vsTab:         TBGRAVirtualScreen;
    vsForm:        TBGRAVirtualScreen;
    procedure vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsTabRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsFormRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmRibbon: TfrmRibbon;

implementation

{$R *.lfm}

uses
  bgraribbon;

{ TfrmMain }

procedure TfrmRibbon.vsBodyRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawBodyGradient(Bitmap);
end;

procedure TfrmRibbon.vsTabRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawTabGradient(Bitmap);
end;

procedure TfrmRibbon.vsFormRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawFormGradient(Bitmap);
end;

procedure TfrmRibbon.FormCreate(Sender: TObject);
begin
  RibbonMenu(btnMenu);
  RibbonTab(btnTab);
  RibbonButton(btnHelp);
  StyleRibbonBody(vsBody);

  btnList.Style := bbtDropDown;
end;

end.
