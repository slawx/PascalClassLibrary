unit uglassmenu;

{$mode objfpc}{$H+}

interface

uses
  Forms, Dialogs, BGRAVirtualScreen, BGRAButton, BGRAKnob, BGRABitmap;

type

  { TfrmGlassMenu }

  TfrmGlassMenu = class(TForm)
    btnShowBkg:        TBGRAButton;
    knbButtonOpacity:  TBGRAKnob;
    btnTextOpacity:    TBGRAButton;
    btnBackground:     TBGRAButton;
    dlgOpenBackground: TOpenDialog;
    vsBackground:      TBGRAVirtualScreen;
    procedure btnShowBkgClick(Sender: TObject);
    procedure knbButtonOpacityValueChanged(Sender: TObject; Value: single);
    procedure btnBackgroundClick(Sender: TObject);
    procedure btnTextOpacityClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure vsBackgroundRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
    theBackground: TBGRABitmap;
    theBlur:       integer;
    theNiceOption: boolean;
  public
    { public declarations }
  end;

var
  frmGlassMenu: TfrmGlassMenu;

implementation

{$R *.lfm}

uses
  bgrasamples;

{ TfrmGlassMenu }

procedure TfrmGlassMenu.btnBackgroundClick(Sender: TObject);
begin
  if dlgOpenBackground.Execute then
  begin
    theBackground.LoadFromFile(dlgOpenBackground.FileName);
    vsBackground.RedrawBitmap;
  end;
end;

procedure TfrmGlassMenu.knbButtonOpacityValueChanged(Sender: TObject; Value: single);
begin
  btnBackground.GlobalOpacity := trunc(Value - knbButtonOpacity.MinValue);
end;

procedure TfrmGlassMenu.btnShowBkgClick(Sender: TObject);
begin
  theNiceOption := not theNiceOption;
  vsBackground.RedrawBitmap;
end;

procedure TfrmGlassMenu.btnTextOpacityClick(Sender: TObject);
begin
  btnBackground.TextApplyGlobalOpacity := not btnBackground.TextApplyGlobalOpacity;
end;

procedure TfrmGlassMenu.FormCreate(Sender: TObject);
begin
  theBlur := 10;
  theBackground := TBGRABitmap.Create(Self.Width, Self.Height);
  theNiceOption := True;

  StyleButtonsSample(Self, ssMacOSXLion);

  with btnBackground do
  begin
    GlobalOpacity := 100;
    with BodyNormal do
    begin
      BorderColorOpacity := 200;
      Gradient2.StartColorOpacity := 50;
    end;
    with BodyHover do
    begin
      BorderColorOpacity := 250;
      Gradient1.StartColorOpacity := 100;
      Gradient2.EndColorOpacity := 100;
    end;
    with BodyClicked do
    begin
      BorderColorOpacity := 100;
      Gradient1.EndColorOpacity := 100;
      Gradient2.StartColorOpacity := 100;
    end;
  end;
end;

procedure TfrmGlassMenu.FormDestroy(Sender: TObject);
begin
  theBackground.Free;
end;

procedure TfrmGlassMenu.vsBackgroundRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Glass(vsBackground, theBackground, theBlur, theNiceOption);
end;

end.
