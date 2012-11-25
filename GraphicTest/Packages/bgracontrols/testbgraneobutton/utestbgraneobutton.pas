{
READ BGRANeoButton.pas unit before using!
}

unit utestbgraneobutton;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, BGRANeoButton;

procedure Windows7StyleButton(AButton: TBGRANeoButton);

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRANeoButton1: TBGRANeoButton;
    BGRANeoButton2: TBGRANeoButton;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

procedure Windows7StyleButton(AButton: TBGRANeoButton);
begin

  { Common }
  with AButton.StyleNormal do
  begin
    Border.RoundX := 3;
    Border.RoundY := 3;
    Border.OuterWidth := 1;
    Border.InnerColorAlpha := 175;
    Border.InnerWidth := 1;
    Text.Font.Height := 20;
    Text.Font.Color := clBlack;
    Shadow.OffsetX := 1;
    Shadow.OffsetY := 1;
    Shadow.Radius := 1;
  end;

  AButton.StyleHover.Assign(AButton.StyleNormal);
  AButton.StyleActive.Assign(AButton.StyleNormal);
  AButton.StyleDisabled.Assign(AButton.StyleNormal);

  with AButton.StyleNormal do
  begin
    Border.OuterColor := RGBToColor(112, 112, 112);
    Gradient1.Color1 := RGBToColor(242, 242, 242);
    Gradient1.Color2 := RGBToColor(235, 235, 235);
    Gradient2.Color1 := RGBToColor(221, 221, 221);
    Gradient2.Color2 := RGBToColor(207, 207, 207);
  end;

  with AButton.StyleHover do
  begin
    Border.OuterColor := RGBToColor(60, 127, 177);
    Gradient1.Color1 := RGBToColor(234, 246, 253);
    Gradient1.Color2 := RGBToColor(217, 240, 252);
    Gradient2.Color1 := RGBToColor(190, 230, 253);
    Gradient2.Color2 := RGBToColor(167, 217, 245);
  end;

  with AButton.StyleActive do
  begin
    Border.OuterColor := RGBToColor(44, 98, 139);
    Border.InnerColorAlpha := 100;
    Gradient1.Color1 := RGBToColor(229, 244, 252);
    Gradient1.Color2 := RGBToColor(196, 229, 246);
    Gradient2.Color1 := RGBToColor(152, 209, 239);
    Gradient2.Color2 := RGBToColor(104, 179, 219);
    GradientPosition := 0.55;
  end;

  with AButton.StyleDisabled do
  begin
    Border.OuterColor := RGBToColor(173, 178, 181);
    Gradient1.Color1 := RGBToColor(244, 244, 244);
    Gradient1.Color2 := RGBToColor(244, 244, 244);
    Gradient2.Color1 := RGBToColor(244, 244, 244);
    Gradient2.Color2 := RGBToColor(244, 244, 244);
    Text.Font.Color := $006D6D6D;
  end;

end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Windows7StyleButton(BGRANeoButton1);
  Windows7StyleButton(BGRANeoButton2);
end;

end.

