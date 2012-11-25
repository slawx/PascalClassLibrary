unit umetroui;

{$mode objfpc}{$H+}

interface

uses
  Forms, BGRAButton, Classes;

type

  { TfrmMetroUI }

  TfrmMetroUI = class(TForm)
    btn1: TBGRAButton;
    btn10: TBGRAButton;
    btn2: TBGRAButton;
    btn3: TBGRAButton;
    btn4: TBGRAButton;
    btn5: TBGRAButton;
    btn6: TBGRAButton;
    btn7: TBGRAButton;
    btn8: TBGRAButton;
    btn9: TBGRAButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMetroUI: TfrmMetroUI;

implementation

uses
  bgrasamples;

{$R *.lfm}

{ TfrmMetroUI }

procedure TfrmMetroUI.FormCreate(Sender: TObject);
begin
  // This call AccentColorButton for each TBGRAButton in the form
  // using the TBGRAButton.Color property as parameter.
  AccentColorButtonAll(Self);
end;

procedure TfrmMetroUI.FormResize(Sender: TObject);
var
  i: integer;
begin
  // This set the Font.Height property for each TBGRAButton in the form
  // when the form is resized to be 1/3 of the button height.
  for i := 0 to Self.ControlCount - 1 do
  begin
    if Self.Controls[i] is TBGRAButton then
      with Self.Controls[i] as TBGRAButton do
      begin
        BodyClicked.Font.Height := Trunc(Self.Controls[i].Height div 3);
        BodyHover.Font.Height := Trunc(Self.Controls[i].Height div 3);
        BodyNormal.Font.Height := Trunc(Self.Controls[i].Height div 3);
      end;
  end;
end;

end.
