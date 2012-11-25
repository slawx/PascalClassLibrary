unit unice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRAImageButton;

type

  { TfrmNice }

  TfrmNice = class(TForm)
    btn3:  TBGRAImageButton;
    btn4:  TBGRAImageButton;
    btn5:  TBGRAImageButton;
    btn6:  TBGRAImageButton;
    btn7:  TBGRAImageButton;
    btn8:  TBGRAImageButton;
    btn9:  TBGRAImageButton;
    btn10: TBGRAImageButton;
    btn1:  TBGRAImageButton;
    btn2:  TBGRAImageButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmNice: TfrmNice;

implementation

uses
  bgrasamples;

{$R *.lfm}

{ TfrmNice }

procedure TfrmNice.FormCreate(Sender: TObject);
begin
  // This call AccentColorImageButton for each TBGRAImageButton in the form
  // using the TBGRAImageButton.Color property as parameter.
  AccentColorImageButtonAll(Self);
end;

procedure TfrmNice.FormResize(Sender: TObject);
var
  i: integer;
begin
  // This set the Font.Height property for each TBGRAImageButton in the form
  // when the form is resized to be 1/3 of the button height.
  for i := 0 to Self.ControlCount - 1 do
    if Self.Controls[i] is TBGRAImageButton then
      Self.Controls[i].Font.Height := Trunc(Self.Controls[i].Height div 3);
end;

end.
