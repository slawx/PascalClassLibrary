{ See NOTICE.txt for the Android buttons. }

unit ubgraimagebuttontest;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, BGRAImageButton, BGRABitmap,
  BGRABitmapTypes, Dialogs;

type

  { TfrmMain }

  Tfrmibtest = class(TForm)
    android_14_dark: TBGRAImageButton;
    android_14_normal: TBGRAImageButton;
    android_dark: TBGRAImageButton;
    android_normal: TBGRAImageButton;
    sample_1: TBGRAImageButton;
    sample_2: TBGRAImageButton;
    sample_3: TBGRAImageButton;
    sample_4: TBGRAImageButton;
    sample_5: TBGRAImageButton;
    sample_6: TBGRAImageButton;
    sample_7: TBGRAImageButton;
    procedure FormCreate(Sender: TObject);
    procedure sample_2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmibtest: Tfrmibtest;

implementation

uses
  BGRASamples;

{$R *.lfm}

{ TfrmMain }

procedure Tfrmibtest.FormCreate(Sender: TObject);
var
  i: integer;
begin
  {$IFDEF WINDOWS}
  DoubleBuffered := True;
  {$ENDIF}

  { Load all the button images from file according to name}
    for i := 0 to Self.ControlCount - 1 do
    if Self.Controls[i] is TBGRAImageButton then
        TBGRAImageButton(Self.Controls[i]).BitmapLoadFromFile(TBGRAImageButton(Self.Controls[i]).Name + '.png');

  { BGRASamples DrawFacebookImageButton }
  DrawFacebookImageButton(sample_6);
end;

procedure Tfrmibtest.sample_2Click(Sender: TObject);
var
  i: integer;
begin
  { Switch Enable in all controls in the form }
  for i := 0 to Self.ControlCount - 1 do
    Self.Controls[i].Enabled := not Self.Controls[i].Enabled;

  { Enable this button }
  TControl(Sender).Enabled := True;
end;

end.

