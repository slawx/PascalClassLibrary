unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, UDpiControls, UDpiFormMain;

type

  { TFormMain }

  TFormMain = class(TForm)
    ButtonNewDpiForm: TButton;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure ButtonNewDpiFormClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
begin
  DpiScreen.Dpi := 96 * 2;
  TrackBar1.Position := DpiScreen.Dpi;
  ButtonNewDpiFormClick(nil);
end;

procedure TFormMain.ButtonNewDpiFormClick(Sender: TObject);
var
  DpiForm: TDpiForm;
begin
  DpiForm := TDpiFormMain.Create(nil);
  DpiForm.Caption := DpiForm.Name;
  DpiForm.SetBounds(100, 100, 400, 200);
  DpiForm.Show;
  DpiScreen.Forms.Add(DpiForm);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  DpiScreen.Dpi := TrackBar1.Position;
end;

end.

