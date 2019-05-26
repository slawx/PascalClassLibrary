unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, UDpiForm, UDpiControls;

type

  { TFormMain }

  TFormMain = class(TForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
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

  DpiForm1 := TDpiForm1.Create(nil);
  DpiForm1.Caption := DpiForm1.Name;
  DpiForm1.SetBounds(100, 100, 400, 200);
  DpiForm1.Show;

  DpiScreen.Forms.Add(DpiForm1);
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  DpiScreen.Dpi := TrackBar1.Position;
end;

end.

