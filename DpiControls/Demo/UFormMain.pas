unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, UDpiForm,
  UDpiControls;

type

  { TForm1 }

  TForm1 = class(TForm)
    TrackBar1: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  DpiScreen.Dpi := 96 * 2;
  TrackBar1.Position := DpiScreen.Dpi;

  DpiForm1 := TDpiForm1.Create(nil);
  DpiForm1.Caption := DpiForm1.Name;
  DpiForm1.SetBounds(100, 100, 400, 200);
  DpiForm1.Show;

  DpiScreen.Forms.Add(DpiForm1);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  DpiScreen.Dpi := TrackBar1.Position;
end;

end.

