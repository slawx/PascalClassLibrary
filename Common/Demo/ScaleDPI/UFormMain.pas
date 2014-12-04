unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UScaleDPI;

type

  { TForm1 }

  TForm1 = class(TForm)
    ScaleDPI1: TScaleDPI;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    Dimensions: TControlDimension;
  end;

var
  Form1: TForm1;

implementation

uses
  UFormTest;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Form2.Show;
  ScaleDPI1.StoreDimensions(Form2, Dimensions);
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  with ScaleDPI1 do begin
    DPI := Point(TrackBar1.Position, TrackBar1.Position);
    ScaleDimensions(Form2, Dimensions);
    Form2.ImageList1.Assign(Form2.ImageList2);
    ScaleImageList(Form2.ImageList1, DesignDPI);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Dimensions := TControlDimension.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Dimensions.Free;
end;

end.

