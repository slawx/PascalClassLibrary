unit uioselements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BGRASamples;

type

  { TfrmiOSElements }

  TfrmiOSElements = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    bitmap: TBGRABitmap;
  end; 

var
  frmiOSElements: TfrmiOSElements;

implementation

{$R *.lfm}

{ TfrmiOSElements }

procedure TfrmiOSElements.FormCreate(Sender: TObject);
begin
  bitmap := TBGRABitmap.Create(Width, Height);
end;

procedure TfrmiOSElements.FormDestroy(Sender: TObject);
begin
  bitmap.Free;
end;

procedure TfrmiOSElements.FormPaint(Sender: TObject);
begin
  bitmap.Draw(Canvas, ClientRect);
end;

procedure TfrmiOSElements.FormResize(Sender: TObject);
var
  temp: TBGRABitmap;
begin
  bitmap.SetSize(Width, Height);

  temp := DrawiOSElement(Width, Height, iOSBackground);
  bitmap.Fill(temp);
  temp.Free;

  temp := DrawiOSElement(Width, 22, iOSBar);
  bitmap.PutImage(0, 0, temp, dmSet);
  temp.Free;

  temp := DrawiOSElement(Width, 49, iOSToolBar);
  bitmap.PutImage(0, 22, temp, dmDrawWithTransparency);
  temp.Free;
end;

end.

