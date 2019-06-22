unit UDpiFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UDpiControls, Dialogs, Graphics;

type

  { TDpiFormMain }

  TDpiFormMain = class(TDpiForm)
    DpiPaintBox1: TDpiPaintBox;
    procedure DpiButton1Click(Sender: TObject);
    procedure DpiFormMainCreate(Sender: TObject);
    procedure DpiFormMainDestroy(Sender: TObject);
    procedure DpiPaintBox1Paint(Sender: TObject);
  private
    Button: TDpiButton;
    Image: TDpiImage;
    ListBox: TDpiListBox;
    PaintBox: TDpiPaintBox;
  public

  end;

var
  DpiFormMain: TDpiFormMain;

implementation

{$R *.lfm}

{ TDpiFormMain }

procedure TDpiFormMain.DpiFormMainCreate(Sender: TObject);
var
  I: Integer;
begin
  Button := TDpiButton.Create(DpiFormMain);
  Button.Parent := Self;
  Button.SetBounds(10, 10, 100, 30);
  Button.Caption := 'Click me';
  Button.Visible := True;

  Image := TDpiImage.Create(DpiFormMain);
  Image.Parent := Self;
  Image.SetBounds(150, 10, 100, 100);
  Image.Visible := True;
  Image.Stretch := True;
  Image.VclImage.Picture.LoadFromFile('dance.jpg');
  //Image.Picture.LoadFromFile('dance.jpg');

  ListBox := TDpiListBox.Create(DpiFormMain);
  for I := 0 to 10 do
    ListBox.VclListBox.Items.Add('Item ' + IntToStr(I));
  ListBox.Parent := Self;
  ListBox.SetBounds(250, 10, 100, 100);
  ListBox.Visible := True;

  DpiPaintBox1.BoundsRect := Rect(0, 0, 100, 100);
  DpiPaintBox1.VclPaintBox.Parent := VclForm;
  DpiPaintBox1.Repaint;
end;

procedure TDpiFormMain.DpiFormMainDestroy(Sender: TObject);
begin
  FreeAndNil(Button);
  FreeAndNil(Image);
  FreeAndNil(ListBox);
end;

procedure TDpiFormMain.DpiPaintBox1Paint(Sender: TObject);
begin
  with DpiPaintBox1.Canvas do begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    FillRect(Rect(0, 0, 100, 100));
    Caption := IntToStr(Width);
    Pen.Color := clGreen;
    Pen.Style := psSolid;
    MoveTo(0, 0);
    LineTo(100, 100);
    Font.Color := clRed;
    TextOut(40, 10, 'Scaled text');
  end;
end;

procedure TDpiFormMain.DpiButton1Click(Sender: TObject);
begin
  ShowMessage('Hello');
end;

end.

