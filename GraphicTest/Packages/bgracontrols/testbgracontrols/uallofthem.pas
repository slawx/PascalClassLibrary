unit uallofthem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, BGRAButton,
  BGRAFlashProgressBar, BGRAGraphicControl, BGRAImageButton, BGRAKnob,
  BGRALabel, BGRALabelFX, BCPanel, BGRAShape, BGRASpeedButton,
  BGRAVirtualScreen, BGRAWin7ToolBar, BGRABitmap,
  BGRABitmapThemeUtils, BGRAImageList, BGRABitmapTypes;

type

  { TfrmAllOfThem }

  TfrmAllOfThem = class(TForm)
    BGRAButton1: TBGRAButton;
    BGRAFlashProgressBar1: TBGRAFlashProgressBar;
    BGRAGraphicControl1: TBGRAGraphicControl;
    BGRAImageButton1: TBGRAImageButton;
    BGRAImageList1: TBGRAImageList;
    BGRAKnob1: TBGRAKnob;
    BGRALabel1: TBGRALabel;
    BGRALabelFX1: TBGRALabelFX;
    BCPanel1: TBCPanel;
    BGRAShape1: TBGRAShape;
    BGRASpeedButton1: TBGRASpeedButton;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    BGRAWin7ToolBar1: TBGRAWin7ToolBar;
    procedure BGRAGraphicControl1Click(Sender: TObject);
    procedure BGRAGraphicControl1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAKnob1ValueChanged(Sender: TObject; Value: single);
    procedure BGRAVirtualScreen1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BGRAVirtualScreen1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    PrevPos: TPoint;
    image: TBGRABitmapThemeUtil;
    number: integer;
  end; 

var
  frmAllOfThem: TfrmAllOfThem;

implementation

{$R *.lfm}

{ TfrmAllOfThem }

procedure TfrmAllOfThem.FormCreate(Sender: TObject);
begin
  BGRAImageButton1.BitmapLoadFromFile(BGRAImageButton1.BitmapFile);

    image := TBGRABitmapThemeUtil.Create('sample_4.png',
    4, // the number of images in the bitmap vertically
    6, // the number of horizontal pixels used as border
    6  // the number of vertical pixels used as border
    );

  number := 0; // the default image

  BGRAGraphicControl1.Align := alLeft;

  BGRAImageList1.GetBitmap(0, BGRAButton1.Glyph);
  BGRAImageList1.GetBitmap(0, BGRASpeedButton1.Glyph);
end;

procedure TfrmAllOfThem.FormDestroy(Sender: TObject);
begin
  image.Free;
end;

procedure TfrmAllOfThem.BGRAKnob1ValueChanged(Sender: TObject; Value: single);
begin
  BGRAFlashProgressBar1.Value := Trunc(BGRAKnob1.Value);
end;

procedure TfrmAllOfThem.BGRAGraphicControl1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  bitmap.Fill(BGRAPixelTransparent);
  image.Draw(bitmap, 0, 0, bitmap.Width, bitmap.Height, number);
end;

procedure TfrmAllOfThem.BGRAGraphicControl1Click(Sender: TObject);
begin
  if number = 3 then
    number := 0
  else
    Inc(number, +1);

  BGRAGraphicControl1.DiscardBitmap;
end;

procedure TfrmAllOfThem.BGRAVirtualScreen1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    BGRAVirtualScreen1.Bitmap.SetPixel(X, Y, BGRABlack);
    PrevPos := Point(X, Y);
    BGRAVirtualScreen1.Repaint;
  end;
end;

procedure TfrmAllOfThem.BGRAVirtualScreen1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    BGRAVirtualScreen1.Bitmap.DrawLineAntialias(X, Y, PrevPos.X,
      PrevPos.Y, BGRABlack, False);
    PrevPos := Point(X, Y);
    BGRAVirtualScreen1.Repaint;
  end;
end;

procedure TfrmAllOfThem.BGRAVirtualScreen1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  Bitmap.DrawLineAntialias(0, 0, Bitmap.Width - 1, Bitmap.Height - 1, BGRABlack, 1);
end;

end.

