unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Forms, Spin, ExtCtrls, StdCtrls, BGRAVirtualScreen, BGRABitmap, BGRASliceScaling, Classes,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    CheckBox1: TCheckBox;
    CheckBox_RepeatMiddleHoriz: TCheckBox;
    CheckBox_RepeatMiddleVert: TCheckBox;
    CheckBox_RepeatTop: TCheckBox;
    CheckBox_RepeatRight: TCheckBox;
    CheckBox_RepeatLeft: TCheckBox;
    CheckBox_RepeatBottom: TCheckBox;
    Image2: TImage;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure CheckBox_RepeatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpinEditChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    sliceScaling: TBGRASliceScaling;
    initializing: boolean;
  end;

var
  Form1: TForm1;

implementation

uses Types;

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  sliceScaling.ResampleMode := rmSimpleStretch;
  sliceScaling.SliceRepeat[srpTop] := CheckBox_RepeatTop.Checked;
  sliceScaling.SliceRepeat[srpLeft] := CheckBox_RepeatLeft.Checked;
  sliceScaling.SliceRepeat[srpRight] := CheckBox_RepeatRight.Checked;
  sliceScaling.SliceRepeat[srpBottom] := CheckBox_RepeatBottom.Checked;
  sliceScaling.SliceRepeat[srpMiddleHorizontal] := CheckBox_RepeatMiddleHoriz.Checked;
  sliceScaling.SliceRepeat[srpMiddleVertical] := CheckBox_RepeatMiddleVert.Checked;
  sliceScaling.SetMargins(SpinEdit1.Value,SpinEdit2.Value,SpinEdit3.Value,SpinEdit4.Value);
  sliceScaling.Draw(Bitmap, rect(0,0,Bitmap.Width,Bitmap.Height), CheckBox1.Checked);
  sliceScaling.Draw(Bitmap, rect(0,0,Bitmap.Width div 2,Bitmap.Height div 2), CheckBox1.Checked);
  sliceScaling.Draw(Bitmap, rect(Bitmap.Width*3 div 4,Bitmap.Height*3 div 4,Bitmap.Width,Bitmap.Height), CheckBox1.Checked);
end;

procedure TForm1.CheckBox_RepeatChange(Sender: TObject);
begin
  if not initializing then BGRAVirtualScreen1.RedrawBitmap;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  initializing := true;
  sliceScaling := TBGRASliceScaling.Create(Image2.Picture.Bitmap,
    SpinEdit1.Value, SpinEdit2.Value, SpinEdit3.Value, SpinEdit4.Value);
  sliceScaling.AutodetectRepeat;

  CheckBox_RepeatTop.Checked := sliceScaling.SliceRepeat[srpTop];
  CheckBox_RepeatLeft.Checked := sliceScaling.SliceRepeat[srpLeft];
  CheckBox_RepeatRight.Checked := sliceScaling.SliceRepeat[srpRight];
  CheckBox_RepeatBottom.Checked := sliceScaling.SliceRepeat[srpBottom];
  CheckBox_RepeatMiddleHoriz.Checked := sliceScaling.SliceRepeat[srpMiddleHorizontal];
  CheckBox_RepeatMiddleVert.Checked := sliceScaling.SliceRepeat[srpMiddleVertical];

  initializing := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sliceScaling.Free;
end;

procedure TForm1.SpinEditChange(Sender: TObject);
begin
  if not initializing then BGRAVirtualScreen1.RedrawBitmap;
end;

{$R *.lfm}

end.
