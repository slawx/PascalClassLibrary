unit uwin7toolbar;

{$mode objfpc}{$H+}

interface

uses
  Forms, SysUtils, StdCtrls, Spin, BGRAButton, BGRASamples, BGRAWin7ToolBar,
  Classes, types, Controls, BGRABitmap, BGRABitmapTypes, Graphics, LCLType;

type

  { TfrmWin7ToolBar }

  TfrmWin7ToolBar = class(TForm)
    BGRAButton1: TBGRAButton;
    BGRAButton2: TBGRAButton;
    BGRAButton3: TBGRAButton;
    BGRAButton5: TBGRAButton;
    BGRAButton6: TBGRAButton;
    BGRAButton7: TBGRAButton;
    BGRAButton8: TBGRAButton;
    BGRAWin7ToolBar1: TBGRAWin7ToolBar;
    ComboBox1: TComboBox;
    ListBox1: TListBox;
    SpinEdit1: TSpinEdit;
    procedure BGRAButton1Click(Sender: TObject);
    procedure BGRAButton2Click(Sender: TObject);
    procedure BGRAButton3Click(Sender: TObject);
    procedure BGRAButton5Click(Sender: TObject);
    procedure BGRAButton6Click(Sender: TObject);
    procedure BGRAButton7Click(Sender: TObject);
    procedure BGRAButton8Click(Sender: TObject);
    procedure ComboBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
    procedure SpinEdit1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmWin7ToolBar: TfrmWin7ToolBar;

implementation

{$R *.lfm}

{ TfrmWin7ToolBar }

procedure TfrmWin7ToolBar.BGRAButton2Click(Sender: TObject);
begin
  // Switch align clockwise
  BGRAWin7ToolBar1.SwitchAlign;
end;

procedure TfrmWin7ToolBar.BGRAButton1Click(Sender: TObject);
begin
   if BGRAButton1.Caption = 'glyph' then
    BGRAButton1.Caption := ''
  else
    BGRAButton1.Caption := 'glyph';
end;

procedure TfrmWin7ToolBar.BGRAButton3Click(Sender: TObject);
begin
  BGRAButton3.Down := not BGRAButton3.Down;
end;

procedure TfrmWin7ToolBar.BGRAButton5Click(Sender: TObject);
begin
  BGRAButton5.Enabled := False;
  BGRAButton5.Caption := 'not enabled';
end;

procedure TfrmWin7ToolBar.BGRAButton6Click(Sender: TObject);
begin
  BGRAButton6.StaticButton := not BGRAButton6.StaticButton;
end;

procedure TfrmWin7ToolBar.BGRAButton7Click(Sender: TObject);
begin
  if BGRAButton7.Style = bbtDropDown then
    BGRAButton7.Style := bbtButton
  else
    BGRAButton7.Style := bbtDropDown;
end;

procedure TfrmWin7ToolBar.BGRAButton8Click(Sender: TObject);
begin
  BGRAButton3.Down := True;
  //BGRAButton4.Caption := '';
  BGRAButton5.Enabled := True;
  BGRAButton5.Caption := 'enabled';
  BGRAButton6.StaticButton := True;
  BGRAButton7.Style := bbtDropDown;
end;

procedure TfrmWin7ToolBar.ComboBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  temp: TBGRABitmap;
  str: string;
begin
  str := TComboBox(Control).Items[Index];

  temp := TBGRABitmap.Create(ARect.Right, TComboBox(Control).ItemHeight,
    BGRAPixelTransparent);

  if odSelected in State then
    DrawWin7ToolBar(temp, alNone)
  else
    DrawIOSBackground(temp);

  temp.FontHeight := Trunc(TComboBox(Control).ItemHeight div 2);
  temp.FontQuality := fqFineClearTypeRGB;

  if str <> '' then
  temp.TextOut(10, Trunc((TComboBox(Control).ItemHeight - temp.FontFullHeight) div 2),
    str, BGRABlack);
  temp.Draw(TComboBox(Control).Canvas, ARect, False);
  temp.Free;
end;

procedure TfrmWin7ToolBar.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SpinEdit1.Value := 8;
end;

procedure TfrmWin7ToolBar.FormCreate(Sender: TObject);
var
  ss: TBGRASampleStyle;
begin
  for ss := low(TBGRASampleStyle) to high(TBGRASampleStyle) do
    ListBox1.Items.Add(TBGRASampleStyleStr[ss]);

  SpinEdit1.Value := BGRAButton2.ArrowSize;
end;

procedure TfrmWin7ToolBar.ListBox1DrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  temp: TBGRABitmap;
  str: string;
begin
  str := TListBox(Control).Items[Index];

  temp := TBGRABitmap.Create(ARect.Right, TListBox(Control).ItemHeight,
    BGRAPixelTransparent);

  if odSelected in State then
    DrawWin7ToolBar(temp, alNone)
  else
    DrawIOSBackground(temp);

  temp.FontHeight := Trunc(TListBox(Control).ItemHeight div 2);
  temp.FontQuality := fqFineClearTypeRGB;

  if str <> '' then
  temp.TextOut(10, Trunc((TListBox(Control).ItemHeight - temp.FontFullHeight) div 2),
    str, BGRABlack);
  temp.Draw(TListBox(Control).Canvas, ARect, False);
  temp.Free;
end;

procedure TfrmWin7ToolBar.ListBox1SelectionChange(Sender: TObject; User: boolean
  );
begin
  BGRAWin7ToolBar1.Style := StrToTBGRASampleStyle(ListBox1.GetSelectedText);
end;

procedure TfrmWin7ToolBar.SpinEdit1Change(Sender: TObject);
var
  i: integer;
begin
  with BGRAWin7ToolBar1 do
  begin
    ChildSizing.LeftRightSpacing := Trunc(SpinEdit1.Value / 2);
    ChildSizing.TopBottomSpacing := Trunc(SpinEdit1.Value / 2);
  end;

  // This Changes the Size Variables (ArrowSize, ArrowSpace, AutoSizeExtraVertical, AutoSizeExtraHorizontal).
  // In Windows by default those variables are affected by the DPI (Screen.PixelsPerInch)
  for i:=0 to BGRAWin7ToolBar1.ControlCount - 1 do
  begin
    if BGRAWin7ToolBar1.Controls[i] is TBGRAButton then
      with BGRAWin7ToolBar1.Controls[i] as TBGRAButton do
        SetSizeVariables(SpinEdit1.Value, SpinEdit1.Value*2, SpinEdit1.Value, SpinEdit1.Value*3);
  end;
end;

end.
