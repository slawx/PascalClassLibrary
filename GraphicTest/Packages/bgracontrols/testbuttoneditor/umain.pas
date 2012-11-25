unit umain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Dialogs, Spin, StdCtrls, ExtCtrls,
  BGRAVirtualScreen, BGRAGraphicControl, BGRABitmap, BGRASamples,
  BGRABitmapTypes, BGRATextFX, FileUtil, IniFiles, SysUtils;

type

  { TfrmButtonEditor }

  TfrmButtonEditor = class(TForm)
    BorderGroup: TGroupBox;
    borWidth: TFloatSpinEdit;
    bs2: TComboBox;
    bs3: TComboBox;
    bs4: TComboBox;
    btnColorAlpha: TSpinEdit;
    btnFont: TButton;
    btnHeight: TSpinEdit;
    btnSave: TButton;
    btnText: TEdit;
    btnWidth: TSpinEdit;
    btnSaveToINI: TButton;
    btnLoadFromINI: TButton;
    buttonGraphic: TBGRAGraphicControl;
    btnColor: TColorButton;
    GeneralGroup: TGroupBox;
    LoadFromIni: TOpenDialog;
    SaveToIni: TSaveDialog;
    StyleGroup: TCheckGroup;
    bs1: TComboBox;
    gd1Horiz: TCheckBox;
    gd2Horiz: TCheckBox;
    gpHoriz: TCheckBox;
    DirectionsGroup: TCheckGroup;
    cl1: TColorButton;
    cl1a: TSpinEdit;
    cl2: TColorButton;
    cl2a: TSpinEdit;
    cl3: TColorButton;
    cl3a: TSpinEdit;
    cl4: TColorButton;
    cl4a: TSpinEdit;
    GradientsGroup: TGroupBox;
    gradValue: TFloatSpinEdit;
    innerAlpha: TSpinEdit;
    innerColor: TColorButton;
    outAlpha: TSpinEdit;
    outColor: TColorButton;
    roundHoriz: TFloatSpinEdit;
    roundVert: TFloatSpinEdit;
    SaveBitmap: TSaveDialog;
    ScrollArea: TScrollBox;
    ScrollBoxMain: TScrollBox;
    ShadowGroup: TGroupBox;
    shdColor: TColorButton;
    shdColorAlpha: TSpinEdit;
    shdHoriz: TSpinEdit;
    shdRadius: TSpinEdit;
    shdVert: TSpinEdit;
    SplitterNice: TSplitter;
    TextGroup: TGroupBox;
    ButtonFont: TFontDialog;
    tileBackgroundValue: TSpinEdit;
    vsTileBackground: TBGRAVirtualScreen;
    procedure btnFontClick(Sender: TObject);
    procedure btnLoadFromINIClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveToINIClick(Sender: TObject);
    procedure buttonGraphicRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure vsTileBackgroundRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure tileBackgroundValueChange(Sender: TObject);
    procedure UpdateButton(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmButtonEditor: TfrmButtonEditor;

function BGRAToStrRGBA(AColor: TBGRAPixel): string;

implementation

{$R *.lfm}

function BGRAToStrRGBA(AColor: TBGRAPixel): string;
begin
  Result := 'rgba(' + IntToStr(AColor.red) + ',' + IntToStr(AColor.green) +
    ',' + IntToStr(AColor.blue) + ',' + IntToStr(AColor.alpha) + ')';
end;

{ TfrmButtonEditor }

procedure TfrmButtonEditor.buttonGraphicRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  shadow: TBGRACustomBitmap;
  gd1, gd2, gd3: TGradientDirection;
  tl, tr, br, bl: TRoundRectangleOptions;
  rh, rv: double;
begin
  tl := [];
  tr := [];
  br := [];
  bl := [];

  { Bevel Border }
  if bs1.ItemIndex = 1 then
    tl := [rrTopLeftBevel]
  else if bs1.ItemIndex = 2 then
    tl := [rrTopLeftSquare]
  else
    tl := [];

  if bs2.ItemIndex = 1 then
    tr := [rrTopRightBevel]
  else if bs2.ItemIndex = 2 then
    tr := [rrTopRightSquare]
  else
    tr := [];

  if bs4.ItemIndex = 1 then
    br := [rrBottomRightBevel]
  else if bs4.ItemIndex = 2 then
    br := [rrBottomRightSquare]
  else
    br := [];

  if bs3.ItemIndex = 1 then
    bl := [rrBottomLeftBevel]
  else if bs3.ItemIndex = 2 then
    bl := [rrBottomLeftSquare]
  else
    bl := [];

  { Set Bevel square if round is zero }
  if (roundHoriz.Value = 0) or (roundVert.Value = 0) then
  begin
    br := [rrTopLeftSquare, rrTopRightSquare, rrBottomLeftSquare, rrBottomRightSquare];
    rh := 1;
    rv := 1;
  end
  else
  begin
    rh := roundHoriz.Value;
    rv := roundVert.Value;
  end;

  { Gradient Directions }
  if gd1Horiz.Checked then
    gd1 := gdHorizontal
  else
    gd1 := gdVertical;

  if gd2Horiz.Checked then
    gd2 := gdHorizontal
  else
    gd2 := gdVertical;

  if gpHoriz.Checked then
    gd3 := gdHorizontal
  else
    gd3 := gdVertical;

  // clear
  bitmap.FillTransparent;

  { Button }
  DrawButton(bitmap, ColorToBGRA(cl1.ButtonColor, cl1a.Value),
    ColorToBGRA(cl2.ButtonColor, cl2a.Value),
    ColorToBGRA(cl3.ButtonColor, cl3a.Value),
    ColorToBGRA(cl4.ButtonColor, cl4a.Value),
    ColorToBGRA(outColor.ButtonColor, outAlpha.Value),
    ColorToBGRA(innerColor.ButtonColor, innerAlpha.Value),
    rh, rv,
    borWidth.Value, gradValue.Value,
    gd1, gd2, gd3, tl + tr + br + bl);

  { Text }
  shadow := TextShadow(buttonGraphic.Width, buttonGraphic.Height,
    btnText.Text, ButtonFont.Font.Height, ColorToBGRA(btnColor.ButtonColor,
    btnColorAlpha.Value), ColorToBGRA(shdColor.ButtonColor, shdColorAlpha.Value),
    shdHoriz.Value, shdVert.Value, shdRadius.Value, ButtonFont.Font.Style,
    ButtonFont.Font.Name, True, fqFineAntialiasing);

  // draw text and free
  bitmap.PutImage(0, 0, shadow, dmDrawWithTransparency);
  shadow.Free;
end;

procedure TfrmButtonEditor.btnSaveClick(Sender: TObject);
begin
  if SaveBitmap.Execute then
  begin
    buttonGraphic.Bitmap.SaveToFile(SaveBitmap.FileName);
    if FileExistsUTF8(SaveBitmap.FileName) then
      ShowMessage('Saved! ' + SaveBitmap.FileName)
    else
      ShowMessage('Error. File not found.');
  end;
end;

procedure TfrmButtonEditor.btnLoadFromINIClick(Sender: TObject);

  function BGRAToStrRGBA(AColor: TBGRAPixel): string;
  begin
    Result := 'rgba(' + IntToStr(AColor.red) + ',' + IntToStr(AColor.green) +
      ',' + IntToStr(AColor.blue) + ',' + IntToStr(AColor.alpha) + ')';
  end;

var
  temp: TMemIniFile;
  tempColor: TBGRAPixel;
begin
  if LoadFromIni.Execute then
  begin
    { Create }
    temp := TMemIniFile.Create(LoadFromIni.FileName);
    { General }
    btnWidth.Value := temp.ReadInteger('General', 'Width', btnWidth.Value);
    btnHeight.Value := temp.ReadInteger('General', 'Heigh', btnHeight.Value);
    { Gradients }
    tempColor := StrToBGRA(temp.ReadString('Gradients', 'Color1',
      BGRAToStrRGBA(ColorToBGRA(cl1.ButtonColor, cl1a.Value))));
    cl1.ButtonColor := BGRAToColor(tempColor);
    cl1a.Value := tempColor.alpha;
    tempColor := StrToBGRA(temp.ReadString('Gradients', 'Color2',
      BGRAToStrRGBA(ColorToBGRA(cl2.ButtonColor, cl2a.Value))));
    cl2.ButtonColor := BGRAToColor(tempColor);
    cl2a.Value := tempColor.alpha;
    tempColor := StrToBGRA(temp.ReadString('Gradients', 'Color3',
      BGRAToStrRGBA(ColorToBGRA(cl3.ButtonColor, cl3a.Value))));
    cl3.ButtonColor := BGRAToColor(tempColor);
    cl3a.Value := tempColor.alpha;
    tempColor := StrToBGRA(temp.ReadString('Gradients', 'Color4',
      BGRAToStrRGBA(ColorToBGRA(cl4.ButtonColor, cl4a.Value))));
    cl4.ButtonColor := BGRAToColor(tempColor);
    cl4a.Value := tempColor.alpha;
    gradValue.Value := temp.ReadFloat('Gradients', 'Position', gradValue.Value);
    { Text }
    btnText.Text := temp.ReadString('Text', 'ButtonText', btnText.Text);
    ButtonFont.Font.Name := temp.ReadString('Text', 'FontName', ButtonFont.Font.Name);
    ButtonFont.Font.Height := temp.ReadInteger('Text', 'FontHeight',
      ButtonFont.Font.Height);
    tempColor := StrToBGRA(temp.ReadString('Text', 'FontColor',
      BGRAToStrRGBA(ColorToBGRA(btnColor.ButtonColor, btnColorAlpha.Value))));
    btnColor.ButtonColor := BGRAToColor(tempColor);
    btnColorAlpha.Value := tempColor.alpha;
    // bold
    if temp.ReadBool('Text', 'FontBold', False) then
      ButtonFont.Font.Style := ButtonFont.Font.Style + [fsBold]
    else
      ButtonFont.Font.Style := ButtonFont.Font.Style - [fsBold];
    // italic
    if temp.ReadBool('Text', 'FontItalic', False) then
      ButtonFont.Font.Style := ButtonFont.Font.Style + [fsItalic]
    else
      ButtonFont.Font.Style := ButtonFont.Font.Style - [fsItalic];
    // strikeout
    if temp.ReadBool('Text', 'FontStrikeOut', False) then
      ButtonFont.Font.Style := ButtonFont.Font.Style + [fsStrikeOut]
    else
      ButtonFont.Font.Style := ButtonFont.Font.Style - [fsStrikeOut];
    // underline
    if temp.ReadBool('Text', 'FontUnderline', False) then
      ButtonFont.Font.Style := ButtonFont.Font.Style + [fsUnderline]
    else
      ButtonFont.Font.Style := ButtonFont.Font.Style - [fsUnderline];

    { Shadow }
    tempColor := StrToBGRA(temp.ReadString('Shadow', 'Color',
      BGRAToStrRGBA(ColorToBGRA(shdColor.ButtonColor, shdColorAlpha.Value))));
    shdColor.ButtonColor := BGRAToColor(tempColor);
    shdColorAlpha.Value := tempColor.alpha;
    shdHoriz.Value := temp.ReadInteger('Shadow', 'HorizontalOffset', shdHoriz.Value);
    shdVert.Value := temp.ReadInteger('Shadow', 'VerticalOffset', shdVert.Value);
    shdRadius.Value := temp.ReadInteger('Shadow', 'Radius', shdRadius.Value);
    { Direction }
    gd1Horiz.Checked := temp.ReadBool('Direction', '1Horizontal', gd1Horiz.Checked);
    gd2Horiz.Checked := temp.ReadBool('Direction', '2Horizontal', gd2Horiz.Checked);
    gpHoriz.Checked := temp.ReadBool('Direction', 'Horizontal', gpHoriz.Checked);
    { Border }
    tempColor := StrToBGRA(temp.ReadString('Border', 'OutColor',
      BGRAToStrRGBA(ColorToBGRA(outColor.ButtonColor, outAlpha.Value))));
    outColor.ButtonColor := BGRAToColor(tempColor);
    outAlpha.Value := tempColor.alpha;
    tempColor := StrToBGRA(temp.ReadString('Border', 'InnerColor',
      BGRAToStrRGBA(ColorToBGRA(innerColor.ButtonColor, innerAlpha.Value))));
    innerColor.ButtonColor := BGRAToColor(tempColor);
    innerAlpha.Value := tempColor.alpha;
    roundHoriz.Value := temp.ReadFloat('Border', 'RoundHorizontal', roundHoriz.Value);
    roundVert.Value := temp.ReadFloat('Border', 'RoundVertical', roundVert.Value);
    borWidth.Value := temp.ReadFloat('Border', 'BorderWidth', borWidth.Value);
    { Border Style }
    bs1.ItemIndex := temp.ReadInteger('BorderStyle', 'TopLeft', bs1.ItemIndex);
    bs2.ItemIndex := temp.ReadInteger('BorderStyle', 'TopRight', bs2.ItemIndex);
    bs3.ItemIndex := temp.ReadInteger('BorderStyle', 'BottomLeft', bs3.ItemIndex);
    bs4.ItemIndex := temp.ReadInteger('BorderStyle', 'BottomRight', bs4.ItemIndex);
    { Redraw }
    buttonGraphic.RedrawBitmap;
    { Free }
    temp.Free;
  end;
end;

procedure TfrmButtonEditor.btnFontClick(Sender: TObject);
begin
  ButtonFont.Font.Color := btnColor.ButtonColor;
  if ButtonFont.Execute then
    buttonGraphic.RedrawBitmap;
end;

procedure TfrmButtonEditor.btnSaveToINIClick(Sender: TObject);
var
  temp: TMemIniFile;
begin
  if SaveToIni.Execute then
  begin
    { Create }
    temp := TMemIniFile.Create(SaveToIni.FileName);
    { General }
    temp.WriteInteger('General', 'Width', btnWidth.Value);
    temp.WriteInteger('General', 'Heigh', btnHeight.Value);
    { Gradients }
    temp.WriteString('Gradients', 'Color1',
      BGRAToStrRGBA(ColorToBGRA(cl1.ButtonColor, cl1a.Value)));
    temp.WriteString('Gradients', 'Color2',
      BGRAToStrRGBA(ColorToBGRA(cl2.ButtonColor, cl2a.Value)));
    temp.WriteString('Gradients', 'Color3',
      BGRAToStrRGBA(ColorToBGRA(cl3.ButtonColor, cl3a.Value)));
    temp.WriteString('Gradients', 'Color4',
      BGRAToStrRGBA(ColorToBGRA(cl4.ButtonColor, cl4a.Value)));
    temp.WriteFloat('Gradients', 'Position', gradValue.Value);
    { Text }
    temp.WriteString('Text', 'ButtonText', btnText.Text);
    temp.WriteString('Text', 'FontName', ButtonFont.Font.Name);
    temp.WriteInteger('Text', 'FontHeight', ButtonFont.Font.Height);
    temp.WriteString('Text', 'FontColor',
      BGRAToStrRGBA(ColorToBGRA(btnColor.ButtonColor, btnColorAlpha.Value)));
    // bold
    if fsBold in ButtonFont.Font.Style then
      temp.WriteBool('Text', 'FontBold', True)
    else
      temp.WriteBool('Text', 'FontBold', False);
    // italic
    if fsItalic in ButtonFont.Font.Style then
      temp.WriteBool('Text', 'FontItalic', True)
    else
      temp.WriteBool('Text', 'FontItalic', False);
    // strikeout
    if fsStrikeOut in ButtonFont.Font.Style then
      temp.WriteBool('Text', 'FontStrikeOut', True)
    else
      temp.WriteBool('Text', 'FontStrikeOut', False);
    // underline
    if fsUnderline in ButtonFont.Font.Style then
      temp.WriteBool('Text', 'FontUnderline', True)
    else
      temp.WriteBool('Text', 'FontUnderline', False);
    { Shadow }
    temp.WriteString('Shadow', 'Color',
      BGRAToStrRGBA(ColorToBGRA(shdColor.ButtonColor, shdColorAlpha.Value)));
    temp.WriteInteger('Shadow', 'HorizontalOffset', shdHoriz.Value);
    temp.WriteInteger('Shadow', 'VerticalOffset', shdVert.Value);
    temp.WriteInteger('Shadow', 'Radius', shdRadius.Value);
    { Direction }
    temp.WriteBool('Direction', '1Horizontal', gd1Horiz.Checked);
    temp.WriteBool('Direction', '2Horizontal', gd2Horiz.Checked);
    temp.WriteBool('Direction', 'Horizontal', gpHoriz.Checked);
    { Border }
    temp.WriteString('Border', 'OutColor',
      BGRAToStrRGBA(ColorToBGRA(outColor.ButtonColor, outAlpha.Value)));
    temp.WriteString('Border', 'InnerColor',
      BGRAToStrRGBA(ColorToBGRA(innerColor.ButtonColor, innerAlpha.Value)));
    temp.WriteFloat('Border', 'RoundHorizontal', roundHoriz.Value);
    temp.WriteFloat('Border', 'RoundVertical', roundVert.Value);
    temp.WriteFloat('Border', 'BorderWidth', borWidth.Value);
    { Border Style }
    temp.WriteInteger('BorderStyle', 'TopLeft', bs1.ItemIndex);
    temp.WriteInteger('BorderStyle', 'TopRight', bs2.ItemIndex);
    temp.WriteInteger('BorderStyle', 'BottomLeft', bs3.ItemIndex);
    temp.WriteInteger('BorderStyle', 'BottomRight', bs4.ItemIndex);
    { Save and Free}
    temp.UpdateFile;
    temp.Free;
  end;
end;

procedure TfrmButtonEditor.vsTileBackgroundRedraw(Sender: TObject; Bitmap: TBGRABitmap);

  procedure DrawTileBackground(ABitmap: TBGRABitmap; Multiply: integer);
  var
    temp: TBGRABitmap;
  begin
    temp := TBGRABitmap.Create(2, 2, BGRAWhite);
    temp.SetPixel(0, 0, clSilver);
    temp.SetPixel(1, 1, clSilver);
    BGRAReplace(temp, temp.Resample(2 * Multiply, 2 * Multiply, rmSimpleStretch));
    ABitmap.Fill(temp, dmSet);
    temp.Free;
  end;

begin
  DrawTileBackground(bitmap, tileBackgroundValue.Value);
end;

procedure TfrmButtonEditor.tileBackgroundValueChange(Sender: TObject);
begin
  vsTileBackground.RedrawBitmap;
end;

procedure TfrmButtonEditor.UpdateButton(Sender: TObject);
begin
  buttonGraphic.Width := btnWidth.Value;
  buttonGraphic.Height := btnHeight.Value;
  buttonGraphic.RedrawBitmap;
end;

end.

