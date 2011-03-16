unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DateUtils, UPlatform, LCLType, IntfGraphics, fpImage,
  Math, GraphType, Contnrs, LclIntf;

const
  SceneFrameCount = 20;

type

  { TScene }

  TScene = class
  private
    function GetSize: TPoint;
    procedure SetSize(const AValue: TPoint);
  public
    Pixels: array of array of Byte;
    procedure RandomImage;
    property Size: TPoint read GetSize write SetSize;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ComboBox1: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListView1: TListView;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Draw1;
    procedure Draw2;
    procedure Draw3;
    procedure Draw4;
    procedure Draw5;
    procedure Draw6;
    { private declarations }
  public
    Bitmap: TBitmap;
    Frames: Integer;
    Scenes: TObjectList; // TObjectList<TScene>
    SceneIndex: Integer;
    StartTime: TDateTime;
    FrameDuration: TDateTime;
  end;

var
  MainForm: TMainForm;

implementation

{ TScene }

function TScene.GetSize: TPoint;
begin
  Result.X := Length(Pixels);
  if Result.X > 0 then Result.Y := Length(Pixels[0])
    else Result.Y := 0;
end;

procedure TScene.SetSize(const AValue: TPoint);
begin
  SetLength(Pixels, AValue.X, AValue.Y);
end;

procedure TScene.RandomImage;
var
  X, Y: Integer;
begin
  for Y := 0 to Size.Y - 1 do
    for X := 0 to Size.X - 1 do
      Pixels[X, Y] := Random(256);
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  NewScene: TScene;
  I: Integer;
begin
  Randomize;
  Scenes := TObjectList.Create;
  for I := 0 to SceneFrameCount - 1 do begin
    NewScene := TScene.Create;
    NewScene.Size := Point(320, 240);
    NewScene.RandomImage;
    Scenes.Add(NewScene);
  end;
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24bit;
  Image1.Picture.Bitmap.SetSize(TScene(Scenes[0]).Size.X, TScene(Scenes[0]).Size.Y);
  Bitmap.SetSize(TScene(Scenes[0]).Size.X, TScene(Scenes[0]).Size.Y);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  ButtonStop.Enabled := True;
  ButtonStart.Enabled := False;
  Timer1.Enabled := True;
  Frames := 0;
  if ComboBox1.ItemIndex = 0 then Draw1;
  if ComboBox1.ItemIndex = 1 then Draw3;
  if ComboBox1.ItemIndex = 2 then Draw2;
  if ComboBox1.ItemIndex = 3 then Draw4;
  if ComboBox1.ItemIndex = 4 then Draw5;
  if ComboBox1.ItemIndex = 5 then Draw6;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  NewItem: TListItem;
begin
  with ListView1, Items do
  try
    BeginUpdate;
    Clear;
    Draw1;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[0];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    Draw3;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[1];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    Draw2;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[2];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    Draw4;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[3];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    Draw5;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[4];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
    Draw6;
    NewItem := Add;
    NewItem.Caption := ComboBox1.Items[5];
    NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
  finally
    EndUpdate;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Scenes.Free;
  Bitmap.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Label2.Caption := IntToStr(Frames);
  Label4.Caption := FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3));
  Frames := 0;
end;

procedure TMainForm.Draw1;
var
  Y, X: Integer;
begin
  repeat
    StartTime := NowPrecise;
    Inc(Frames);
    with TScene(Scenes[SceneIndex]) do begin
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do
          Image1.Picture.Bitmap.Canvas.Pixels[X, Y] := Pixels[X, Y] * $010101;
      FrameDuration := NowPrecise - StartTime;
    end;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
end;

procedure TMainForm.Draw2;
var
  Y, X: Integer;
  TempIntfImage: TLazIntfImage;
begin
  try
  TempIntfImage := TLazIntfImage.Create(0, 0);
  repeat
    StartTime := NowPrecise;
    Inc(Frames);
    with TScene(Scenes[SceneIndex]) do begin
      TempIntfImage.LoadFromBitmap(Image1.Picture.Bitmap.Handle,
        Image1.Picture.Bitmap.MaskHandle);
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do
          TempIntfImage.Colors[X, Y] := TColorToFPColor(Pixels[X, Y] * $010101);
      Image1.Picture.Bitmap.LoadFromIntfImage(TempIntfImage);
      FrameDuration := NowPrecise - StartTime;
    end;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
  finally
    TempIntfImage.Free;
  end;
end;

procedure TMainForm.Draw3;
var
  Y, X: Integer;
begin
  repeat
    StartTime := NowPrecise;
    Inc(Frames);
    with TScene(Scenes[SceneIndex]) do
    try
      Image1.Picture.Bitmap.BeginUpdate(True);
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do
          Image1.Picture.Bitmap.Canvas.Pixels[X, Y] := Pixels[X, Y] * $010101;
    finally
      Image1.Picture.Bitmap.EndUpdate(False);
    end;
    FrameDuration := NowPrecise - StartTime;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
end;

procedure TMainForm.Draw4;
var
  Y, X: Integer;
  TempIntfImage: TLazIntfImage;
  C: TFPColor;
begin
  try
  TempIntfImage := Image1.Picture.Bitmap.CreateIntfImage;
  repeat
    StartTime := NowPrecise;
    Inc(Frames);

    with TScene(Scenes[SceneIndex]) do begin
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do begin
          C := TColorToFPColor(Pixels[X, Y] * $010101);
          TempIntfImage.Colors[X, Y] := C;
        end;
      Image1.Picture.Bitmap.LoadFromIntfImage(TempIntfImage);
    end;
    FrameDuration := NowPrecise - StartTime;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
  finally
    TempIntfImage.Free;
  end;
end;

procedure TMainForm.Draw5;
var
  Y, X: Integer;
  PixelPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
begin
  P := Image1.Picture.Bitmap.PixelFormat;
  repeat
    StartTime := NowPrecise;
    Inc(Frames);
    with TScene(Scenes[SceneIndex]) do
    try
      Image1.Picture.Bitmap.BeginUpdate(False);
      RawImage := Image1.Picture.Bitmap.RawImage;
      PixelPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do begin
          PixelPtr^ := Pixels[X, Y] * $010101;
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
    finally
      Image1.Picture.Bitmap.EndUpdate(False);
    end;
    FrameDuration := NowPrecise - StartTime;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
end;

procedure TMainForm.Draw6;
var
  Y, X: Integer;
  PixelPtr: PInteger;
  P: TPixelFormat;
  RawImage: TRawImage;
  BytePerPixel: Integer;
  hPaint, hBmp: HDC;
begin
  P := Image1.Picture.Bitmap.PixelFormat;
  repeat
    StartTime := NowPrecise;
    Inc(Frames);
    with TScene(Scenes[SceneIndex]) do
    try
      Bitmap.BeginUpdate(False);
      RawImage := Bitmap.RawImage;
      PixelPtr := PInteger(RawImage.Data);
      BytePerPixel := RawImage.Description.BitsPerPixel div 8;
      for X := 0 to Size.X - 1 do
        for Y := 0 to Size.Y - 1 do begin
          PixelPtr^ := Pixels[X, Y] * $010101;
          Inc(PByte(PixelPtr), BytePerPixel);
        end;
    finally
      Bitmap.EndUpdate(False);
    end;
    hBmp := Bitmap.Canvas.Handle;
    hPaint := PaintBox1.Canvas.Handle;
    BitBlt(hPaint, 0, 0, Bitmap.Width, Bitmap.Height, hBmp, 0, 0, srcCopy);

    FrameDuration := NowPrecise - StartTime;
    SceneIndex := (SceneIndex + 1) mod Scenes.Count;
    Application.ProcessMessages;
  until ButtonStart.Enabled;
end;

end.

