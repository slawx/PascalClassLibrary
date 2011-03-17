unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DateUtils, UPlatform, LCLType, IntfGraphics, fpImage,
  Math, GraphType, Contnrs, LclIntf, UFastBitmap, UDrawMethod;

const
  SceneFrameCount = 100;

type


  { TMainForm }

  TMainForm = class(TForm)
    ButtonBenchmark: TButton;
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
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
  public
    DrawMethod: TDrawMethod;
    Bitmap: TBitmap;
    Scenes: TObjectList; // TObjectList<TFastBitmap>
    SceneIndex: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  NewScene: TFastBitmap;
  I: Integer;
begin
  TabSheet1.DoubleBuffered := True;
  Randomize;
  Scenes := TObjectList.Create;
  for I := 0 to SceneFrameCount - 1 do begin
    NewScene := TFastBitmap.Create;
    NewScene.Size := Point(320, 240);
    NewScene.RandomImage;
    Scenes.Add(NewScene);
  end;
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24bit;
  Image1.Picture.Bitmap.SetSize(TFastBitmap(Scenes[0]).Size.X, TFastBitmap(Scenes[0]).Size.Y);
  Bitmap.SetSize(TFastBitmap(Scenes[0]).Size.X, TFastBitmap(Scenes[0]).Size.Y);
  ComboBox1.ItemIndex := 0;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  ButtonStop.Enabled := True;
  ButtonStart.Enabled := False;
  Timer1.Enabled := True;
  DrawMethod.Free;
  if ComboBox1.ItemIndex >= 0 then begin
    DrawMethod := DrawMethodClasses[ComboBox1.ItemIndex].Create;
    DrawMethod.Bitmap := Image1.Picture.Bitmap;
    DrawMethod.Bitmap.SetSize(Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Height);
    repeat
      DrawMethod.DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
      SceneIndex := (SceneIndex + 1) mod Scenes.Count;
      Application.ProcessMessages;
    until not ButtonStop.Enabled;
  end;
  ButtonStopClick(Self);
end;

procedure TMainForm.ButtonBenchmarkClick(Sender: TObject);
var
  NewItem: TListItem;
  I: Integer;
begin
  with ListView1, Items do
  try
    BeginUpdate;
    Clear;
    for I := 0 to High(DrawMethodClasses) do begin
      DrawMethod.Free;
      DrawMethod := DrawMethodClasses[I].Create;
      DrawMethod.Bitmap := Image1.Picture.Bitmap;
      DrawMethod.Bitmap.SetSize(Image1.Picture.Bitmap.Width, Image1.Picture.Bitmap.Height);
      DrawMethod.DrawFrameTiming(TFastBitmap(Scenes[0]));
      NewItem := Add;
      NewItem.Caption := DrawMethod.Caption;
      NewItem.SubItems.Add(FloatToStr(RoundTo(DrawMethod.FrameDuration / OneMillisecond, -3)));
      NewItem.SubItems.Add(FloatToStr(RoundTo(1 / (DrawMethod.FrameDuration / OneSecond), -3)));
    end;
  finally
    EndUpdate;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  ButtonStopClick(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Scenes.Free;
  Bitmap.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if Assigned(DrawMethod) then begin
    if (DrawMethod.FrameDuration > 0) then
      Label2.Caption := FloatToStr(RoundTo(1 / (DrawMethod.FrameDuration / OneSecond), -3))
      else Label2.Caption := '0';
    Label4.Caption := FloatToStr(RoundTo(DrawMethod.FrameDuration / OneMillisecond, -3)) + ' ms';
  end;
end;

end.

