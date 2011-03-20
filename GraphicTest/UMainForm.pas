unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, DateUtils, UPlatform, LCLType, IntfGraphics, fpImage,
  Math, GraphType, Contnrs, LclIntf, Spin, UFastBitmap, UDrawMethod, GL,
  OpenGLContext;

const
  SceneFrameCount = 100;

type


  { TMainForm }

  TMainForm = class(TForm)
    ButtonBenchmark: TButton;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ComboBox1: TComboBox;
    FloatSpinEdit1: TFloatSpinEdit;
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
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    OpenGLControl1: TOpenGLControl;
    TextureId: GLuint;
    TextureData: Pointer;
    MethodIndex: Integer;
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure InitGL;
  public
    DrawMethods: TObjectList; // TObjectList<TDrawMethod>
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
  NewDrawMethod: TDrawMethod;
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

  OpenGLControl1 := TOpenGLControl.Create(Self);
  with OpenGLControl1 do begin
    Name := 'OpenGLControl1';
    Parent := TabSheet3;
    SetBounds(0, 0, 320, 240);
    InitGL;
    //OnPaint := OpenGLControl1Paint;
    OnResize := OpenGLControl1Resize;
  end;
  GetMem(TextureData, OpenGLControl1.Width * OpenGLControl1.Height * SizeOf(Integer));

  DrawMethods := TObjectList.Create;
  ComboBox1.Clear;
  for I := 0 to High(DrawMethodClasses) do begin
    NewDrawMethod := DrawMethodClasses[I].Create;
    NewDrawMethod.Bitmap := Image1.Picture.Bitmap;
    NewDrawMethod.PaintBox := PaintBox1;
    NewDrawMethod.OpenGLBitmap := TextureData;
    NewDrawMethod.OpenGLControl := OpenGLControl1;
    NewDrawMethod.Init;
    DrawMethods.Add(NewDrawMethod);
    ComboBox1.Items.Add(NewDrawMethod.Caption);
  end;
  ComboBox1.ItemIndex := DrawMethods.Count - 1;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
begin
  MethodIndex := ComboBox1.ItemIndex;
  ButtonStop.Enabled := True;
  ButtonStart.Enabled := False;
  Timer1.Enabled := True;
  if MethodIndex >= 0 then
  with TDrawMethod(DrawMethods[MethodIndex]) do begin
    PageControl1.TabIndex := Integer(PaintObject);
    Application.ProcessMessages;
    repeat
      DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
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
  C: Integer;
  StartTime: TDateTime;
begin
  Timer1.Enabled := True;
  with ListView1, Items do
  try
    //BeginUpdate;
    Clear;
    for I := 0 to DrawMethods.Count - 1 do
    with TDrawMethod(DrawMethods[I]) do begin
      MethodIndex := I;
      PageControl1.TabIndex := Integer(PaintObject);
      StartTime := NowPrecise;
      repeat
        DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
        SceneIndex := (SceneIndex + 1) mod Scenes.Count;
        Application.ProcessMessages;
      until (NowPrecise - StartTime) > OneSecond * FloatSpinEdit1.Value;
      NewItem := Add;
      NewItem.Caption := Caption;
      NewItem.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)));
      NewItem.SubItems.Add(FloatToStr(RoundTo(1 / (FrameDuration / OneSecond), -3)));
    end;
  finally
    //EndUpdate;
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
  FreeMem(TextureData, OpenGLControl1.Width * OpenGLControl1.Height);
  DrawMethods.Free;
  Scenes.Free;
  Bitmap.Free;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  if (MethodIndex >= 0) then
  with TDrawMethod(DrawMethods[MethodIndex]) do begin
    if (FrameDuration > 0) then
      Label2.Caption := FloatToStr(RoundTo(1 / (FrameDuration / OneSecond), -3))
      else Label2.Caption := '0';
    Label4.Caption := FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)) + ' ms';
  end;
end;

procedure TMainForm.OpenGLControl1Resize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
end;

procedure TMainForm.InitGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.Height, 0, 0, 1);
//  glOrtho(0, 1, 1, 0, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glDisable(GL_DEPTH_TEST);
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
  //gluPerspective( 45.0, (GLfloat)(OpenGLControl1.Width)/(GLfloat)(OpenGLControl1.Height), 0.1f, 500.0 );

    //glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
    //glTranslatef (0.0, 0.0,-3.0);
  //  glClearColor(0.0, 0.0, 0.0, 1.0);

  glGenTextures(1, @TextureId);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
end;

end.

