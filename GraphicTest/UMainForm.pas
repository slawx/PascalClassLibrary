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
    ButtonStop: TButton;
    ButtonBenchmark: TButton;
    ButtonSingleTest: TButton;
    FloatSpinEdit1: TFloatSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    ListViewMethods: TListView;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    procedure ButtonBenchmarkClick(Sender: TObject);
    procedure ButtonSingleTestClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewMethodsData(Sender: TObject; Item: TListItem);
    procedure ListViewMethodsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    OpenGLControl1: TOpenGLControl;
    TextureId: GLuint;
    TextureData: Pointer;
    MethodIndex: Integer;
    SingleTestActive: Boolean;
    AllTestActive: Boolean;
    procedure OpenGLControl1Resize(Sender: TObject);
    procedure InitGL;
    procedure UpdateMethodList;
    procedure UpdateInterface;
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
  Image1.Picture.Bitmap.PixelFormat := pf32bit;
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
  for I := 0 to High(DrawMethodClasses) do begin
    NewDrawMethod := DrawMethodClasses[I].Create;
    NewDrawMethod.Bitmap := Image1.Picture.Bitmap;
    NewDrawMethod.PaintBox := PaintBox1;
    NewDrawMethod.OpenGLBitmap := TextureData;
    NewDrawMethod.OpenGLControl := OpenGLControl1;
    NewDrawMethod.Init;
    DrawMethods.Add(NewDrawMethod);
  end;
end;

procedure TMainForm.ButtonSingleTestClick(Sender: TObject);
begin
  try
    SingleTestActive := True;
    UpdateInterface;
    Timer1.Enabled := True;
    MethodIndex := ListViewMethods.Selected.Index;
    Timer1.Enabled := True;
    if MethodIndex >= 0 then
    with TDrawMethod(DrawMethods[MethodIndex]) do begin
      PageControl1.TabIndex := Integer(PaintObject);
      Application.ProcessMessages;
      repeat
        DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
        SceneIndex := (SceneIndex + 1) mod Scenes.Count;
        Application.ProcessMessages;
      until not SingleTestActive;
    end;
  finally
    Timer1.Enabled := False;
    SingleTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonBenchmarkClick(Sender: TObject);
var
  I: Integer;
  C: Integer;
  StartTime: TDateTime;
begin
  try
    AllTestActive := True;
    UpdateInterface;
    Timer1.Enabled := True;
    with ListViewMethods, Items do
    for I := 0 to DrawMethods.Count - 1 do
    with TDrawMethod(DrawMethods[I]) do begin
      MethodIndex := I;
      PageControl1.TabIndex := Integer(PaintObject);
      StartTime := NowPrecise;
      repeat
        DrawFrameTiming(TFastBitmap(Scenes[SceneIndex]));
        SceneIndex := (SceneIndex + 1) mod Scenes.Count;
        Application.ProcessMessages;
      until ((NowPrecise - StartTime) > OneSecond * FloatSpinEdit1.Value) or not AllTestActive;
    end;
  finally
    Timer1.Enabled := False;
    AllTestActive := False;
    UpdateInterface;
  end;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  SingleTestActive := False;
  AllTestActive := False;
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

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateMethodList;
  UpdateInterface;
end;

procedure TMainForm.ListViewMethodsData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < DrawMethods.Count) then
  with TDrawMethod(DrawMethods[Item.Index]) do begin
    Item.Caption := Caption;
    if FrameDuration > 0 then
      Item.SubItems.Add(FloatToStr(RoundTo(1 / (FrameDuration / OneSecond), -3)))
      else Item.SubItems.Add('0');
    Item.SubItems.Add(FloatToStr(RoundTo(FrameDuration / OneMillisecond, -3)) + ' ms');
  end;
end;

procedure TMainForm.ListViewMethodsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateInterface;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  UpdateMethodList;
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

procedure TMainForm.UpdateMethodList;
begin
  ListViewMethods.Items.Count := DrawMethods.Count;
  ListViewMethods.Refresh;
end;

procedure TMainForm.UpdateInterface;
begin
  ButtonSingleTest.Enabled := not SingleTestActive and not AllTestActive and Assigned(ListViewMethods.Selected);
  ButtonBenchmark.Enabled := not AllTestActive and not SingleTestActive;
  ButtonStop.Enabled := SingleTestActive or AllTestActive;
end;

end.

