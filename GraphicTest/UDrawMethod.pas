unit UDrawMethod;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, UPlatform, UFastBitmap, Graphics, Controls,
  LCLType, IntfGraphics, fpImage, GraphType, DateUtils,
  LclIntf{$IFDEF opengl}, GL, GLExt, OpenGLContext{$ENDIF};

type
  TPaintObject = (poImage, poPaintBox, poOpenGL);


  { TDrawMethod }

  TDrawMethod = class
  private
    FControl: TControl;
    FFPS: Real;
  public
    Caption: string;
    Description: TStringList;
    Terminated: Boolean;
    FrameDuration: TDateTime;
    StepDuration: TDateTime;
    PaintObject: TPaintObject;
    TempBitmap: TBitmap;
    FrameCounter: Integer;
    FrameCounterStart: TDateTime;
    function GetFPS: Real;
    property FPS: Real read FFPS write FFPS;
    procedure Init(Parent: TWinControl; Size: TPoint); virtual;
    procedure Done; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure DrawFrame(FastBitmap: TFastBitmap); virtual;
    procedure DrawFrameTiming(FastBitmap: TFastBitmap);
    property Control: TControl read FControl;
  end;

  TDrawMethodClass = class of TDrawMethod;

  { TDrawMethodImage }

  TDrawMethodImage = class(TDrawMethod)
    Image: TImage;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  { TDrawMethodPaintBox }

  TDrawMethodPaintBox = class(TDrawMethod)
    PaintBox: TPaintBox;
    procedure Paint(Sender: TObject); virtual;
    procedure Init(Parent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  {$IFDEF opengl}

  { TDrawMethodOpenGL }

  TDrawMethodOpenGL = class(TDrawMethod)
    OpenGLControl: TOpenGLControl;
    TextureId: GLuint;
    OpenGLBitmap: Pointer;
    procedure InitGL;
    procedure OpenGLControlResize(Sender: TObject);
    procedure Init(AParent: TWinControl; Size: TPoint); override;
    procedure Done; override;
  end;

  {$ENDIF}


implementation


{ TDrawMethodPaintBox }

procedure TDrawMethodPaintBox.Paint(Sender: TObject);
begin

end;

procedure TDrawMethodPaintBox.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  PaintBox := TPaintBox.Create(Parent);
  PaintBox.Parent := Parent;
  PaintBox.SetBounds(0, 0, Size.X, Size.Y);
  PaintBox.OnPaint := Paint;
  PaintBox.Show;
end;

procedure TDrawMethodPaintBox.Done;
begin
  FreeAndNil(PaintBox);
  inherited Done;
end;

{ TDrawMethodImage }

procedure TDrawMethodImage.Init(Parent: TWinControl; Size: TPoint);
begin
  inherited Init(Parent, Size);
  Image := TImage.Create(Parent);
  Image.Parent := Parent;
  Image.SetBounds(0, 0, Size.X, Size.Y);
  Image.Picture.Bitmap.SetSize(Size.X, Size.Y);
  Image.Picture.Bitmap.PixelFormat := pf32bit;
  Image.Show;
end;

procedure TDrawMethodImage.Done;
begin
  FreeAndNil(Image);
  inherited Done;
end;


{$IFDEF opengl}

{ TDrawMethodOpenGL }

procedure TDrawMethodOpenGL.Init(AParent: TWinControl; Size: TPoint);
begin
  inherited Init(aParent, Size);
  OpenGLControl := TOpenGLControl.Create(AParent);
  with OpenGLControl do begin
    Name := 'OpenGLControl';
    Parent := AParent;
    SetBounds(0, 0, Size.X, Size.Y);
    InitGL;
    //OnPaint := OpenGLControl1Paint;
    OnResize := OpenGLControlResize;
  end;
  GetMem(OpenGLBitmap, OpenGLControl.Width * OpenGLControl.Height * SizeOf(Integer));
end;

procedure TDrawMethodOpenGL.Done;
begin
  FreeMem(OpenGLBitmap, OpenGLControl.Width * OpenGLControl.Height);
  FreeAndNil(OpenGLControl);
  inherited;
end;

procedure TDrawMethodOpenGL.OpenGLControlResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);
end;

procedure TDrawMethodOpenGL.InitGL;
begin
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, OpenGLControl.Width, OpenGLControl.Height, 0, 0, 1);
//  glOrtho(0, 1, 1, 0, 0, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glDisable(GL_DEPTH_TEST);
  glViewport(0, 0, OpenGLControl.Width, OpenGLControl.Height);
  //gluPerspective( 45.0, (GLfloat)(OpenGLControl1.Width)/(GLfloat)(OpenGLControl1.Height), 0.1f, 500.0 );

    //glFrustum (-1.0, 1.0, -1.0, 1.0, 1.5, 20.0);
    //glTranslatef (0.0, 0.0,-3.0);
  //  glClearColor(0.0, 0.0, 0.0, 1.0);

  glGenTextures(1, @TextureId);
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
end;

{$ENDIF}


{ TDrawMethod }

function TDrawMethod.GetFPS: Real;
begin
  Result := FrameCounter / ((NowPrecise - FrameCounterStart) / OneSecond);
  FrameCounter := 0;
  FrameCounterStart := NowPrecise;
end;

procedure TDrawMethod.Init(Parent: TWinControl; Size: TPoint);
begin
  if (TempBitmap.Width <> Size.X) or (TempBitmap.Height <> Size.Y) then
    TempBitmap.SetSize(Size.X, Size.Y);
end;

procedure TDrawMethod.Done;
begin

end;

constructor TDrawMethod.Create;
begin
  TempBitmap := TBitmap.Create;
  Description := TStringList.Create;
end;

destructor TDrawMethod.Destroy;
begin
  FreeAndNil(Description);
  FreeAndNil(TempBitmap);
  inherited Destroy;
end;

procedure TDrawMethod.DrawFrame(FastBitmap: TFastBitmap);
begin

end;

procedure TDrawMethod.DrawFrameTiming(FastBitmap: TFastBitmap);
var
  StartTime: TDateTime;
begin
  StartTime := NowPrecise;
  DrawFrame(FastBitmap);
  FrameDuration := NowPrecise - StartTime;
end;

end.

