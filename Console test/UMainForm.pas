unit UMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, UMyConsoleApp, StdCtrls;

type
  TMainForm = class;

  TMainThread = class(TThread)
    Parent: TMainForm;
    Character: Char;
    TextColor: TColor;
    BackgroundColor: TColor;
    Position: TPoint;
    CursorVisible: Boolean;
    Size: TPoint;
    procedure DoPaintChar;
    procedure DoSizeChange;
    procedure Execute; override;
  end;

  TMainForm = class(TForm)
    Image1: TImage;
    Timer1: TTimer;
    Button1: TButton;
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    Dimensions: TPoint;
    CharacterSize: TPoint;
    MainThread: TMainThread;
    procedure TextScreenPaintChar(Position: TPoint; Character: Char; TextColor, BackgroundColor: TColor; CursorVisible: Boolean);
    procedure TextScreenSizeChange(Size: TPoint);
  public
    MyConsoleApp: TMyConsoleApp;
  end;

var
  MainForm: TMainForm;

implementation

uses UTextScreen;

{$R *.dfm}

procedure TMainForm.Button1Click(Sender: TObject);
begin
  MainThread.Free;
  MainThread := TMainThread.Create(True);
  MainThread.Parent := Self;
  Button1.Enabled := False;
  MainThread.Resume;
end;

procedure TMainThread.DoPaintChar;
begin
  with Parent, Self, Image1.Picture.Bitmap.Canvas do begin
    Brush.Color := BackgroundColor;
    Pen.Color := BackgroundColor;
    Rectangle(Position.X * CharacterSize.X, Position.Y * CharacterSize.Y,
      (Position.X + 1) * CharacterSize.X, (Position.Y + 1) * CharacterSize.Y);
    Font.Color := TextColor;
    TextOut(Position.X * CharacterSize.X, Position.Y * CharacterSize.Y, Character);

    if CursorVisible then begin
      Brush.Color := TextColor;
      Pen.Color := TextColor;
      Rectangle(Position.X * CharacterSize.X, (Position.Y + 1) * CharacterSize.Y - 2,
        (Position.X + 1) * CharacterSize.X, (Position.Y + 1) * CharacterSize.Y);
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MyConsoleApp := TMyConsoleApp.Create;
  MyConsoleApp.Screen.OnPaintChar := TextScreenPaintChar;
  MyConsoleApp.Screen.OnSizeChange := TextScreenSizeChange;
  MainThread := TMainThread.Create(True);
  MainThread.Parent := Self;
  DoubleBuffered := True;
  Dimensions := Point(40, 25);
  CharacterSize := Point(16, 16);
  with Image1.Picture.Bitmap, Canvas do begin
    Font.Name := 'Lucida Console';
    Font.Height := CharacterSize.Y + 2;
    Width := Dimensions.X * CharacterSize.X;
    Height := Dimensions.Y * CharacterSize.Y;
  end;
  MyConsoleApp.Screen.SetSize(Dimensions);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MainThread.Free;
  MyConsoleApp.Destroy;
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  MyConsoleApp.Keyboard.PressKey(Key);
end;

procedure TMainForm.TextScreenPaintChar(Position: TPoint; Character: Char; TextColor, BackgroundColor: TColor; CursorVisible: Boolean);
begin
  MainThread.Position := Position;
  MainThread.Character := Character;
  MainThread.TextColor := TextColor;
  MainThread.BackgroundColor := BackgroundColor;
  MainThread.CursorVisible := CursorVisible;
  MainThread.Synchronize(MainThread.DoPaintChar);
end;

procedure TMainForm.TextScreenSizeChange(Size: TPoint);
begin
  MainThread.Size := Size;
  MainThread.Synchronize(MainThread.DoSizeChange);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  with MyConsoleApp.Screen do begin
    CursorBlinkState := not CursorBlinkState;
    PaintChar(CursorPosition);
  end;
end;

{ TMainThread }

procedure TMainThread.DoSizeChange;
begin
  with Parent, Image1.Picture.Bitmap do begin
    Width := Dimensions.X * CharacterSize.X;
    Height := Dimensions.Y * CharacterSize.Y;
  end;
end;

procedure TMainThread.Execute;
begin
  inherited;
  Parent.MyConsoleApp.Execute;
  Parent.Button1.Enabled := True;
end;

end.
