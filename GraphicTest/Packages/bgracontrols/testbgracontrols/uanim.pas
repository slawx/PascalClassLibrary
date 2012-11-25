unit uanim;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Dialogs, FileUtil, Forms, Graphics, StdCtrls, SysUtils,
  BGRASpriteAnimation, BGRABitmap, BGRABitmapTypes;

type

  { TfrmAnimation }

  TfrmAnimation = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    test:   TBGRASpriteAnimation;
    lbLap:  TLabel;
    lbPos:  TLabel;
    test1: TBGRASpriteAnimation;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure test1LapChanged(Sender: TObject);
    procedure testLapChanged(Sender: TObject);
    procedure testPositionChanged(Sender: TObject);
    procedure testRedrawAfter(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
  public
    { public declarations }
    n: integer;
  end;

var
  frmAnimation: TfrmAnimation;

implementation

{$R *.lfm}

procedure TfrmAnimation.testLapChanged(Sender: TObject);
begin
  Label1.Caption := 'Lap ' + IntToStr(test.AnimRepeatLap);
end;

procedure TfrmAnimation.FormShow(Sender: TObject);
begin
  test1.AnimStatic := False;
  test.AnimStatic := False;
end;

procedure TfrmAnimation.test1LapChanged(Sender: TObject);
begin
  case test1.SpriteFillOpacity of
    0: n := +5;
    255: n := -5;
  end;

  test1.SpriteFillOpacity := test1.SpriteFillOpacity + n;
end;

procedure TfrmAnimation.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  test1.AnimStatic := True;
  test.AnimStatic := True;
end;

procedure TfrmAnimation.FormCreate(Sender: TObject);
begin
  
end;

procedure TfrmAnimation.testPositionChanged(Sender: TObject);
begin
  Label2.Caption := 'Pos ' + IntToStr(test.AnimPosition);
end;

procedure TfrmAnimation.testRedrawAfter(Sender: TObject; Bitmap: TBGRABitmap);
begin
  Bitmap.FontHeight := 10;
  Bitmap.TextOut(0, 0, IntToStr(test.AnimRepeatLap), BGRABlack, taLeftJustify);
  Bitmap.TextOut(0, 12, IntToStr(test.AnimPosition), BGRABlack, taLeftJustify);
end;

{ TfrmAnimation }

end.
