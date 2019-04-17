unit UFormDraw;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType;

type

  { TFormDraw }

  TFormDraw = class(TForm)
    procedure FormCreate(Sender: TObject);
  private

  public
    FrameSize: TPoint;
    EraseBackgroundEnabled: Boolean;
    procedure EraseBackground(DC: HDC); override;
  end;

var
  FormDraw: TFormDraw;

implementation

{ TFormDraw }

procedure TFormDraw.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

procedure TFormDraw.EraseBackground(DC: HDC);
begin
  if EraseBackgroundEnabled then
    inherited EraseBackground(DC);
end;

initialization
  {$I UFormDraw.lrs}

end.

