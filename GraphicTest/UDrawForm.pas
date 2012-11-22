unit UDrawForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  LCLType;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    EraseBackgroundEnabled: Boolean;
    procedure EraseBackground(DC: HDC); override;
  end;

var
  DrawForm: TDrawForm;

implementation

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

procedure TDrawForm.EraseBackground(DC: HDC);
begin
  if EraseBackgroundEnabled then
    inherited EraseBackground(DC);
end;

initialization
  {$I UDrawForm.lrs}

end.

