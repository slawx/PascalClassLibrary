unit UDrawForm;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TDrawForm }

  TDrawForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  DrawForm: TDrawForm;

implementation

{ TDrawForm }

procedure TDrawForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
end;

initialization
  {$I UDrawForm.lrs}

end.

