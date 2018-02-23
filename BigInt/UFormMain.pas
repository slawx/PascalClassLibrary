unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, UInt128;

type

  { TFormMain }

  TFormMain = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormShow(Sender: TObject);
var
  A, B, C: Int128;
  X: Int64;
begin
  A := 10;
  B := 10;
  C := A + B;
  X := $12345678;
  Memo1.Lines.Add('X: ' + IntToHex(X, 8));
  Memo1.Lines.Add(IntToStr(C));
  Memo1.Lines.Add(IntToHex(C, 0));
end;

end.

