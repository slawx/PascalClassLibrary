unit UDockForm; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCoolDocking;

type

  { TDockForm }

  TDockForm = class(TForm)
    CoolDockClient1: TCoolDockClient;
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockForm: TDockForm;

implementation

{$R *.lfm}

{ TDockForm }

procedure TDockForm.FormDestroy(Sender: TObject);
var
  Temp: string;
begin
  Temp := Name;
end;

end.

