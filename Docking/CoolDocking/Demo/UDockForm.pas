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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  DockForm: TDockForm;

implementation

{$R *.lfm}

end.

