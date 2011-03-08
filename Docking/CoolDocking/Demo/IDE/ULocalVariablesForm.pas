unit ULocalVariablesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCoolDocking;

type

  { TLocalVariablesForm }

  TLocalVariablesForm = class(TForm)
  published
    Memo1: TMemo;
    CoolDockClient1: TCoolDockClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  LocalVariablesForm: TLocalVariablesForm;

implementation

{$R *.lfm}

end.

