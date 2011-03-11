unit ULocalVariablesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCDClient;

type

  { TLocalVariablesForm }

  TLocalVariablesForm = class(TForm)
  published
    Memo1: TMemo;
    CoolDockClient1: TCDClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  LocalVariablesForm: TLocalVariablesForm;

implementation

{$R *.lfm}

end.

