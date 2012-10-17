unit ULogForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TLogForm }

  TLogForm = class(TForm)
    Memo1: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LogForm: TLogForm;

implementation

{$R *.lfm}

end.

