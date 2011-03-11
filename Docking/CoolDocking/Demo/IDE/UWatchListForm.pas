unit UWatchListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UCoolDockClient;

type

  { TWatchListForm }

  TWatchListForm = class(TForm)
  published
    Memo1: TMemo;
    CoolDockClient1: TCoolDockClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  WatchListForm: TWatchListForm;

implementation

{$R *.lfm}

end.

