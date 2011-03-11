unit UWatchListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, UCDClient;

type

  { TWatchListForm }

  TWatchListForm = class(TForm)
  published
    Memo1: TMemo;
    CoolDockClient1: TCDClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  WatchListForm: TWatchListForm;

implementation

{$R *.lfm}

end.

