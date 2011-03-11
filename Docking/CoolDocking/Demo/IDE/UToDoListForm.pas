unit UToDoListForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCDClient;

type

  { TToDoListForm }

  TToDoListForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
    ListView1: TListView;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ToDoListForm: TToDoListForm;

implementation

{$R *.lfm}

end.

