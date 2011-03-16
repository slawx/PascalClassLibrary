unit UCallStackForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCDClient;

type

  { TCallStackForm }

  TCallStackForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
    ListView1: TListView;
    { private declarations }
  public
    { public declarations }
  end; 

var
  CallStackForm: TCallStackForm;

implementation

{$R *.lfm}

end.

