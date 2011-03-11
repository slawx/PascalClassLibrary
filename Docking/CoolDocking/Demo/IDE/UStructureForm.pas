unit UStructureForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCDClient;

type

  { TStructureForm }

  TStructureForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
    TreeView1: TTreeView;
    { private declarations }
  public
    { public declarations }
  end; 

var
  StructureForm: TStructureForm;

implementation

{$R *.lfm}

end.

