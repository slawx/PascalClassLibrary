unit UStructureForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCoolDockClient;

type

  { TStructureForm }

  TStructureForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
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

