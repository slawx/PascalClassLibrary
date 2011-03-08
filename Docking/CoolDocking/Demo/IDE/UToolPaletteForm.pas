unit UToolPaletteForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCoolDocking;

type

  { TToolPaletteForm }

  TToolPaletteForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    TreeView1: TTreeView;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ToolPaletteForm: TToolPaletteForm;

implementation

{$R *.lfm}

end.

