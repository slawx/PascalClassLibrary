unit UToolPaletteForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCDClient;

type

  { TToolPaletteForm }

  TToolPaletteForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
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

