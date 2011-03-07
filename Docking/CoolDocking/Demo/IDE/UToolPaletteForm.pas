unit UToolPaletteForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UCoolDocking;

type

  { TToolPaletteForm }

  TToolPaletteForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ToolPaletteForm: TToolPaletteForm;

implementation

{$R *.lfm}

end.

