unit UStructureForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UCoolDocking;

type

  { TStructureForm }

  TStructureForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  StructureForm: TStructureForm;

implementation

{$R *.lfm}

end.

