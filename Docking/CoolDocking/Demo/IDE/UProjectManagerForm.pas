unit UProjectManagerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, UCoolDocking;

type

  { TProjectManagerForm }

  TProjectManagerForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ProjectManagerForm: TProjectManagerForm;

implementation

{$R *.lfm}

end.

