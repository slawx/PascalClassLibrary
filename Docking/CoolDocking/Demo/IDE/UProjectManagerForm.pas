unit UProjectManagerForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  UCoolDocking;

type

  { TProjectManagerForm }

  TProjectManagerForm = class(TForm)
  published
    CoolDockClient1: TCoolDockClient;
    TreeView1: TTreeView;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ProjectManagerForm: TProjectManagerForm;

implementation

{$R *.lfm}

end.

