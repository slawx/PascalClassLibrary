unit UObjectInspectorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  PopupNotifier, Grids, UCDClient;

type

  { TObjectInspectorForm }

  TObjectInspectorForm = class(TForm)
  published
    CoolDockClient1: TCDClient;
    StringGrid1: TStringGrid;
    { private declarations }
  public
    { public declarations }
  end; 

var
  ObjectInspectorForm: TObjectInspectorForm;

implementation

{$R *.lfm}

end.

