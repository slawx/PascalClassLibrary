unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, UCoolDocking, UToolPaletteForm, UObjectInspectorForm,
  UProjectManagerForm, UStructureForm;

type

  { TMainForm }

  TMainForm = class(TForm)
  published
    AViewStructure: TAction;
    AViewToolPalette: TAction;
    AViewProjectManager: TAction;
    AViewObjectInspector: TAction;
    AViewWindowList: TAction;
    CoolDockMaster1: TCoolDockMaster;
    CoolDockWindowList1: TCoolDockWindowList;
    MenuItem11: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem2: TMenuItem;
    ActionList1: TActionList;
    MenuItem1: TMenuItem;
    MainMenu1: TMainMenu;
    procedure AViewObjectInspectorExecute(Sender: TObject);
    procedure AViewProjectManagerExecute(Sender: TObject);
    procedure AViewStructureExecute(Sender: TObject);
    procedure AViewToolPaletteExecute(Sender: TObject);
    procedure AViewWindowListExecute(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AViewToolPaletteExecute(Sender: TObject);
begin
  ToolPaletteForm.Show;
end;

procedure TMainForm.AViewWindowListExecute(Sender: TObject);
begin
  CoolDockWindowList1.Execute;
end;

procedure TMainForm.AViewProjectManagerExecute(Sender: TObject);
begin
  ProjectManagerForm.Show;
end;

procedure TMainForm.AViewStructureExecute(Sender: TObject);
begin
  StructureForm.Show;
end;

procedure TMainForm.AViewObjectInspectorExecute(Sender: TObject);
begin
  ObjectInspectorForm.Show;
end;

end.

