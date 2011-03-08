unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ActnList, ExtCtrls, ComCtrls, StdCtrls, UCoolDocking, UCoolDockLayout,
  UToolPaletteForm, UObjectInspectorForm, UProjectManagerForm, UStructureForm,
  UMessagesForm, UCallStackForm, ULocalVariablesForm, UToDoListForm,
  UWatchListForm, UThreadStatusForm, USourceEditorForm;

type

  { TMainForm }

  TMainForm = class(TForm)
  published
    ANewFile: TAction;
    ACustomizeDocking: TAction;
    ADesktopSave: TAction;
    AViewThreadStatus: TAction;
    AViewWatchList: TAction;
    AViewToDoList: TAction;
    AViewLocalVariables: TAction;
    AViewCallStack: TAction;
    AViewMessages: TAction;
    AViewStructure: TAction;
    AViewToolPalette: TAction;
    AViewProjectManager: TAction;
    AViewObjectInspector: TAction;
    AViewWindowList: TAction;
    ComboBox1: TComboBox;
    CoolDockClient1: TCoolDockClient;
    CoolDockCustomize1: TCoolDockCustomize;
    CoolDockLayoutList1: TCoolDockLayoutList;
    CoolDockMaster1: TCoolDockMaster;
    CoolDockWindowList1: TCoolDockWindowList;
    ImageList1: TImageList;
    MenuItem11: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
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
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure ACustomizeDockingExecute(Sender: TObject);
    procedure ADesktopSaveExecute(Sender: TObject);
    procedure ANewFileExecute(Sender: TObject);
    procedure AViewThreadStatusExecute(Sender: TObject);
    procedure AViewCallStackExecute(Sender: TObject);
    procedure AViewLocalVariablesExecute(Sender: TObject);
    procedure AViewMessagesExecute(Sender: TObject);
    procedure AViewObjectInspectorExecute(Sender: TObject);
    procedure AViewProjectManagerExecute(Sender: TObject);
    procedure AViewStructureExecute(Sender: TObject);
    procedure AViewToDoListExecute(Sender: TObject);
    procedure AViewToolPaletteExecute(Sender: TObject);
    procedure AViewWatchListExecute(Sender: TObject);
    procedure AViewWindowListExecute(Sender: TObject);
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

procedure TMainForm.AViewWatchListExecute(Sender: TObject);
begin
  WatchListForm.Show;
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

procedure TMainForm.AViewToDoListExecute(Sender: TObject);
begin
  ToDoListForm.Show;
end;

procedure TMainForm.AViewObjectInspectorExecute(Sender: TObject);
begin
  ObjectInspectorForm.Show;
end;

procedure TMainForm.AViewCallStackExecute(Sender: TObject);
begin
  CallStackForm.Show;
end;

procedure TMainForm.AViewThreadStatusExecute(Sender: TObject);
begin
  ThreadStatusForm.Show;
end;

procedure TMainForm.ADesktopSaveExecute(Sender: TObject);
begin

end;

procedure TMainForm.ANewFileExecute(Sender: TObject);
begin
  SourceEditorForm.Show;
end;

procedure TMainForm.ACustomizeDockingExecute(Sender: TObject);
begin
  CoolDockCustomize1.Execute;
end;

procedure TMainForm.AViewLocalVariablesExecute(Sender: TObject);
begin
  LocalVariablesForm.Show;
end;

procedure TMainForm.AViewMessagesExecute(Sender: TObject);
begin
  MessagesForm.Show;
end;

end.

