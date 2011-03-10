program IDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UObjectInspectorForm, CoolDocking, UStructureForm, 
UToolPaletteForm, UProjectManagerForm, UMessagesForm, UCallStackForm, 
ULocalVariablesForm, UToDoListForm, UWatchListForm, UThreadStatusForm, 
USourceEditorForm, UComponentTree
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TObjectInspectorForm, ObjectInspectorForm);
  Application.CreateForm(TStructureForm, StructureForm);
  Application.CreateForm(TToolPaletteForm, ToolPaletteForm);
  Application.CreateForm(TProjectManagerForm, ProjectManagerForm);
  Application.CreateForm(TMessagesForm, MessagesForm);
  Application.CreateForm(TCallStackForm, CallStackForm);
  Application.CreateForm(TLocalVariablesForm, LocalVariablesForm);
  Application.CreateForm(TToDoListForm, ToDoListForm);
  Application.CreateForm(TWatchListForm, WatchListForm);
  Application.CreateForm(TThreadStatusForm, ThreadStatusForm);
  Application.CreateForm(TSourceEditorForm, SourceEditorForm);
  Application.CreateForm(TComponentTree, ComponentTree);
  Application.Run;
end.

