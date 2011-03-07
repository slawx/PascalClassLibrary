program IDE;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMainForm, UObjectInspectorForm, CoolDocking, UStructureForm, 
UToolPaletteForm, UProjectManagerForm
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
  Application.Run;
end.

