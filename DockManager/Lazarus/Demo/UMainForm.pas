unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, UCustomDockManager, UDockForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1EndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ListView1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    FormIndex: Integer;
    LastDockForm: TDockForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Button1Click(Self);
  LastDockForm.ManualDock(Panel1);
end;

procedure TMainForm.ListView1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin

end;

procedure TMainForm.ListView1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin

end;

procedure TMainForm.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  NewForm: TDockForm;
begin
  NewForm := TDockForm.Create(Self);
  NewForm.Name := 'Form' + IntToStr(FormIndex);
  NewForm.Memo1.Text := NewForm.Name;
  NewForm.DragKind := dkDock;
  NewForm.DragMode := dmAutomatic;
  Inc(FormIndex);
  NewForm.Show;
  LastDockForm := NewForm;
end;

end.

