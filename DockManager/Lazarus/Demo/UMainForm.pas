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
  private
    { private declarations }
  public
    FormIndex: Integer;
    LastDockForm: TDockForm;
    function NewDockForm: TDockForm;
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
var
  DockForm1: TDockForm;
  DockForm2: TDockForm;
  DockForm3: TDockForm;
begin
  DockForm1 := NewDockForm;
  DockForm1.ManualDock(Panel1);

  DockForm2 := NewDockForm;
  DockForm2.ManualDock(Panel1);

  DockForm3 := NewDockForm;
  DockForm3.ManualDock(DockForm2);
end;

function TMainForm.NewDockForm: TDockForm;
begin
  Result := TDockForm.Create(Self);
  Result.Name := 'Form' + IntToStr(FormIndex);
  Result.Memo1.Text := Result.Name;
  Result.DragKind := dkDock;
  Result.DragMode := dmAutomatic;
  Result.DockSite := True;
  Result.UseDockManager := True;
  Inc(FormIndex);
  Result.Show;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  NewDockForm;
end;

end.

