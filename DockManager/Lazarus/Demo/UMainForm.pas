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
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    FormIndex: Integer;
    DockForms: TList;
    function NewDockForm: TDockForm;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DockForms := TList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DockForms.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  DockForm1: TDockForm;
  DockForm2: TDockForm;
  DockForm3: TDockForm;
begin
  NewDockForm.ManualDock(Panel1);
  NewDockForm.ManualDock(Panel1, nil, alBottom);
  NewDockForm.ManualDock(TForm(DockForms[1]).Parent);
  NewDockForm.ManualDock(TForm(DockForms[1]).Parent);
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
  DockForms.Add(Result);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  NewDockForm;
end;

end.

