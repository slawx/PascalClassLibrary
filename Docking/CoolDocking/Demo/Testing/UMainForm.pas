unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Buttons, Menus, UCDMaster, UCDCustomize, UCDClient,
  UDockForm, UComponentTree, UCDWindowList, UCDConjoinForm, UCDManager,
  UCDCommon;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CoolDockClient1: TCDClient;
    CoolDockCustomize1: TCDCustomize;
    CoolDockMaster1: TCDMaster;
    CoolDockWindowList1: TCDWindowList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabControl1: TTabControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
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
  ConjoinedDockForm1: TCDConjoinForm;
  ConjoinedDockForm2: TCDConjoinForm;
begin
  NewDockForm.ManualDock(Panel1);
  NewDockForm.ManualDock(Panel1);
  ConjoinedDockForm1 := TCDManager(Panel1.DockManager).CreateConjoinForm;
  ConjoinedDockForm1.Name := 'Model';;
  //TCoolDockManager(ConjoinedDockForm1.Panel.DockManager).TabsPos := hpLeft;
  ConjoinedDockForm1.ManualDock(Panel1);
  TCDManager(ConjoinedDockForm1.DockManager).DockStyle := dsTabs;
  NewDockForm.ManualDock(ConjoinedDockForm1);
  NewDockForm.ManualDock(ConjoinedDockForm1);
  NewDockForm.ManualDock(ConjoinedDockForm1);
  //NewDockForm.ManualDock(TForm(DockForms[0]));
  //NewDockForm.ManualDock(TForm(DockForms[0]));
  //NewDockForm.ManualDock(TForm(DockForms[0]));
  //TCustomDockManager(TDockForm(DockForms[0]).DockManager).DockStyle := dsTabs;
end;

procedure TMainForm.MenuItem2Click(Sender: TObject);
begin
  CoolDockCustomize1.Execute;
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  CoolDockWindowList1.Execute;
end;

procedure TMainForm.PageControl1Change(Sender: TObject);
begin

end;

function TMainForm.NewDockForm: TDockForm;
begin
  Application.CreateForm(TDockForm, Result);
//  Result := TDockForm.Create(Self);
  Result.Name := 'Form' + IntToStr(FormIndex);
  Result.CoolDockClient1.Name := 'CoolDockClient' + IntToStr(FormIndex);
  Result.Caption := Result.Name;
  Result.Memo1.Text := Result.Name;
  //Result.DragKind := dkDock;
  //Result.DragMode := dmAutomatic;
  //Result.DockSite := True;
  //Result.UseDockManager := True;
  Inc(FormIndex);
  Result.Show;
  DockForms.Add(Result);
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  NewDockForm;
end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  ComponentTree.Show;
end;

end.

