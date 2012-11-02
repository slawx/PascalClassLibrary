unit UFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, UUpdateChecker, DateUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonCheck: TButton;
    DateEdit1: TDateEdit;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    UpdateChecker1: TUpdateChecker;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCheckClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure UpdateChecker1Terminate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.ButtonCheckClick(Sender: TObject);
begin
  Memo1.Lines.SaveToFile(UpdateChecker1.VersionInfoURL);
  UpdateChecker1.Check(DateEdit1.Date);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Memo1.Lines.SaveToFile(UpdateChecker1.VersionInfoURL);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  DateEdit1.Date := EncodeDate(2012, 11, 1);
  Memo1.Lines.LoadFromFile(UpdateChecker1.VersionInfoURL);
end;

procedure TForm1.UpdateChecker1Terminate(Sender: TObject);
begin
  Application.Terminate;
end;

end.

