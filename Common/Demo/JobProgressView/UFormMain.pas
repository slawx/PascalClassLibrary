unit UFormMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, UJobProgressView;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonTest: TButton;
    CheckBoxAutoClose: TCheckBox;
    CheckBoxTextProgress: TCheckBox;
    CheckBoxErrors: TCheckBox;
    JobProgressView1: TJobProgressView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SpinEditJobCount: TSpinEdit;
    SpinEditShowDelay: TSpinEdit;
    procedure ButtonTestClick(Sender: TObject);
  private
    procedure JobMethod(Job: TJob);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonTestClick(Sender: TObject);
var
  I: Integer;
begin
  with JobProgressView1 do begin
    AutoClose := CheckBoxAutoClose.Checked;
    ShowDelay := SpinEditShowDelay.Value;

    for I := 0 to SpinEditJobCount.Value - 1 do
      AddJob('Job ' + IntToStr(I + 1), JobMethod);
    Start;
  end;
end;

procedure TForm1.JobMethod(Job: TJob);
var
  Count: Integer;
  I: Integer;
begin
  Count := Random(100);
  Job.Progress.Max := Count;
  for I := 0 to Count - 1 do begin
    Sleep(10);
    Job.Progress.Value := I;
    if CheckBoxErrors.Checked and (Random < 0.1) then
      Job.AddLogItem('Error during execution');
    if CheckBoxTextProgress.Checked then Job.Progress.Text := 'Processing item ' + IntToStr(I);
    if Job.Terminate then Break;
  end;
end;

end.

