unit UJobProgressView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, StdCtrls, ExtCtrls;

type
  TMethod = procedure(Thread: TThread) of object;

  TJobThread = class(TThread)
    procedure Execute; override;
  public
    ExceptionText: string;
    Index: Integer;
    Title: string;
    Method: TMethod;
  end;

  TJobProgressView = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    ListView1: TListView;
    ImageList1: TImageList;
    Label2: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    Animate1: TAnimate;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    //FLastProgress: Real;
    FProgress: Real;
    FPosition: Integer;
    Jobs: array of record
      Method: TMethod;
      Direct: Boolean;
    end;
    Job: TJobThread;
    WindowList: Pointer;
    StartTime: TDateTime;
    procedure SetProgress(Value: Real);
    procedure UpdateProgress;
  public
    Terminate: Boolean;
    procedure AddJob(Title: string; Method: TMethod; Direct: Boolean = False);
    procedure Start;
    procedure Stop;
    procedure TermSleep(Delay: Integer);
    property Progress: Real read FProgress write SetProgress;
  end;

var
  JobProgressView: TJobProgressView;

implementation

{$R *.dfm}

procedure TJobThread.Execute;
begin
  ExceptionText := '';
  try
    //raise Exception.Create('dsds');
    Method(Self);
    Terminate;
  except
    on E:Exception do begin
      ExceptionText := 'V úloze "' + Title + '" došlo k vyjímce: ' + E.Message;
      Terminate;
    end;
  end;
end;

procedure TJobProgressView.AddJob(Title: string; Method: TMethod; Direct: Boolean = False);
var
  NewItem: TListItem;
begin
  with ListView1, Items do begin
    BeginUpdate;
    NewItem := Add;
    with NewItem do begin
      Caption := Title;
      ImageIndex := 2;
    end;
    SetLength(Jobs, Length(Jobs) + 1);
    Jobs[High(Jobs)].Method := Method;
    Jobs[High(Jobs)].Direct := Direct;
    EndUpdate;
  end;
end;

procedure TJobProgressView.Start;
var
  I: Integer;
  LocalExceptionText: string;
begin
  Caption := 'Prosím èekejte...';
  try
  Terminate := False;
  WindowList := DisableTaskWindows(0);
  Height := 100 + 18 * ListView1.Items.Count;
  //Show;
  Timer1.Enabled := True;
//  Timer1Timer(Self);
  for I := 0 to High(Jobs) do begin
    StartTime := Now;
    ListView1.Items.Item[I].ImageIndex := 1;
    Label3.Caption := '';
    ProgressBar1.Position := 0;
    FPosition := 0;
    FProgress := 0;
    Application.ProcessMessages;
    if Jobs[I].Direct then Jobs[I].Method(nil) else begin
      Job := TJobThread.Create(True);
      with Job do begin
        Method := Jobs[I].Method;
        Title := ListView1.Items.Item[I].Caption;
        Index := I;
        Resume;
        while not Terminated do begin
          Application.ProcessMessages;
          Sleep(1);
        end;
        WaitFor;
        LocalExceptionText := ExceptionText;
        Free;
      end;
      if LocalExceptionText <> '' then raise Exception.Create(LocalExceptionText);
    end;
    ProgressBar1.Hide;
    ListView1.Items.Item[I].ImageIndex := 0;
    if Terminate then Break;
  end;
  finally
    Timer1.Enabled := False;
    EnableTaskWindows(WindowList);
    if Visible then begin
      JobProgressView.Hide;
    end;
    SetLength(Jobs, 0);
    with ListView1.Items do begin
      BeginUpdate;
      Clear;
      EndUpdate;
    end;
  end;
end;

procedure TJobProgressView.Timer1Timer(Sender: TObject);
begin
  UpdateProgress;
  if (not ProgressBar1.Visible) and (FProgress > 0) then ProgressBar1.Visible := True;
  if not Visible then Show;
end;

procedure TJobProgressView.FormCreate(Sender: TObject);
begin
  try
    Animate1.FileName := ExtractFileDir(Application.ExeName) + '\horse.avi';
    Animate1.Active := True;
  except

  end;
end;

procedure TJobProgressView.Stop;
begin
  Terminate := True;
end;

procedure TJobProgressView.TermSleep(Delay: Integer);
const
  Quantum = 100;
var
  I: Integer;
begin
  Sleep(Delay mod Quantum);
  for I := 1 to (Delay div Quantum) do begin
    if Terminate then Break;
    Sleep(Quantum);
  end;
end;

procedure TJobProgressView.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Terminate;
  Terminate := True;
  Caption := 'Prosím èekejte...pøerušení';
end;

procedure TJobProgressView.SetProgress(Value: Real);
begin
  if (Value * 100) > FPosition then begin
    FPosition := Trunc(Value * 100) + 1;
    UpdateProgress;
  end;
  FProgress := Value;
end;

procedure TJobProgressView.UpdateProgress;
begin
  ProgressBar1.Position := FPosition;
  if (FPosition > 4) and (FProgress > 0) then
    Label3.Caption := TimeToStr((Now - StartTime) / FProgress * (1 - FProgress));
end;

end.
