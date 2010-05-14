unit UJobProgressView;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls;

const
  EstimatedTimeShowTreshold = 4;

type

  { TProgress }

  TProgress = class
  private
    FOnChange: TNotifyEvent;
    FValue: Integer;
    FMax: Integer;
    procedure SetMax(const AValue: Integer);
    procedure SetValue(const AValue: Integer);
  public
    procedure Increment;
    procedure Reset;
    constructor Create;
    property Value: Integer read FValue write SetValue;
    property Max: Integer read FMax write SetMax;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJobProgressView = class;
  TJobThread = class;
  TJob = class;

  TJobProgressViewMethod = procedure(Job: TJob) of object;

  { TJob }

  TJob = class
    StartTime: TDateTime;
    EndTime: TDateTime;
    ProgressView: TJobProgressView;
    Title: string;
    Method: TJobProgressViewMethod;
    Direct: Boolean;
    WaitFor: Boolean;
    Terminate: Boolean;
    Progress: TProgress;
    Thread: TJobThread;
    constructor Create;
    destructor Destroy; override;
  end;

  TJobThread = class(TThread)
    procedure Execute; override;
  private
    ExceptionText: string;
  public
    ProgressView: TJobProgressView;
    Job: TJob;
  end;

  { TJobProgressView }

  TJobProgressView = class(TForm)
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    ListView1: TListView;
    ImageList1: TImageList;
    Label2: TLabel;
    Timer1: TTimer;
    LabelEstimatedTime: TLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FTerminate: Boolean;
    procedure SetTerminate(const AValue: Boolean);
    //WindowList: Pointer;
    procedure UpdateProgress;
    procedure ReloadJobList;
  public
    Jobs: TList; // of TJob
    CurrentJob: TJob;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddJob(Title: string; Method: TJobProgressViewMethod;
      Direct: Boolean = False; WaitFor: Boolean = False);
    procedure Start;
    procedure Stop;
    procedure TermSleep(Delay: Integer);
    property Terminate: Boolean read FTerminate write SetTerminate;
  end;

implementation

{$R *.lfm}

procedure TJobThread.Execute;
begin
  ExceptionText := '';
  try
    //raise Exception.Create('dsds');
    Job.Method(Self.Job);
    Terminate;
  except
    on E:Exception do begin
      ExceptionText := 'V úloze "' + Job.Title + '" došlo k vyjímce: ' + E.Message;
      Terminate;
    end;
  end;
end;

procedure TJobProgressView.AddJob(Title: string; Method: TJobProgressViewMethod;
  Direct: Boolean = False; WaitFor: Boolean = False);
var
  NewJob: TJob;
begin
  NewJob := TJob.Create;
  NewJob.ProgressView := Self;
  NewJob.Title := Title;
  NewJob.Method := Method;
  NewJob.Direct := Direct;
  NewJob.WaitFor := WaitFor;
  NewJob.Progress.Max := 100;
  NewJob.Progress.Reset;
  Jobs.Add(NewJob);
  ReloadJobList;
end;

procedure TJobProgressView.Start;
var
  LocalExceptionText: string;
  JobThread: TJobThread;
  I: Integer;
begin
  Caption := 'Prosím čekejte...';
  try
    //WindowList := DisableTaskWindows(0);
    Height := 100 + 18 * Jobs.Count;
    //Show;
    Timer1.Enabled := True;
    // Timer1Timer(Self);
    for I := 0 to Jobs.Count - 1 do
    with TJob(Jobs[I]) do begin
      CurrentJob := Jobs[I];
      StartTime := Now;
      ListView1.Items.Item[I].ImageIndex := 1;
      LabelEstimatedTime.Caption := '';
      ProgressBar1.Position := 0;
      Application.ProcessMessages;
      if Direct then Method(CurrentJob) else begin
        JobThread := TJobThread.Create(True);
        with JobThread do begin
          Job := CurrentJob;
          ProgressView := Self;
          Resume;
          while not Terminated do begin
            Application.ProcessMessages;
            Sleep(1);
          end;
          WaitFor;
          LocalExceptionText := ExceptionText;
          Free;
        end;
        if LocalExceptionText <> '' then
          raise Exception.Create(LocalExceptionText);
      end;
      ProgressBar1.Hide;
      ListView1.Items.Item[I].ImageIndex := 0;
      if Terminate then Break;
      EndTime := Now;
    end;
    CurrentJob := nil;
  finally
    Timer1.Enabled := False;
    //EnableTaskWindows(WindowList);
    if Visible then Hide;
  end;
end;

procedure TJobProgressView.Timer1Timer(Sender: TObject);
begin
  UpdateProgress;
  if (not ProgressBar1.Visible) and Assigned(CurrentJob) and
  (CurrentJob.Progress.Value > 0) then
    ProgressBar1.Visible := True;
  if not Visible then Show;
end;

procedure TJobProgressView.FormCreate(Sender: TObject);
begin
  try
    //Animate1.FileName := ExtractFileDir(Application.ExeName) + '\horse.avi';
    //Animate1.Active := True;
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
  Caption := 'Prosím čekejte...přerušení';
end;

procedure TJobProgressView.SetTerminate(const AValue: Boolean);
var
  I: Integer;
begin
  for I := 0 to Jobs.Count - 1 do
    TJob(Jobs[I]).Terminate := AValue;
  FTerminate := AValue;
end;

procedure TJobProgressView.UpdateProgress;
begin
  if Assigned(CurrentJob) then
  with CurrentJob do begin
    ProgressBar1.Max := Progress.Max;
    ProgressBar1.Position := Progress.Value;
    if (Progress.Value >= EstimatedTimeShowTreshold) then
      LabelEstimatedTime.Caption :=
        TimeToStr((Now - StartTime) / Progress.Value * (Progress.Max - Progress.Value));
  end;
end;

procedure TJobProgressView.ReloadJobList;
var
  NewItem: TListItem;
  I: Integer;
begin
  with ListView1, Items do begin
    BeginUpdate;
    Clear;
    for I := 0 to Jobs.Count - 1 do
    with TJob(Jobs[I]) do begin
      NewItem := Add;
      with NewItem do begin
        Caption := Title;
        ImageIndex := 2;
        Data := Jobs[I];
      end;
    end;
    EndUpdate;
  end;
end;

constructor TJobProgressView.Create(TheOwner: TComponent);
begin
  inherited;
  Jobs := TList.Create;
end;

procedure TJobProgressView.Clear;
var
  I: Integer;
begin
  for I := 0 to Jobs.Count - 1 do
    TJob(Jobs[I]).Destroy;
  Jobs.Clear;
  ReloadJobList;
end;

destructor TJobProgressView.Destroy;
begin
  Clear;
  Jobs.Destroy;
  inherited Destroy;
end;

procedure TProgress.SetMax(const AValue: Integer);
begin
  FMax := AValue;
  if FValue >= FMax then FValue := FMax;
end;

procedure TProgress.SetValue(const AValue: Integer);
var
  Change: Boolean;
begin
  if AValue < Max then begin
    change := AValue <> FValue;
    FValue := AValue;
    if Change and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TProgress }

procedure TProgress.Increment;
begin
  Value := Value + 1;
end;

procedure TProgress.Reset;
begin
  FValue := 0;
end;

constructor TProgress.Create;
begin
  FMax := 100;
end;

{ TJob }

constructor TJob.Create;
begin
  Progress := TProgress.Create;
  Terminate := False;
end;

destructor TJob.Destroy;
begin
  Progress.Destroy;
  inherited Destroy;
end;

end.
