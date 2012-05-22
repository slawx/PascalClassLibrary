unit UJobProgressView;

{$MODE Delphi}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Syncobjs,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Contnrs, UThreading,
  DateUtils;

const
  EstimatedTimeShowTreshold = 4;
  EstimatedTimeShowTresholdTotal = 1;
  MemoLogHeight = 200;
  UpdateInterval = 100; // ms

type

  { TProgress }

  TProgress = class
  private
    FLock: TCriticalSection;
    FOnChange: TNotifyEvent;
    FValue: Integer;
    FMax: Integer;
    procedure SetMax(const AValue: Integer);
    procedure SetValue(const AValue: Integer);
  public
    procedure Increment;
    procedure Reset;
    constructor Create;
    destructor Destroy; override;
    property Value: Integer read FValue write SetValue;
    property Max: Integer read FMax write SetMax;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TJobProgressView = class;
  TJobThread = class;
  TJob = class;

  TJobProgressViewMethod = procedure of object;

  { TJob }

  TJob = class
  private
    FResultString: string;
    FTerminate: Boolean;
    procedure SetTerminate(const AValue: Boolean);
  public
    StartTime: TDateTime;
    EndTime: TDateTime;
    ProgressView: TJobProgressView;
    Title: string;
    Method: TJobProgressViewMethod;
    NoThreaded: Boolean;
    WaitFor: Boolean;
    Progress: TProgress;
    Thread: TJobThread;
    ResultString: string;
    Finished: Boolean;
    procedure AddLogItem(Value: string);
    constructor Create;
    destructor Destroy; override;
    property Terminate: Boolean read FTerminate write SetTerminate;
  end;

  TJobThread = class(TListedThread)
    procedure Execute; override;
  public
    ProgressView: TJobProgressView;
    Job: TJob;
  end;

  { TJobProgressView }

  TJobProgressView = class(TForm)
    ImageList1: TImageList;
    Label2: TLabel;
    LabelOperation: TLabel;
    LabelEstimatedTimePart: TLabel;
    LabelEstimatedTimeTotal: TLabel;
    ListViewJobs: TListView;
    MemoLog: TMemo;
    PanelProgressTotal: TPanel;
    PanelOperationsTitle: TPanel;
    PanelLog: TPanel;
    PanelOperations: TPanel;
    PanelProgress: TPanel;
    ProgressBarPart: TProgressBar;
    ProgressBarTotal: TProgressBar;
    TimerUpdate: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewJobsData(Sender: TObject; Item: TListItem);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    Finished: Boolean;
    FOnJobFinish: TJobProgressViewMethod;
    FTerminate: Boolean;
    FormList: TList;
    Log: TStringList;
    procedure SetTerminate(const AValue: Boolean);
    procedure UpdateProgress;
    procedure ReloadJobList;
    procedure StartJobs;
    procedure UpdateHeight;
  public
    TotalStartTime: TDateTime;
    Jobs: TObjectList; // TListObject<TJob>
    CurrentJob: TJob;
    CurrentJobIndex: Integer;
    ShowDelay: Integer;
    AutoClose: Boolean;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddJob(Title: string; Method: TJobProgressViewMethod;
      NoThreaded: Boolean = False; WaitFor: Boolean = False);
    procedure Start(AAutoClose: Boolean = True);
    procedure Stop;
    procedure TermSleep(Delay: Integer);
    property Terminate: Boolean read FTerminate write SetTerminate;
    property OnJobFinish: TJobProgressViewMethod read FOnJobFinish
      write FOnJobFinish;
  end;

  var
    JobProgressView: TJobProgressView;

resourcestring
  SExecuted = 'Executed';

implementation

{$R *.lfm}

resourcestring
  SPleaseWait = 'Please wait...';
  STerminate = 'Termination';
  SEstimatedTime = 'Estimated time: %s';
  STotalEstimatedTime = 'Total estimated time: %s';
  SFinished = 'Finished';

procedure Register;
begin
  RegisterComponents('Samples', [TJobProgressView]);
end;


procedure TJobThread.Execute;
begin
  try
    try
      //raise Exception.Create('Exception in job');
      JobProgressView.CurrentJob.Method;
    except
      on E: Exception do begin
        ProgressView.Terminate := True;
        raise;
      end;
    end;
  finally
    Terminate;
  end;
end;

procedure TJobProgressView.AddJob(Title: string; Method: TJobProgressViewMethod;
  NoThreaded: Boolean = False; WaitFor: Boolean = False);
var
  NewJob: TJob;
begin
  NewJob := TJob.Create;
  NewJob.ProgressView := Self;
  NewJob.Title := Title;
  NewJob.Method := Method;
  NewJob.NoThreaded := NoThreaded;
  NewJob.WaitFor := WaitFor;
  NewJob.Progress.Max := 100;
  NewJob.Progress.Reset;
  Jobs.Add(NewJob);
  //ReloadJobList;
end;

procedure TJobProgressView.Start(AAutoClose: Boolean = True);
begin
  AutoClose := AAutoClose;
  StartJobs;
end;

procedure TJobProgressView.StartJobs;
var
  I: Integer;
  H: Integer;
begin
  Terminate := False;

  BringToFront;

  Finished := False;
  Caption := SPleaseWait;
  try
    FormList := Screen.DisableForms(Self);
    Log.Clear;
    MemoLog.Clear;

    LabelEstimatedTimePart.Visible := False;
    LabelEstimatedTimeTotal.Visible := False;

    CurrentJob := nil;
    if ShowDelay = 0 then begin
      TimerUpdate.Interval := UpdateInterval;
      TimerUpdate.Enabled := True;
      TimerUpdateTimer(Self);
    end else begin
      TimerUpdate.Interval := ShowDelay;
      TimerUpdate.Enabled := True;
    end;

    TotalStartTime := Now;
    ProgressBarTotal.Position := 0;
    ProgressBarTotal.Visible := False;
    //UpdateHeight;

    I := 0;
    while I < Jobs.Count do
    with TJob(Jobs[I]) do begin
      CurrentJobIndex := I;
      CurrentJob := TJob(Jobs[I]);
      StartTime := Now;
      LabelEstimatedTimePart.Caption := Format(SEstimatedTime, ['']);
      ProgressBarPart.Position := 0;
      ProgressBarPart.Visible := False;
      //Show;
      ReloadJobList;
      Application.ProcessMessages;
      if NoThreaded then begin
        Thread := nil;
        Method;
      end else begin
        try
          Thread := TJobThread.Create(True);
          with Thread do begin
            FreeOnTerminate := False;
            Job := CurrentJob;
            Name := 'Job: ' + Job.Title;
            ProgressView := Self;
            Start;
            while not Terminated do begin
              Application.ProcessMessages;
              Sleep(1);
            end;
            WaitFor;
          end;
        finally
          FreeAndNil(Thread);
        end;
      end;
      ProgressBarPart.Hide;
      if Terminate then Break;
      EndTime := Now;
      Finished := True;
      if Assigned(FOnJobFinish) then
        FOnJobFinish;
      Inc(I);
    end;
  finally
    CurrentJob := nil;
    TimerUpdate.Enabled := False;
    Screen.EnableForms(FormList);
    //if Visible then Hide;
    MemoLog.Lines.Assign(Log);
    if (MemoLog.Lines.Count = 0) and AutoClose then begin
      Hide;
    end;
    Clear;
    Caption := SFinished;
    //LabelEstimatedTimePart.Visible := False;
    Finished := True;
    CurrentJobIndex := -1;
    ReloadJobList;
  end;
end;

procedure TJobProgressView.UpdateHeight;
var
  H: Integer;
  PanelOperationsVisible: Boolean;
  PanelOperationsHeight: Integer;
  PanelProgressVisible: Boolean;
  PanelProgressTotalVisible: Boolean;
  PanelLogVisible: Boolean;
begin
  H := PanelOperationsTitle.Height;
  PanelOperationsVisible := Jobs.Count > 0;
  if PanelOperationsVisible <> PanelOperations.Visible then
    PanelOperations.Visible := PanelOperationsVisible;
  PanelOperationsHeight := 8 + 18 * Jobs.Count;
  if PanelOperationsHeight <> PanelOperations.Height then
    PanelOperations.Height := PanelOperationsHeight;
  if PanelOperationsVisible then
    H := H + PanelOperations.Height;

  PanelProgressVisible := (Jobs.Count > 0) and not Finished;
  if PanelProgressVisible <> PanelProgress.Visible then
    PanelProgress.Visible := PanelProgressVisible;
  if PanelProgressVisible then
    H := H + PanelProgress.Height;
  PanelProgressTotalVisible := (Jobs.Count > 1) and not Finished;
  if PanelProgressTotalVisible <> PanelProgressTotal.Visible then
    PanelProgressTotal.Visible := PanelProgressTotalVisible;
  if PanelProgressTotalVisible then
    H := H + PanelProgressTotal.Height;
  Constraints.MinHeight := H;
  PanelLogVisible := MemoLog.Lines.Count > 0;
  if PanelLogVisible <> PanelLog.Visible then
    PanelLog.Visible := PanelLogVisible;
  if PanelLogVisible then
    H := H + MemoLogHeight;
  if Height <> H then Height := H;
end;

procedure TJobProgressView.TimerUpdateTimer(Sender: TObject);
var
  ProgressBarPartVisible: Boolean;
  ProgressBarTotalVisible: Boolean;
begin
  UpdateProgress;
  if Visible and (not ProgressBarPart.Visible) and Assigned(CurrentJob) and
  (CurrentJob.Progress.Value > 0) then begin
    ProgressBarPartVisible := True;
    if ProgressBarPartVisible <> ProgressBarPart.Visible then
      ProgressBarPart.Visible := ProgressBarPartVisible;
    ProgressBarTotalVisible := True;
    if ProgressBarTotalVisible <> ProgressBarTotal.Visible then
      ProgressBarTotal.Visible := ProgressBarTotalVisible;
  end;
  if not Visible then begin
    TimerUpdate.Interval := UpdateInterval;
    Show;
  end;
end;

procedure TJobProgressView.FormDestroy(Sender:TObject);
begin
end;

procedure TJobProgressView.ListViewJobsData(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < Jobs.Count) then
  with TJob(Jobs[Item.Index]) do begin
    Item.Caption := Title;
    if Item.Index = CurrentJobIndex then Item.ImageIndex := 1
      else if Finished then Item.ImageIndex := 0
      else Item.ImageIndex := 2;
    Item.Data := Jobs[Item.Index];
  end;
end;

procedure TJobProgressView.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  ListViewJobs.Clear;
end;

procedure TJobProgressView.FormCreate(Sender: TObject);
begin
  Caption := SPleaseWait;
  try
    //Animate1.FileName := ExtractFileDir(UTF8Encode(Application.ExeName)) +
    //  DirectorySeparator + 'horse.avi';
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
  CanClose := Finished;
  Terminate := True;
  Caption := SPleaseWait + STerminate;
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
const
  OneJobValue: Integer = 100;
var
  TotalMax: Integer;
  TotalValue: Integer;
  EstimatedTimePart: TDateTime;
  RemainingTime: TDateTime;
  LabelEstimatedTimePartCaption: string;
  LabelEstimatedTimePartVisible: Boolean;
begin
  if Assigned(CurrentJob) then
  with CurrentJob do begin
    // Part progress
    ProgressBarPart.Max := Progress.Max;
    ProgressBarPart.Position := Progress.Value;
    if (Progress.Value >= EstimatedTimeShowTreshold) then begin
      EstimatedTimePart := (Now - StartTime) / Progress.Value * (Progress.Max - Progress.Value);
      LabelEstimatedTimePart.Caption := Format(SEstimatedTime, [
        TimeToStr(EstimatedTimePart)]);
      LabelEstimatedTimePart.Visible := True;
    end;

    // Total progress
    TotalMax := Jobs.Count * OneJobValue;
    TotalValue := Int64(CurrentJobIndex) * OneJobValue +
      Round(Progress.Value / Progress.Max * OneJobValue);
    ProgressBarTotal.Max := TotalMax;
    ProgressBarTotal.Position := TotalValue;
    if (TotalValue >= EstimatedTimeShowTresholdTotal) then begin
      // Project estimated time according part estimated time plus
      // estimated time by elapsed time divided by elapsed ticks mutiplied by rest ticks
      RemainingTime := EstimatedTimePart +
        (Now - TotalStartTime + EstimatedTimePart) /
        ((CurrentJobIndex + 1) * OneJobValue) *
        ((Jobs.Count - 1 - CurrentJobIndex) * OneJobValue);
      if (RemainingTime > 0) and (RemainingTime < EncodeDate(2100, 1, 1)) then begin
        LabelEstimatedTimeTotal.Caption := Format(STotalEstimatedTime, [
          TimeToStr(RemainingTime)]);
        LabelEstimatedTimeTotal.Visible := True;
      end else begin
        LabelEstimatedTimeTotal.Visible := False;
      end;
    end;
  end;
end;

procedure TJobProgressView.ReloadJobList;
var
  OldImageIndex: Integer;
begin
  UpdateHeight;
  // Workaround for not showing first line
  ListViewJobs.Items.Count := Jobs.Count + 1;
  ListViewJobs.Refresh;

  if ListViewJobs.Items.Count <> Jobs.Count then
    ListViewJobs.Items.Count := Jobs.Count;
  ListViewJobs.Refresh;
  //Application.ProcessMessages;
end;

constructor TJobProgressView.Create(TheOwner: TComponent);
begin
  inherited;
  Jobs := TObjectList.Create;
  Log := TStringList.Create;
  //PanelOperationsTitle.Height := 80;
  ShowDelay := 0; //1000; // ms
end;

procedure TJobProgressView.Clear;
begin
  Jobs.Clear;
  //ReloadJobList;
end;

destructor TJobProgressView.Destroy;
begin
  Log.Free;
  Jobs.Free;
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
    Change := AValue <> FValue;
    FValue := AValue;
    if Change and Assigned(FOnChange) then FOnChange(Self);
  end;
end;

{ TProgress }

procedure TProgress.Increment;
begin
  try
    FLock.Acquire;
    Value := Value + 1;
  finally
    FLock.Release;
  end;
end;

procedure TProgress.Reset;
begin
  try
    FLock.Acquire;
    FValue := 0;
  finally
    FLock.Release;
  end;
end;

constructor TProgress.Create;
begin
  FMax := 100;
  FLock := TCriticalSection.Create;
end;

destructor TProgress.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;

{ TJob }

procedure TJob.SetTerminate(const AValue: Boolean);
begin
  if FTerminate = AValue then Exit;
  FTerminate := AValue;
  if Assigned(Thread) then Thread.Terminate;
end;

procedure TJob.AddLogItem(Value: string);
begin
  with ProgressView do begin
    Log.Add(Value);
  end;
end;

constructor TJob.Create;
begin
  Progress := TProgress.Create;
  Terminate := False;
  Finished := False;
end;

destructor TJob.Destroy;
begin
  Progress.Free;
  inherited Destroy;
end;

end.
