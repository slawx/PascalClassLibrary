unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, UMicroThreading, Coroutine, DateUtils;

type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonSchedulerStartStop: TButton;
    Button2: TButton;
    ButtonAddWorkers: TButton;
    ButtonGetMaxThread: TButton;
    ButtonShowThreadId: TButton;
    ButtonClearMicroThreads: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonSchedulerStartStopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonAddWorkersClick(Sender: TObject);
    procedure ButtonGetMaxThreadClick(Sender: TObject);
    procedure ButtonShowThreadIdClick(Sender: TObject);
    procedure ButtonClearMicroThreadsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure SpinEdit2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Worker(MicroThread: TMicroThread);
  public
    Scheduler: TMicroThreadScheduler;
  end;

var
  MainForm: TMainForm;

implementation

{ TTest }

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Scheduler := TMicroThreadScheduler.Create;
  DoubleBuffered := True;
  ListView1.DoubleBuffered := True;
  Label6.Caption := IntToStr(Scheduler.GetCPUCoreCount);
end;

procedure TMainForm.ButtonSchedulerStartStopClick(Sender: TObject);
var
  I: Integer;
begin
  if ButtonSchedulerStartStop.Caption = 'Start scheduler' then begin
    ButtonSchedulerStartStop.Caption := 'Stop scheduler';
    Memo1.Clear;
    Scheduler.Active := True;
  end else begin
    ButtonSchedulerStartStop.Caption := 'Start scheduler';
    Scheduler.Active := False;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
const
  MaxBlock = MaxInt - $f;
type
  PBytes = ^TBytes;
  TBytes = array[0..MaxBlock div SizeOf(Byte)] of Byte;
  PDWORDS = ^TDWORDS;
  TDWORDS = array[0..MaxBlock div SizeOf(DWORD)] of DWORD;
  PStackFrame = ^TStackFrame;
  TStackFrame = record
    CallersEBP: PStackFrame;
    CallerAdr: DWORD;
  end;
  TStackInfo = record
    CallerAdr: DWORD;
    Level: DWORD;
    CallersEBP: DWORD;
    DumpSize: DWORD;
    ParamSize: DWORD;
    ParamPtr: PDWORDS;
    case integer of
      0: (StackFrame: PStackFrame);
      1: (DumpPtr: PBytes);
  end;
  PStackInfo = ^TStackInfo;
var
  I: Integer;
  FrameAddr: PStackFrame;
  FrameAddr2: PStackFrame;
begin
(*  for I := 0 to 100 do begin
    Memo1.Lines.Add(IntToStr(I));
    Sleep(10);
    Application.ProcessMessages;
  end;
  *)
  FrameAddr := get_frame;
  Memo1.Lines.Add('get_frame: ' + IntToHex(Integer(FrameAddr), 8));
  Memo1.Lines.Add('get_caller_addr: ' + IntToHex(Integer(get_caller_addr(get_frame)), 8));
  Memo1.Lines.Add('get_caller_frame: ' + IntToHex(Integer(get_caller_frame(get_frame)), 8));
  Memo1.Lines.Add(IntToHex(Integer(FrameAddr^.CallersEBP), 8));
  Memo1.Lines.Add(IntToHex(FrameAddr^.CallerAdr, 8));

  for I := 0 to 10 do begin
    Memo1.Lines.Add('Stack frame ' + IntToStr(I));
    Memo1.Lines.Add(IntToHex(Integer(FrameAddr^.CallersEBP), 8));
    Memo1.Lines.Add(IntToHex(FrameAddr^.CallerAdr, 8));
    Memo1.Lines.Add('Size: ' + IntToStr(Integer(FrameAddr^.CallersEBP) - Integer(FrameAddr)));
    FrameAddr := FrameAddr^.CallersEBP;
  end;
  //FrameAddr^.CallerAdr := FrameAddr2^.CallerAdr;
end;

procedure TMainForm.ButtonAddWorkersClick(Sender: TObject);
var
  I: Integer;
begin
  //Scheduler.FMicroThreads.Clear;
  for I := 0 to SpinEdit1.Value do
    Scheduler.AddMethod(Worker);
end;

procedure TMainForm.ButtonGetMaxThreadClick(Sender: TObject);
var
  NewThread: TThread;
  I: Integer;
begin
  try
    I := 0;
    while True do begin
      NewThread := TThread.Create(True);
      NewThread.FreeOnTerminate:= False;
      Inc(I);
    end;
  except
    ShowMessage('Application can create ' + IntToStr(I) +' TThread instances');
  end;
end;

procedure TMainForm.ButtonShowThreadIdClick(Sender: TObject);
begin
  ShowMessage(IntToStr(GetThreadID));
end;

procedure TMainForm.ButtonClearMicroThreadsClick(Sender: TObject);
begin
  try
    Scheduler.MicroThreadsLock.Acquire;
    Scheduler.MicroThreads.Clear;
  finally
    Scheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Scheduler.Free;
end;

procedure TMainForm.ListView1Data(Sender: TObject; Item: TListItem);
begin
  try
    Scheduler.MicroThreadsLock.Acquire;
    if Item.Index < Scheduler.MicroThreads.Count then
    with TMicroThread(Scheduler.MicroThreads[Item.Index]) do begin
      Item.Caption := IntToStr(Id);
      Item.SubItems.Add(Name);
      Item.SubItems.Add('');
      Item.SubItems.Add(IntToStr(Priority));
      Item.SubItems.Add(MicroThreadStateText[State]);
      Item.SubItems.Add(FloatToStr(ExecutionTime));
      Item.SubItems.Add(IntToStr(Trunc(Completion * 100)) + '%');
    end;
  finally
    Scheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  Scheduler.ThreadPoolSize := SpinEdit2.Value;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  ListView1.Items.Count := Scheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;
  Label2.Caption := DateTimeToStr(Scheduler.GetNow) + ' ' +
    FloatToStr(Frac(Scheduler.GetNow / OneSecond));
end;

procedure TMainForm.Worker(MicroThread: TMicroThread);
var
  I: Integer;
  Q: Integer;
const
  TotalSteps = 100;
begin
  with MicroThread do begin
    Memo1.Lines.Add('Worker ' + IntToStr(Id));
    for I := 0 to TotalSteps - 1 do begin
      Q := 0;
      while Q < 1000000 do Inc(Q);
      //Memo1.Lines.Add(IntToStr(Id) + ': ' + IntToStr(I) + ' ' +
      //  FloatToStr(ExecutionTime));
      Completion := I / TotalSteps;
      //Sleep(1 * Id * OneMillisecond);
      Yield;
    end;
  end;
end;

end.

