unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, UMicroThreading, DateUtils, UPlatform,
  UMicroThreadList, UThreadEx;

type
  TMainForm = class;

  { TWorker }

  TWorker = class(TMicroThread)
    procedure Execute; override;
  private
    MainForm: TMainForm;
    procedure DoWriteToMemo;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ButtonAddWorkers: TButton;
    ButtonClearMicroThreads: TButton;
    ButtonGetMaxThread: TButton;
    ButtonSchedulerStartStop: TButton;
    ButtonShowThreadId: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBoxUseMainThread: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TimerRedraw: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ButtonSchedulerStartStopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonAddWorkersClick(Sender: TObject);
    procedure ButtonGetMaxThreadClick(Sender: TObject);
    procedure ButtonShowThreadIdClick(Sender: TObject);
    procedure ButtonClearMicroThreadsClick(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure CheckBox4Change(Sender: TObject);
    procedure CheckBoxUseMainThreadChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView2Data(Sender: TObject; Item: TListItem);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure SpinEdit6Change(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
  private
    MicroThreadList: TMicroThreadList;
    Lock: TMicroThreadCriticalSection;
    LastException: Exception;
    LastExceptionSender: TObject;
    procedure WorkerSubRoutine;
    procedure ShowException(Sender: TObject; E: Exception);
    procedure DoShowException;
  public
    DoWriteToMemo: Boolean;
    DoSleep: Boolean;
    DoCriticalSection: Boolean;
    RaiseException: Boolean;
    SleepDuration: Integer;
    CriticalSectionSleepDuration: Integer;
    DoWaitForEvent: Boolean;
    Event: TMicroThreadEvent;
    WaitForEventDuration: Integer;
    Iterations: Integer;
  end;

var
  MainForm: TMainForm;

implementation

{ TTest }

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
//  ListView1.DoubleBuffered := True;
  Label6.Caption := IntToStr(GetLogicalProcessorCount);
  Event := TMicroThreadEvent.Create;
  MicroThreadList := TMicroThreadList.Create(Self);
  UMicroThreading.ExceptionHandler := ShowException;
  Lock := TMicroThreadCriticalSection.Create;
end;

procedure TMainForm.ButtonSchedulerStartStopClick(Sender: TObject);
var
  I: Integer;
begin
  if ButtonSchedulerStartStop.Caption = 'Start scheduler' then begin
    ButtonSchedulerStartStop.Caption := 'Stop scheduler';
    MainScheduler.ThreadPoolSize := SpinEdit2.Value;
    MainScheduler.Active := True;
  end else begin
    ButtonSchedulerStartStop.Caption := 'Start scheduler';
    MainScheduler.Active := False;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  Event.SetEvent;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  MicroThreadList.Form.Show;
end;

procedure TMainForm.Button5Click(Sender: TObject);
begin
  RaiseException := True;
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
  NewWorker: TWorker;
begin
  //Scheduler.FMicroThreads.Clear;
  for I := 0 to SpinEdit1.Value - 1 do begin
    NewWorker := TWorker.Create(True);
    NewWorker.MainForm := Self;
    NewWorker.Start;
  end;
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
    MainScheduler.MicroThreadsLock.Acquire;
    MainScheduler.MicroThreads.Clear;
  finally
    MainScheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  SleepDuration := SpinEdit4.Value;
  DoSleep := CheckBox1.Checked;
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
begin
  DoWriteToMemo := CheckBox2.Checked;
end;

procedure TMainForm.CheckBox3Change(Sender: TObject);
begin
  DoWaitForEvent := CheckBox3.Checked;
  WaitForEventDuration := SpinEdit5.Value;
end;

procedure TMainForm.CheckBox4Change(Sender: TObject);
begin
  CriticalSectionSleepDuration := SpinEdit4.Value;
  DoCriticalSection := CheckBox4.Checked;
end;

procedure TMainForm.CheckBoxUseMainThreadChange(Sender: TObject);
begin
  MainScheduler.UseMainThread := CheckBoxUseMainThread.Checked;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MicroThreadList.Free;
  MainScheduler.Active := False;
  Event.Free;
  Lock.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
  SpinEdit2.Value := 6;
  ButtonAddWorkers.Click;
  ButtonSchedulerStartStop.Click;
  Label16.Caption := IntToStr(MainThreadID);
end;

procedure TMainForm.ListView2Data(Sender: TObject; Item: TListItem);
begin
end;

procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  MainScheduler.ThreadPoolSize := SpinEdit2.Value;
end;

procedure TMainForm.SpinEdit3Change(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
end;

procedure TMainForm.SpinEdit5Change(Sender: TObject);
begin

end;

procedure TMainForm.SpinEdit6Change(Sender: TObject);
begin
end;

procedure TMainForm.TimerRedrawTimer(Sender: TObject);
begin
  Label2.Caption := DateTimeToStr(NowPrecise) + ' ' +
    FloatToStr(Frac(NowPrecise / OneSecond));
  Label9.Caption := IntToStr(MainScheduler.ThreadPoolCount);
  Label10.Caption := IntToStr(MainScheduler.MicroThreadCount);
end;

procedure TMainForm.WorkerSubRoutine;
begin
  //MTSleep(1 * OneMillisecond);
end;

procedure TMainForm.ShowException(Sender: TObject; E: Exception);
begin
  LastException := E;
  LastExceptionSender := Sender;
  if MainThreadID <> ThreadID then
    TThread.Synchronize(TThreadEx.CurrentThread, DoShowException)
    else DoShowException;
end;

procedure TMainForm.DoShowException;
begin
  ShowMessage('Exception "' + LastException.Message + '" in class "' +
    LastExceptionSender.ClassName + '"')
end;

procedure TWorker.Execute;
var
  I: Integer;
  Q: Integer;
begin
  for I := 0 to MainForm.Iterations - 1 do begin
    Q := 0;
    while Q < 100000 do Inc(Q);
    if MainForm.DoWriteToMemo then Synchronize(DoWriteToMemo);
    if MainForm.DoWaitForEvent then MainForm.Event.WaitFor(MainForm.WaitForEventDuration * OneMillisecond);
    if MainForm.DoSleep then MTSleep(MainForm.SleepDuration * OneMillisecond);
    if MainForm.RaiseException then begin
      MainForm.RaiseException := False;
      raise Exception.Create('Exception from microthread');
    end;
    if MainForm.DoCriticalSection then begin
      try
        MainForm.Lock.Acquire;
        MTSleep(MainForm.CriticalSectionSleepDuration * OneMillisecond);
      finally
        MainForm.Lock.Release;
      end;
    end;
    //WorkerSubRoutine;
    Completion := I / MainForm.Iterations;
    Yield;
  end;
end;

procedure TWorker.DoWriteToMemo;
begin
  MainForm.Memo1.Lines.Add(IntToStr(Id) + ': ' + IntToStr(Trunc(Completion * 100)) + ' %');
end;

constructor TWorker.Create(CreateSuspended: Boolean; const StackSize: SizeUInt);
begin
  inherited;
end;

destructor TWorker.Destroy;
begin
  inherited Destroy;
end;

end.

