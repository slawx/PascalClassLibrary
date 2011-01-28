unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, UMicroThreading, DateUtils, UPlatform;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonAddWorkers: TButton;
    ButtonClearMicroThreads: TButton;
    ButtonGetMaxThread: TButton;
    ButtonSchedulerStartStop: TButton;
    ButtonShowThreadId: TButton;
    CheckBoxUseMainThread: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    PageControl1: TPageControl;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TimerRedraw: TTimer;
    TimerSchedulerStart: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure ButtonSchedulerStartStopClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ButtonAddWorkersClick(Sender: TObject);
    procedure ButtonGetMaxThreadClick(Sender: TObject);
    procedure ButtonShowThreadIdClick(Sender: TObject);
    procedure ButtonClearMicroThreadsClick(Sender: TObject);
    procedure CheckBoxUseMainThreadChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure SpinEdit2Change(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure TimerRedrawTimer(Sender: TObject);
    procedure TimerSchedulerStartTimer(Sender: TObject);
  private
    procedure Worker(MicroThread: TMicroThread);
    procedure WorkerDoWrite;
    procedure WorkerSubRoutine;
  public
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
  ListView1.DoubleBuffered := True;
  Label6.Caption := IntToStr(GetLogicalProcessorCount);
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
  for I := 0 to SpinEdit1.Value - 1 do
    MainScheduler.AddMethod(Worker);
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

procedure TMainForm.CheckBoxUseMainThreadChange(Sender: TObject);
begin
  MainScheduler.UseMainThread := CheckBoxUseMainThread.Checked;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  MainScheduler.Active := False;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
end;

procedure TMainForm.ListView1Data(Sender: TObject; Item: TListItem);
begin
  try
    MainScheduler.MicroThreadsLock.Acquire;
    if Item.Index < MainScheduler.MicroThreads.Count then
    with TMicroThread(MainScheduler.MicroThreads[Item.Index]) do begin
      Item.Caption := IntToStr(Id);
      Item.SubItems.Add(Name);
      Item.SubItems.Add('');
      Item.SubItems.Add(IntToStr(Priority));
      Item.SubItems.Add(MicroThreadStateText[State]);
      Item.SubItems.Add(MicroThreadBlockStateText[BlockState]);
      Item.SubItems.Add(FloatToStr(ExecutionTime));
      Item.SubItems.Add(IntToStr(Trunc(Completion * 100)) + '%');
      Item.SubItems.Add(IntToStr(StackUsed));
    end;
  finally
    MainScheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  MainScheduler.ThreadPoolSize := SpinEdit2.Value;
end;

procedure TMainForm.SpinEdit3Change(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
end;

procedure TMainForm.TimerRedrawTimer(Sender: TObject);
begin
  if ListView1.Items.Count <> MainScheduler.MicroThreadCount then
    ListView1.Items.Count := MainScheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;
  Label2.Caption := DateTimeToStr(NowPrecise) + ' ' +
    FloatToStr(Frac(NowPrecise / OneSecond));
  Label9.Caption := IntToStr(MainScheduler.ThreadPoolCount);
  Label10.Caption := IntToStr(MainScheduler.MicroThreadCount);
end;

procedure TMainForm.TimerSchedulerStartTimer(Sender: TObject);
begin
  TimerSchedulerStart.Enabled := False;
  ButtonAddWorkers.Click;
  ButtonSchedulerStartStop.Click;
end;

procedure TMainForm.WorkerSubRoutine;
begin
  MTSleep(1 * OneMillisecond);
end;

procedure TMainForm.Worker(MicroThread: TMicroThread);
var
  I: Integer;
  Q: Integer;
begin
  with MicroThread do begin
    //Memo1.Lines.Add('Worker ' + IntToStr(Id));
    for I := 0 to Iterations - 1 do begin
      Q := 0;
      while Q < 100 do Inc(Q);
      Synchronize(WorkerDoWrite);
      //Memo1.Lines.Add(IntToStr(Id) + ': ' + IntToStr(I) + ' ' +
      //  FloatToStr(ExecutionTime));
      Completion := I / Iterations;
      //MTSleep(1 * Id * OneMillisecond);
      Yield;
      WorkerSubRoutine;
    end;
  end;
end;

procedure TMainForm.WorkerDoWrite;
begin
  Memo1.Lines.Add('.');
end;

end.

