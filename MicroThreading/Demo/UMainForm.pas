unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Spin, UMicroThreading, Coroutine, DateUtils, UPlatform;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
  public
    Iterations: Integer;
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
  Label6.Caption := IntToStr(GetLogicalProcessorCount);
end;

procedure TMainForm.ButtonSchedulerStartStopClick(Sender: TObject);
var
  I: Integer;
begin
  if ButtonSchedulerStartStop.Caption = 'Start scheduler' then begin
    ButtonSchedulerStartStop.Caption := 'Stop scheduler';
    Scheduler.ThreadPoolSize := SpinEdit2.Value;
    Scheduler.Active := True;
  end else begin
    ButtonSchedulerStartStop.Caption := 'Start scheduler';
    Scheduler.Active := False;
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

procedure TMainForm.CheckBoxUseMainThreadChange(Sender: TObject);
begin
  Scheduler.UseMainThread := CheckBoxUseMainThread.Checked;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Scheduler.Active := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Scheduler.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
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
      Item.SubItems.Add(IntToStr(StackUsed));
    end;
  finally
    Scheduler.MicroThreadsLock.Release;
  end;
end;

procedure TMainForm.SpinEdit2Change(Sender: TObject);
begin
  Scheduler.ThreadPoolSize := SpinEdit2.Value;
end;

procedure TMainForm.SpinEdit3Change(Sender: TObject);
begin
  Iterations := SpinEdit3.Value;
end;

procedure TMainForm.TimerRedrawTimer(Sender: TObject);
begin
  if ListView1.Items.Count <> Scheduler.MicroThreadCount then
    ListView1.Items.Count := Scheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;
  Label2.Caption := DateTimeToStr(NowPrecise) + ' ' +
    FloatToStr(Frac(NowPrecise / OneSecond));
  Label9.Caption := IntToStr(Scheduler.ThreadPoolCount);
  Label10.Caption := IntToStr(Scheduler.MicroThreadCount);
end;

procedure TMainForm.TimerSchedulerStartTimer(Sender: TObject);
begin
  TimerSchedulerStart.Enabled := False;
  ButtonAddWorkers.Click;
  ButtonSchedulerStartStop.Click;
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
      //Memo1.Lines.Add(IntToStr(Id) + ': ' + IntToStr(I) + ' ' +
      //  FloatToStr(ExecutionTime));
      Completion := I / Iterations;
      //Sleep(1 * Id * OneMillisecond);
      Yield;
    end;
  end;
end;

end.

