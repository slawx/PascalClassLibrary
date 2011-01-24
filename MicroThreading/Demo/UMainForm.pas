unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, UMicroThreading, Coroutine, DateUtils;

type

  { TTest }

  TTest = class(TCoroutine)
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Worker(MicroThread: TMicroThread);
  public
    Scheduler: TMicroThreadScheduler;
    Test: TTest;
    Terminate: Boolean;
  end; 

var
  Form1: TForm1; 

implementation

{ TTest }

procedure TTest.Execute;
var
  I: Integer;
begin
//  for I := 0 to 100 do begin
    Form1.Memo1.Lines.Add(IntToStr(I));
      Sleep(10);
      //raise Exception.Create('Test');
      Yield;

//  end;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scheduler := TMicroThreadScheduler.Create;
  Scheduler.FreeMicroThreadOnFinish := False;
  Test := TTest.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Executed: Integer;
begin
  if Button1.Caption = 'Start scheduler' then begin
    Button1.Caption := 'Stop scheduler';
    Terminate := False;
    Scheduler.MicroThreads.Clear;
    Memo1.Clear;
    for I := 0 to 20 do
      Scheduler.Add('Worker', Worker);
    repeat
      Executed := Scheduler.Execute(10);
      Application.ProcessMessages;
      if Executed = 0 then Sleep(1);
    until (Scheduler.MicroThreadCount = 0) or Terminate;
  end else begin
    Button1.Caption := 'Start scheduler';
    Terminate := True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
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

procedure TForm1.Button3Click(Sender: TObject);
begin
  Test.Invoke;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Terminate := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Test.Free;
  Scheduler.Free;
end;

procedure TForm1.ListView1Data(Sender: TObject; Item: TListItem);
begin
  try
    Scheduler.Lock.Acquire;
    if Item.Index < Scheduler.MicroThreads.Count then
    with TMicroThread(Scheduler.MicroThreads[Item.Index]) do begin
      Item.Caption := IntToStr(Id);
      Item.SubItems.Add(Name);
      Item.SubItems.Add('');
      Item.SubItems.Add(IntToStr(Priority));
      Item.SubItems.Add(MicroThreadStateText[State]);
      Item.SubItems.Add(FloatToStr(ExecutionTime));
    end;
  finally
    Scheduler.Lock.Release;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  ListView1.Items.Count := Scheduler.MicroThreadCount;
  ListView1.Items[-1];
  ListView1.Refresh;
  Label2.Caption := DateTimeToStr(Scheduler.GetNow) + ' ' +
    FloatToStr(Frac(Scheduler.GetNow / OneSecond));
end;

procedure TForm1.Worker(MicroThread: TMicroThread);
var
  I: Integer;
begin
  with MicroThread do begin
    Memo1.Lines.Add('Worker ' + IntToStr(Id));
    for I := 0 to 1000 do begin
      Memo1.Lines.Add(IntToStr(Id) + ': ' + IntToStr(I) + ' ' +
        FloatToStr(ExecutionTime));
      //Sleep(1 * Id * OneMillisecond);
      Yield;
    end;
  end;
end;

end.

