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
    ListView1: TListView;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure Worker(MicroThread: TMicroThread);
  public
    Scheduler: TMicroThreadScheduler;
    Test: TTest;
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
begin
  for I := 0 to 1 do
    Scheduler.Add('Worker', Worker);
  repeat
    Scheduler.Start;
    Application.ProcessMessages;
    Sleep(1);
  until Scheduler.MicroThreadCount = 0;
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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Test.Free;
  Scheduler.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  I: Integer;
  NewItem: TListItem;
begin
  try
    ListView1.BeginUpdate;
    ListView1.Clear;
    Scheduler.Lock.Acquire;
    for I := 0 to Scheduler.MicroThreads.Count - 1 do
    with TMicroThread(Scheduler.MicroThreads[I]) do begin
      NewItem := ListView1.Items.Add;
      NewItem.Caption := IntToStr(Id);
      NewItem.SubItems.Add(Name);
      NewItem.SubItems.Add('');
      NewItem.SubItems.Add(IntToStr(Priority));
      NewItem.SubItems.Add(MicroThreadStateText[State]);
      NewItem.SubItems.Add(FloatToStr(ExecutionTime));
    end;
  finally
    Scheduler.Lock.Release;
    ListView1.EndUpdate;
  end;
end;

procedure TForm1.Worker(MicroThread: TMicroThread);
var
  I: Integer;
begin
  with MicroThread do begin
    Memo1.Lines.Add('Worker ' + IntToStr(Id));
    for I := 0 to 10 do begin
      Memo1.Lines.Add(InttoStr(Id) + ': ' + IntToStr(I));
      Sleep(100 * Id * OneMillisecond);
    end;
  end;
end;

end.

