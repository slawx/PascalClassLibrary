unit UThreading;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Contnrs;

type

  TMethodCall = procedure of object;

  { TTermThread }

  TTermThread = class(TThread)
    Finished: Boolean;
    Method: TMethodCall;
    procedure Execute; override;
  end;

  { TListedThread }

  TListedThread = class(TThread)
    Name: string;
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Sleep(Delay: Integer);
  end;

var
  ThreadList: TObjectList; // TListedThread

procedure RunInThread(Method: TMethodCall);
procedure Synchronize(Method: TMethodCall);


implementation

procedure RunInThread(Method: TMethodCall);
var
  Thread: TTermThread;
begin
  try
    Thread := TTermThread.Create(True);
    Thread.FreeOnTerminate := False;
    Thread.Resume;
    Thread.Method := Method;
    while not Thread.Finished do begin
      Application.ProcessMessages;
      Sleep(1);
    end;
  finally
    Thread.Free;
  end;
end;

procedure Synchronize(Method: TMethodCall);
var
  I: Integer;
  Thread: TListedThread;
begin
  if MainThreadID = ThreadID then Method
  else begin
    I := 0;
    while (I < ThreadList.Count) and (TListedThread(ThreadList[I]).ThreadID <> ThreadID) do
      Inc(I);
    if I < ThreadList.Count then begin
      Thread := TListedThread(ThreadList[I]);
      TThread.Synchronize(Thread, Method);
    end else raise Exception.Create(Format('Current thread ID %d not found in list.', [ThreadID]));
  end;
end;

{ TListedThread }

constructor TListedThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  ThreadList.Add(Self);
end;

destructor TListedThread.Destroy;
begin
  ThreadList.Delete(ThreadList.IndexOf(Self));
  inherited Destroy;
end;

procedure TListedThread.Sleep(Delay: Integer);
const
  Quantum = 20;
var
  I: Integer;
begin
  SysUtils.Sleep(Delay mod Quantum);
  for I := 1 to (Delay div Quantum) do begin
    if Terminated then Break;
    SysUtils.Sleep(Quantum);
  end;
end;

{ TTermThread }

procedure TTermThread.Execute;
begin
  Method;
  Finished := True;
end;

initialization

ThreadList := TObjectList.Create;
ThreadList.OwnsObjects := False;

finalization

ThreadList.Free;

end.

