unit UThreading;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Contnrs, SyncObjs;

type
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TMethodCall = procedure of object;

  { TListedThread }

  TListedThread = class(TThread)
    Name: string;
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Sleep(Delay: Integer);
    property Terminated;
  end;

  { TTermThread }

  TTermThread = class(TListedThread)
  private
  public
    Finished: Boolean;
    Method: TMethodCall;
    procedure Execute; override;
  end;

var
  ThreadList: TObjectList; // TList<TListedThread>
  ThreadListLock: TCriticalSection;
  OnException: TExceptionEvent;

procedure RunInThread(Method: TMethodCall);
procedure Synchronize(Method: TMethodCall);

resourcestring
  SCurrentThreadNotFound = 'Current thread ID %d not found in list.';


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
      if MainThreadID = ThreadID then Application.ProcessMessages;
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
    end else raise Exception.Create(Format(SCurrentThreadNotFound, [ThreadID]));
  end;
end;

{ TListedThread }

constructor TListedThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  try
    ThreadListLock.Acquire;
    ThreadList.Add(Self);
  finally
    ThreadListLock.Release;
  end;
end;

destructor TListedThread.Destroy;
begin
  if not Suspended then
  begin
    Terminate;
    WaitFor;
  end;
  try
    ThreadListLock.Acquire;
    ThreadList.Delete(ThreadList.IndexOf(Self));
  finally
    ThreadListLock.Release;
  end;
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
  try
    Method;
    Finished := True;
  except
    on E: Exception do
      if Assigned(OnException) then
        OnException(Self, E);
  end;
end;

initialization

ThreadListLock := TCriticalSection.Create;
ThreadList := TObjectList.Create;
ThreadList.OwnsObjects := False;

finalization

ThreadList.Free;
ThreadListLock.Free;

end.

