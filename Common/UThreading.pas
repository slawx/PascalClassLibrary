unit UThreading;

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Contnrs, SyncObjs;

type
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;
  TMethodCall = procedure of object;


  { TVirtualThread }

  TVirtualThread = class
  private
    function GetFreeOnTerminate: Boolean; virtual; abstract;
    function GetPriority: TThreadPriority; virtual; abstract;
    function GetSuspended: Boolean; virtual; abstract;
    function GetTerminated: Boolean; virtual; abstract;
    function GetThreadId: Integer; virtual; abstract;
    procedure SetFreeOnTerminate(const AValue: Boolean); virtual; abstract;
    procedure SetPriority(const AValue: TThreadPriority); virtual; abstract;
    procedure SetSuspended(const AValue: Boolean); virtual; abstract;
    procedure SetTerminated(const AValue: Boolean); virtual; abstract;
  public
    Name: string;
    procedure Execute; virtual; abstract;
    procedure Resume; virtual; abstract;
    procedure Suspend; virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Terminate; virtual; abstract;
    procedure Synchronize(AMethod: TThreadMethod); virtual; abstract;
    procedure WaitFor; virtual; abstract;
    procedure Sleep(Delay: Integer); virtual; abstract;
    property FreeOnTerminate: Boolean read GetFreeOnTerminate
      write SetFreeOnTerminate;
    property Suspended: Boolean read GetSuspended
      write SetSuspended;
    property Priority: TThreadPriority read GetPriority write SetPriority;
    property Terminated: Boolean read GetTerminated write SetTerminated;
    property ThreadId: Integer read GetThreadId;
  end;

  TListedThread = class;

  { TListedThreadExecute }

  TListedThreadExecute = class(TThread)
    Parent: TListedThread;
    procedure Execute; override;
  end;

  { TListedThread }

  TListedThread = class(TVirtualThread)
  private
    FTerminated: Boolean;
    FThread: TListedThreadExecute;
    function GetFreeOnTerminate: Boolean; override;
    function GetPriority: TThreadPriority; override;
    function GetSuspended: Boolean; override;
    function GetTerminated: Boolean; override;
    function GetThreadId: Integer; override;
    procedure SetFreeOnTerminate(const AValue: Boolean); override;
    procedure SetPriority(const AValue: TThreadPriority); override;
    procedure SetSuspended(const AValue: Boolean); override;
    procedure SetTerminated(const AValue: Boolean); override;
  public
    constructor Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Sleep(Delay: Integer); override;
    procedure Execute; override;
    procedure Resume; override;
    procedure Suspend; override;
    procedure Start; override;
    procedure Terminate; override;
    procedure Synchronize(AMethod: TThreadMethod); override;
    procedure WaitFor; override;
  end;

  TTermThreadState = (ttsReady, ttsRunning, ttsFinished, ttsExceptionOccured);

  { TTermThread }

  TTermThread = class(TListedThread)
  private
  public
    State: TTermThreadState;
    ExceptionMessage: string;
    Method: TMethodCall;
    procedure Execute; override;
  end;

  { TThreadList }

  TThreadList = class(TObjectList)
    function FindById(Id: Integer): TVirtualThread;
    constructor Create;
  end;

var
  ThreadList: TThreadList;
  ThreadListLock: TCriticalSection;
  OnException: TExceptionEvent;

procedure RunInThread(Method: TMethodCall);
procedure Synchronize(Method: TMethodCall);

resourcestring
  SCurrentThreadNotFound = 'Current thread ID %d not found in virtual thread list.';


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
    while (Thread.State = ttsRunning) or (Thread.State = ttsReady) do begin
      if MainThreadID = ThreadID then Application.ProcessMessages;
      Sleep(1);
    end;
    if Thread.State = ttsExceptionOccured then
      raise Exception.Create(Thread.ExceptionMessage);
  finally
    Thread.Free;
  end;
end;

procedure Synchronize(Method: TMethodCall);
var
  I: Integer;
  Thread: TVirtualThread;
begin
  if MainThreadID = ThreadID then Method
  else begin
    Thread := ThreadList.FindById(ThreadID);
    if Assigned(Thread) then begin
      Thread.Synchronize(Method);
    end else raise Exception.Create(Format(SCurrentThreadNotFound, [ThreadID]));
  end;
end;

{ TVirtualThread }


{ TThreadList }

function TThreadList.FindById(Id: Integer): TVirtualThread;
var
  I: Integer;
begin
  I := 0;
  while (I < ThreadList.Count) and (TVirtualThread(ThreadList[I]).ThreadID <> Id) do
    Inc(I);
  if I < ThreadList.Count then Result := TVirtualThread(ThreadList[I])
    else Result := nil;
end;

constructor TThreadList.Create;
begin
  inherited Create;
  OwnsObjects := False;
end;

{ TListedThreadExecute }

procedure TListedThreadExecute.Execute;
begin
  try
    Parent.Execute;
  except
    on E: Exception do
      if Assigned(OnException) then
        OnException(Parent.FThread, E);
  end;
                                                                                                                                                                                                                                                                                                                                    end;

{ TVirtualThread }


{ TListedThread }

function TListedThread.GetFreeOnTerminate: Boolean;
begin
  Result := FThread.FreeOnTerminate;
end;

function TListedThread.GetPriority: TThreadPriority;
begin
  Result := FThread.Priority;
end;

function TListedThread.GetSuspended: Boolean;
begin
  Result := FThread.Suspended;
end;

function TListedThread.GetTerminated: Boolean;
begin
  Result := FTerminated;
end;

function TListedThread.GetThreadId: Integer;
begin
  Result := FThread.ThreadID;
end;

procedure TListedThread.SetFreeOnTerminate(const AValue: Boolean);
begin
  FThread.FreeOnTerminate := AValue;
end;

procedure TListedThread.SetPriority(const AValue: TThreadPriority);
begin
  FThread.Priority := AValue;
end;

procedure TListedThread.SetSuspended(const AValue: Boolean);
begin
  FThread.Suspended := AValue;
end;

procedure TListedThread.SetTerminated(const AValue: Boolean);
begin
  FTerminated := AValue;
  if AValue then FThread.Terminate;
end;

constructor TListedThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  FThread := TListedThreadExecute.Create(True, StackSize);
  FThread.Parent := Self;
  try
    ThreadListLock.Acquire;
    ThreadList.Add(Self);
  finally
    ThreadListLock.Release;
  end;
  if CreateSuspended = False then FThread.Start;
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
  FThread.Free;
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

procedure TListedThread.Execute;
begin
end;

procedure TListedThread.Resume;
begin
  FThread.Resume;
end;

procedure TListedThread.Suspend;
begin
  FThread.Suspend;
end;

procedure TListedThread.Start;
begin
  FThread.Start;
end;

procedure TListedThread.Terminate;
begin
  FTerminated := True;
  FThread.Terminate;
end;

procedure TListedThread.Synchronize(AMethod: TThreadMethod);
begin
  FThread.Synchronize(FThread, AMethod);
end;

procedure TListedThread.WaitFor;
begin
  FThread.WaitFor;
end;

{ TTermThread }

procedure TTermThread.Execute;
begin
  try
    State := ttsRunning;
    Method;
    State := ttsFinished;
  except
    on E: Exception do
      if Assigned(OnException) then begin
        OnException(FThread, E);
        ExceptionMessage := E.Message;
        State := ttsExceptionOccured;
      end;
  end;
end;

initialization

ThreadListLock := TCriticalSection.Create;
ThreadList := TThreadList.Create;

finalization

ThreadList.Free;
ThreadListLock.Free;

end.

