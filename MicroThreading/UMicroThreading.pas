unit UMicroThreading;

{$mode Delphi}{$H+}
{$asmmode intel}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF}
  {$IFDEF Linux}BaseUnix, UnixUtil, Unix,{$ENDIF}
  Classes, SysUtils, Contnrs, SyncObjs, DateUtils, Dialogs, Forms;

const
  DefaultStackSize = $4000;

type
  TMicroThread = class;
  TMicroThreadScheduler = class;
  TMicroThreadManager = class;

  TMicroThreadState = (tsWaiting, tsRunning, tsBlocked, tsSuspended,
    tsSleeping);

  { TMicroThread }

  TMicroThread = class
  private
    FFreeOnTerminate: Boolean;
    FExecutionStartTime: TDateTime;
    FExecutionEndTime: TDateTime;
    FExecutionTime: TDateTime;
    FStack: Pointer;
    FStackPointer: Pointer;
    FStackSize: Integer;
    FBasePointer: Pointer;
    FWakeupTime: TDateTime;
    FTerminated: Boolean;
    FExecuted: Boolean; // At first go through Execute method, then switch context
    FFinished: Boolean;
  public
    Id: Integer;
    Name: string;
    Priority: Integer;
    State: TMicroThreadState;
    Manager: TMicroThreadManager;
    Scheduler: TMicroThreadScheduler;
    Completion: Single; // Can be used for progress information in range <0, 1>
    procedure Execute; virtual;

    // Internal execution
    procedure Yield;
    procedure Sleep(Duration: TDateTime);
    function WaitForSignal(Signal: TEvent): TWaitResult;

    // External execution
    procedure WaitFor;
    procedure Terminate;
    procedure Start;
    procedure Stop;

    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property ExecutionTime: TDateTime read FExecutionTime;
    property FreeOnTerminate: Boolean read FFreeOnTerminate
      write FFreeOnTerminate;
    property Terminated: Boolean read FTerminated;
  end;

  TMicroThreadEvent = procedure(MicroThread: TMicroThread) of object;

  { TMicroThreadMethod }

  TMicroThreadMethod = class(TMicroThread)
    Method: TMicroThreadEvent;
    procedure Execute; override;
  end;

  { TMicroThreadSchedulerPoolThread }

  TMicroThreadSchedulerPoolThread = class(TThread)
    Scheduler: TMicroThreadScheduler;
    Manager: TMicroThreadManager;
    procedure Execute; override;
  end;

  TThreadPool = class(TObjectList)

  end;

  { TMicroThreadManager }

  TMicroThreadManager = class
  private
    FStack: Pointer;
    FStackSize: Pointer;
    FStackPointer: Pointer;
    FBasePointer: Pointer;
    FExecuteCount: Integer;
    FExecutedCount: Integer;
    FTerminated: Boolean;
    FTempPointer: Pointer;
    function Execute(Count: Integer): Integer;
  public
    Scheduler: TMicroThreadScheduler;
    CurrentMicroThread: TMicroThread;
    procedure Yield;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    ThreadPool: TThreadPool;
    RoundRobinIndex: Integer;
    LastId: Integer;
    FFrequency: Int64;
    FThreadPoolSize: Integer;
    FTerminated: Boolean;
    function GetMicroThreadCount: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const AValue: Integer);
    function GetNextMicroThread: TMicroThread;
  public
    MainThreadManager: TMicroThreadManager;
    MicroThreads: TObjectList; // TList<TMicroThread>
    Lock: TCriticalSection;
    function GetNow: TDateTime;
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadEvent): Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property MicroThreadCount: Integer read GetMicroThreadCount;
    property ThreadPoolSize: Integer read GetThreadPoolSize
      write SetThreadPoolSize;
  end;

var
  MainScheduler: TMicroThreadScheduler;

const
  MicroThreadStateText: array[TMicroThreadState] of string = ('Waiting',
    'Running', 'Blocked', 'Suspended', 'Sleeping');

implementation

var
  StaticManagers: TObjectList; // TList<TMicroThreadManager>;
  StaticManager: TMicroThreadManager;
  StaticMicroThread: TMicroThread;

function GetMicroThreadId: Integer;
var
  I: Integer;
  CurrentStack: Pointer;
begin
  asm
    mov CurrentStack, sp
  end;
  with MainScheduler do begin
    try
      Lock.Acquire;
      I := 0;
      while (I < MicroThreads.Count) and
        not ((CurrentStack >= TMicroThread(MicroThreads[I]).FStack) and
        (CurrentStack <= (TMicroThread(MicroThreads[I]).FStack +
        TMicroThread(MicroThreads[I]).FStackSize))) do Inc(I);
      if I < MicroThreads.Count then begin
        Result := TMicroThread(MicroThreads[I]).Id;
      end else Result := -1;
    finally
      Lock.Release;
    end;
  end;
end;

{ TMicroThreadManager }

function TMicroThreadManager.Execute(Count: Integer): Integer;
begin
  FStack := StackBottom;
  FStackSize := StackBottom + StackLength;
  FExecuteCount := Count;
  FExecutedCount := 0;
  Yield;
  Result := FExecutedCount;
end;

procedure TMicroThreadManager.Yield;
var
  I: Integer;
  Time: TDateTime;
begin
  Time := Scheduler.GetNow;
  if Assigned(CurrentMicroThread) then begin
    CurrentMicroThread.FExecutionEndTime := Time;
    CurrentMicroThread.FExecutionTime := CurrentMicroThread.FExecutionTime +
      (CurrentMicroThread.FExecutionEndTime - CurrentMicroThread.FExecutionStartTime);
    if CurrentMicroThread.State = tsRunning then
      CurrentMicroThread.State := tsWaiting;
    StaticMicroThread := CurrentMicroThread;
    asm
      // Store microthread stack
      mov eax, StaticMicroThread
      mov edx, esp
      mov [eax].TMicroThread.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThread.FBasePointer, edx
    end;
    StaticManager := CurrentMicroThread.Manager;
    asm
      // Restore scheduler stack
      mov eax, StaticManager  // Self is invalid before BP restore
      mov edx, [eax].TMicroThreadManager.FStackPointer
      mov esp, edx
      mov edx, [eax].TMicroThreadManager.FBasePointer
      mov ebp, edx
    end;
    CurrentMicroThread.Manager := nil;
    CurrentMicroThread := nil;
  end;

  CurrentMicroThread := Scheduler.GetNextMicroThread;

  if Assigned(CurrentMicroThread) and (FExecutedCount < FExecuteCount) then begin
    CurrentMicroThread.Manager := Self;
    Inc(FExecutedCount);
    asm
      // Store scheduler stack
      mov eax, Self
      mov edx, esp
      mov [eax].TMicroThreadManager.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThreadManager.FBasePointer, edx
    end;
    if not CurrentMicroThread.FExecuted then begin
      CurrentMicroThread.FExecuted := True;
      CurrentMicroThread.State := tsRunning;
      CurrentMicroThread.FExecutionStartTime := Time;
      StaticMicroThread := CurrentMicroThread;
      asm
        // Restore microthread stack
        mov eax, StaticMicroThread
        mov edx, [eax].TMicroThread.FStackPointer
        mov esp, edx
        push ebp
        mov edx, [eax].TMicroThread.FBasePointer
        mov ebp, edx
      end;
      StaticMicroThread.Execute;
      asm
        pop ebp
      end;
      //FSelected.Method(FSelected);
      StaticManager := CurrentMicroThread.Manager;
      asm
        // Restore scheduler stack
        mov eax, StaticManager // Self is invalid before BP restore
        mov edx, [eax].TMicroThreadManager.FStackPointer
        mov esp, edx
        mov edx, [eax].TMicroThreadManager.FBasePointer
        mov ebp, edx
      end;
      CurrentMicroThread.Manager := nil;
      CurrentMicroThread.FExecutionEndTime := Time;
      CurrentMicroThread.FExecutionTime := CurrentMicroThread.FExecutionTime +
       (CurrentMicroThread.FExecutionEndTime - CurrentMicroThread.FExecutionStartTime);
      CurrentMicroThread.FFinished := True;
      if CurrentMicroThread.FFreeOnTerminate then begin
        // Microthread is finished, remove it from queue
        with Scheduler do
        try
          Lock.Acquire;
          MicroThreads.Delete(MicroThreads.IndexOf(CurrentMicroThread));
        finally
          Lock.Release;
        end;
      end;
      CurrentMicroThread := nil;
    end else
    if CurrentMicroThread.State = tsWaiting then begin
      // Execute selected thread
      CurrentMicroThread.State := tsRunning;
      CurrentMicroThread.FExecutionStartTime := Time;
      FTempPointer := CurrentMicroThread.FStackPointer;
      asm
        // Restore microthread stack
        mov eax, Self
        mov edx, [eax].TMicroThreadManager.FTempPointer
        mov esp, edx
      end;
      FTempPointer := CurrentMicroThread.FBasePointer;
      asm
        mov eax, Self
        mov edx, [eax].TMicroThreadManager.FTempPointer
        mov ebp, edx
      end;
    end;
  end else begin
    CurrentMicroThread := nil;
  end;
end;

constructor TMicroThreadManager.Create;
begin
  CurrentMicroThread := nil;
end;

destructor TMicroThreadManager.Destroy;
begin
  inherited Destroy;
end;

{ TMicroThreadSchedulerPoolThread }

procedure TMicroThreadSchedulerPoolThread.Execute;
var
  ExecutedCount: Integer;
begin
  inherited Execute;
  try
    repeat
      ExecutedCount := Manager.Execute(10);
      if ExecutedCount = 0 then Sleep(1);
    until Terminated;
  except
    on E: Exception do
      //ExceptionHandler(E);
  end;
end;

{ TMicroThreadMethod }

procedure TMicroThreadMethod.Execute;
begin
  inherited Execute;
  Method(Self);
end;


{ TMicroThread }

procedure TMicroThread.Execute;
begin

end;

procedure TMicroThread.Yield;
begin
  Manager.Yield;
end;

procedure TMicroThread.WaitFor;
begin
  if GetMicroThreadId <> -1 then
  while not FFinished do begin
    Sleep(1);
  end;
end;

procedure TMicroThread.Sleep(Duration: TDateTime);
begin
  FWakeUpTime := Scheduler.GetNow + Duration;
  State := tsSleeping;
  Yield;
end;

function TMicroThread.WaitForSignal(Signal: TEvent): TWaitResult;
begin
  repeat
    Result := Signal.WaitFor(1);
    Sleep(1);
  until Result <> wrTimeout;
end;

constructor TMicroThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt = DefaultStackSize);
begin
  FStackSize := StackSize;
  FStack := GetMem(FStackSize);
  FBasePointer := FStack + FStackSize;
  FStackPointer := FBasePointer - 20;
  FExecutionTime := 0;
  FTerminated := False;
  if CreateSuspended then
    State := tsSuspended;
  FFreeOnTerminate := True;
end;

procedure TMicroThread.Terminate;
begin
  FTerminated := True;
end;

destructor TMicroThread.Destroy;
begin
  //Terminate;
  //WaitFor;
  FreeMem(FStack);
  inherited Destroy;
end;

procedure TMicroThread.Start;
begin
  State := tsWaiting;
end;

procedure TMicroThread.Stop;
begin
  State := tsSuspended;
end;


{ TMicroThreadScheduler }

function TMicroThreadScheduler.GetNow: TDateTime;
var
  {$IFDEF Linux}T: TimeVal;{$ENDIF}
  {$IFDEF Windows}TimerValue: Int64;{$ENDIF}
begin
  {$IFDEF Windows}
  QueryPerformanceCounter(TimerValue);
  //Result := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) * 1000) // an alternative Win32 timebase
  Result := TimerValue / FFrequency;
  {$ENDIF}

  {$IFDEF Linux}
  fpgettimeofday(@t, nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := t.tv_sec + t.tv_usec / 1000000;
  {$ENDIF}

  Result := (Trunc(Now / OneSecond) + Frac(Result)) * OneSecond;
end;

function TMicroThreadScheduler.Add(MicroThread: TMicroThread): Integer;
begin
  Inc(LastId);
  MicroThread.Scheduler := Self;
  MicroThread.Id := LastId;
  Result := MicroThreads.Add(MicroThread);
end;

function TMicroThreadScheduler.AddMethod(Method: TMicroThreadEvent): Integer;
var
  NewMicroThread: TMicroThreadMethod;
begin
  NewMicroThread := TMicroThreadMethod.Create(False);
  NewMicroThread.Method := Method;
  NewMicroThread.Scheduler := Self;
  Result := Add(NewMicroThread);
end;

constructor TMicroThreadScheduler.Create;
begin
  Lock := TCriticalSection.Create;
  MicroThreads := TObjectList.Create;
  ThreadPool := TThreadPool.Create;
  {$IFDEF Windows}
  QueryPerformanceFrequency(FFrequency);
  {$ENDIF}
  RoundRobinIndex := -1;
  MainThreadManager := TMicroThreadManager.Create;
  MainThreadManager.Scheduler := Self;
end;

destructor TMicroThreadScheduler.Destroy;
begin
  MainThreadManager.Free;
  FTerminated := True;
  ThreadPool.Free;
  MicroThreads.Free;
  Lock.Free;
  inherited Destroy;
end;

procedure TMicroThreadScheduler.Start;
var
  Executed: Integer;
begin
  FTerminated := False;
  repeat
    Executed := MainThreadManager.Execute(10);
    Application.ProcessMessages;
    if Executed = 0 then Sleep(1);
  until FTerminated;
end;

procedure TMicroThreadScheduler.Stop;
begin
  FTerminated := True;
end;

function TMicroThreadScheduler.GetNextMicroThread: TMicroThread;
var
  I: Integer;
begin
  Result := nil;
  try
    Lock.Acquire;
    I := 0;
    Inc(RoundRobinIndex);
    if RoundRobinIndex >= MicroThreads.Count then
      RoundRobinIndex := 0;
    while (I < MicroThreads.Count) and
     (TMicroThread(MicroThreads[RoundRobinIndex]).State <> tsWaiting) do begin
      // WakeUp sleeping threads
      if (TMicroThread(MicroThreads[RoundRobinIndex]).State = tsSleeping) and
        (TMicroThread(MicroThreads[RoundRobinIndex]).FWakeupTime < Time) then
          TMicroThread(MicroThreads[RoundRobinIndex]).State := tsWaiting else
      begin
        // Go to next thread
        Inc(I);
        Inc(RoundRobinIndex);
        if RoundRobinIndex >= MicroThreads.Count then
          RoundRobinIndex := 0;
      end;
    end;
    if I < MicroThreads.Count then begin
      Result := TMicroThread(MicroThreads[RoundRobinIndex]);
    end;
  finally
    Lock.Release;
  end;
end;

function TMicroThreadScheduler.GetMicroThreadCount: Integer;
begin
  try
    Lock.Acquire;
    Result := MicroThreads.Count;
  finally
    Lock.Release;
  end;
end;

function TMicroThreadScheduler.GetThreadPoolSize: Integer;
begin
  Result := FThreadPoolSize;
end;

procedure TMicroThreadScheduler.SetThreadPoolSize(const AValue: Integer);
begin
  FThreadPoolSize := AValue;
end;

initialization

StaticManagers := TObjectList.Create;
MainScheduler := TMicroThreadScheduler.Create;

finalization

MainScheduler.Free;
StaticManagers.Free;

end.

