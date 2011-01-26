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
    FWakeUpTime: TDateTime;
    FTerminated: Boolean;
    FExecuted: Boolean; // At first go through Execute method, then switch context
    FFinished: Boolean;
    FSuspended: Boolean;
    FState: TMicroThreadState;
    FScheduler: TMicroThreadScheduler;
    FManager: TMicroThreadManager;
    FId: Integer;
  public
    Name: string;
    Priority: Integer;
    Completion: Single; // Can be used for progress information in range <0, 1>
    procedure Execute; virtual;

    procedure Yield;
    procedure Sleep(Duration: TDateTime);
    function WaitForSignal(Signal: TEvent): TWaitResult;
    procedure WaitFor;
    procedure Terminate;
    procedure Start;
    procedure Resume;
    procedure Suspend;

    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property Id: Integer read FId;
    property State: TMicroThreadState read FState;
    property ExecutionTime: TDateTime read FExecutionTime;
    property FreeOnTerminate: Boolean read FFreeOnTerminate
      write FFreeOnTerminate;
    property Terminated: Boolean read FTerminated;
    property Scheduler: TMicroThreadScheduler read FScheduler;
    property Manager: TMicroThreadManager read FManager;
  end;

  TMicroThreadEvent = procedure(MicroThread: TMicroThread) of object;

  { TMicroThreadMethod }

  TMicroThreadMethod = class(TMicroThread)
    Method: TMicroThreadEvent;
    procedure Execute; override;
  end;

  { TMicroThreadSchedulerPoolThread }

  TMicroThreadSchedulerPoolThread = class(TThread)
    Manager: TMicroThreadManager;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
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
    FCurrentMicroThread: TMicroThread;
    FScheduler: TMicroThreadScheduler;
    function Execute(Count: Integer): Integer;
  public
    procedure Yield;
    constructor Create;
    destructor Destroy; override;
    property Scheduler: TMicroThreadScheduler read FScheduler;
    property CurrentMicroThread: TMicroThread read FCurrentMicroThread;
  end;

  TMicroThreadSchedulerState = (ssStopped, ssRunning, ssTerminating);

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    FActive: Boolean;
    FThreadPool: TThreadPool;
    FThreadPoolLock: TCriticalSection;
    FThreadPoolSize: Integer;
    FRoundRobinIndex: Integer;
    FLastId: Integer;
    FFrequency: Int64;
    FTerminate: Boolean;
    FTerminated: Boolean;
    FMicroThreads: TObjectList; // TList<TMicroThread>
    FMainThreadManager: TMicroThreadManager;
    FMicroThreadsLock: TCriticalSection;
    FState: TMicroThreadSchedulerState;
    function GetMicroThreadCount: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetActive(const AValue: Boolean);
    procedure SetThreadPoolSize(const AValue: Integer);
    function GetNextMicroThread: TMicroThread;
    procedure WaitFor;
    procedure Start;
    procedure Stop;
    function ThreadPoolTerminated: Boolean;
  public
    function GetNow: TDateTime;
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadEvent): Integer;
    function GetCPUCoreCount: Integer;
    constructor Create;
    destructor Destroy; override;
    property MicroThreadCount: Integer read GetMicroThreadCount;
    property ThreadPoolSize: Integer read GetThreadPoolSize
      write SetThreadPoolSize;
    property MicroThreads: TObjectList read FMicroThreads;
    property MicroThreadsLock: TCriticalSection read FMicroThreadsLock;
    property MainThreadManager: TMicroThreadManager read FMainThreadManager;
    property Active: Boolean read FActive write SetActive;
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
      FMicroThreadsLock.Acquire;
      I := 0;
      while (I < FMicroThreads.Count) and
        not ((CurrentStack >= TMicroThread(FMicroThreads[I]).FStack) and
        (CurrentStack <= (TMicroThread(FMicroThreads[I]).FStack +
        TMicroThread(FMicroThreads[I]).FStackSize))) do Inc(I);
      if I < FMicroThreads.Count then begin
        Result := TMicroThread(FMicroThreads[I]).FId;
      end else Result := -1;
    finally
      FMicroThreadsLock.Release;
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
  CurrentTime: TDateTime;
begin
  CurrentTime := FScheduler.GetNow;
  if Assigned(FCurrentMicroThread) then begin
    FCurrentMicroThread.FExecutionEndTime := CurrentTime;
    FCurrentMicroThread.FExecutionTime := FCurrentMicroThread.FExecutionTime +
      (FCurrentMicroThread.FExecutionEndTime - FCurrentMicroThread.FExecutionStartTime);
    if FCurrentMicroThread.FState = tsRunning then
      FCurrentMicroThread.FState := tsWaiting;
    StaticMicroThread := FCurrentMicroThread;
    asm
      // Store microthread stack
      mov eax, StaticMicroThread
      mov edx, esp
      mov [eax].TMicroThread.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThread.FBasePointer, edx
    end;
    StaticManager := FCurrentMicroThread.FManager;
    asm
      // Restore FScheduler stack
      mov eax, StaticManager  // Self is invalid before BP restore
      mov edx, [eax].TMicroThreadManager.FStackPointer
      mov esp, edx
      mov edx, [eax].TMicroThreadManager.FBasePointer
      mov ebp, edx
    end;
    FCurrentMicroThread.FManager := nil;
    FCurrentMicroThread := nil;
  end;

  FCurrentMicroThread := FScheduler.GetNextMicroThread;

  if Assigned(FCurrentMicroThread) and (FExecutedCount < FExecuteCount) then begin
    FCurrentMicroThread.FManager := Self;
    Inc(FExecutedCount);
    asm
      // Store FScheduler stack
      mov eax, Self
      mov edx, esp
      mov [eax].TMicroThreadManager.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThreadManager.FBasePointer, edx
    end;
    if not FCurrentMicroThread.FExecuted then begin
      FCurrentMicroThread.FExecuted := True;
      FCurrentMicroThread.FState := tsRunning;
      FCurrentMicroThread.FExecutionStartTime := CurrentTime;
      StaticMicroThread := FCurrentMicroThread;
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
      StaticManager := FCurrentMicroThread.FManager;
      asm
        // Restore FScheduler stack
        mov eax, StaticManager // Self is invalid before BP restore
        mov edx, [eax].TMicroThreadManager.FStackPointer
        mov esp, edx
        mov edx, [eax].TMicroThreadManager.FBasePointer
        mov ebp, edx
      end;
      FCurrentMicroThread.FManager := nil;
      FCurrentMicroThread.FExecutionEndTime := CurrentTime;
      FCurrentMicroThread.FExecutionTime := FCurrentMicroThread.FExecutionTime +
       (FCurrentMicroThread.FExecutionEndTime - FCurrentMicroThread.FExecutionStartTime);
      FCurrentMicroThread.FFinished := True;
      if FCurrentMicroThread.FFreeOnTerminate then begin
        // Microthread is finished, remove it from queue
        with FScheduler do
        try
          FMicroThreadsLock.Acquire;
          FMicroThreads.Delete(FMicroThreads.IndexOf(FCurrentMicroThread));
        finally
          FMicroThreadsLock.Release;
        end;
      end;
      FCurrentMicroThread := nil;
    end else
    if FCurrentMicroThread.State = tsWaiting then begin
      // Execute selected thread
      FCurrentMicroThread.FState := tsRunning;
      FCurrentMicroThread.FExecutionStartTime := CurrentTime;
      FTempPointer := FCurrentMicroThread.FStackPointer;
      asm
        // Restore microthread stack
        mov eax, Self
        mov edx, [eax].TMicroThreadManager.FTempPointer
        mov esp, edx
      end;
      FTempPointer := FCurrentMicroThread.FBasePointer;
      asm
        mov eax, Self
        mov edx, [eax].TMicroThreadManager.FTempPointer
        mov ebp, edx
      end;
    end;
  end else begin
    FCurrentMicroThread := nil;
  end;
end;

constructor TMicroThreadManager.Create;
begin
  FCurrentMicroThread := nil;
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

constructor TMicroThreadSchedulerPoolThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Manager := TMicroThreadManager.Create;
end;

destructor TMicroThreadSchedulerPoolThread.Destroy;
begin
  Manager.Free;
  inherited Destroy;
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
  FManager.Yield;
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
  FWakeUpTime := FScheduler.GetNow + Duration;
  FState := tsSleeping;
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
  if CreateSuspended then begin
    FState := tsSuspended;
    FSuspended := True;
  end else FSuspended := False;
  FFreeOnTerminate := True;
end;

procedure TMicroThread.Terminate;
begin
  FTerminated := True;
end;

procedure TMicroThread.Start;
begin
  FState := tsWaiting;
end;

destructor TMicroThread.Destroy;
begin
  //Terminate;
  //WaitFor;
  FreeMem(FStack);
  inherited Destroy;
end;

procedure TMicroThread.Resume;
begin
  FSuspended := False;
  if FState = tsSuspended then
    FState := tsWaiting;
end;

procedure TMicroThread.Suspend;
begin
  FSuspended := True;
  //Yield;
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
  Inc(FLastId);
  MicroThread.FScheduler := Self;
  MicroThread.FId := FLastId;
  Result := FMicroThreads.Add(MicroThread);
end;

function TMicroThreadScheduler.AddMethod(Method: TMicroThreadEvent): Integer;
var
  NewMicroThread: TMicroThreadMethod;
begin
  NewMicroThread := TMicroThreadMethod.Create(False);
  NewMicroThread.Method := Method;
  NewMicroThread.FScheduler := Self;
  Result := Add(NewMicroThread);
end;

function TMicroThreadScheduler.GetCPUCoreCount: Integer;
var
  SystemInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
end;

constructor TMicroThreadScheduler.Create;
begin
  FTerminated := True;
  FMicroThreadsLock := TCriticalSection.Create;
  FMicroThreads := TObjectList.Create;
  FThreadPool := TThreadPool.Create;
  FThreadPoolLock := TCriticalSection.Create;
  {$IFDEF Windows}
  QueryPerformanceFrequency(FFrequency);
  {$ENDIF}
  FRoundRobinIndex := -1;
  FMainThreadManager := TMicroThreadManager.Create;
  FMainThreadManager.FScheduler := Self;
end;

destructor TMicroThreadScheduler.Destroy;
begin
  Active := False;
  FMainThreadManager.Free;
  FThreadPool.Free;
  FMicroThreads.Free;
  FMicroThreadsLock.Free;
  inherited Destroy;
end;

procedure TMicroThreadScheduler.Start;
var
  Executed: Integer;
  I: Integer;
begin
  FTerminated := False;
  FTerminate := False;
  for I := 0 to FThreadPool.Count - 1 do
    TMicroThreadSchedulerPoolThread(FThreadPool[I]).Start;
  repeat
    Executed := FMainThreadManager.Execute(10);
    Application.ProcessMessages;
    if Executed = 0 then Sleep(1);
  until FTerminate;
  FTerminated := True;
end;

procedure TMicroThreadScheduler.Stop;
var
  I: Integer;
begin
  try
    FThreadPoolLock.Acquire;
    for I := 0 to FThreadPool.Count - 1 do begin
      TMicroThreadSchedulerPoolThread(FThreadPool[I]).Terminate;
    end;
  finally
    FThreadPoolLock.Release;
  end;
  FTerminate := True;

  // Wait for all thread managers to finish
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until FTerminated and (ThreadPoolSize = 0);
end;

function TMicroThreadScheduler.ThreadPoolTerminated: Boolean;
var
  I: Integer;
begin
  try
    FThreadPoolLock.Acquire;
    I := 0;
    while (I < FThreadPool.Count) and
      (TMicroThreadSchedulerPoolThread(FThreadPool[I]).Terminated do
  finally
    FThreadPoolLock.Release;
  end;
end;

function TMicroThreadScheduler.GetNextMicroThread: TMicroThread;
var
  I: Integer;
  CurrentTime: TDateTime;
begin
  CurrentTime := GetNow;
  Result := nil;
  try
    FMicroThreadsLock.Acquire;
    I := 0;
    Inc(FRoundRobinIndex);
    if FRoundRobinIndex >= FMicroThreads.Count then
      FRoundRobinIndex := 0;
    while (I < FMicroThreads.Count) and
     (TMicroThread(FMicroThreads[FRoundRobinIndex]).State <> tsWaiting) do begin
      // WakeUp sleeping threads
      if (TMicroThread(FMicroThreads[FRoundRobinIndex]).FState = tsSleeping) and
        (TMicroThread(FMicroThreads[FRoundRobinIndex]).FWakeupTime < CurrentTime) then
          TMicroThread(FMicroThreads[FRoundRobinIndex]).FState := tsWaiting else
      begin
        // Go to next thread
        Inc(I);
        Inc(FRoundRobinIndex);
        if FRoundRobinIndex >= FMicroThreads.Count then
          FRoundRobinIndex := 0;
      end;
    end;
    if I < FMicroThreads.Count then begin
      Result := TMicroThread(FMicroThreads[FRoundRobinIndex]);
    end;
  finally
    FMicroThreadsLock.Release;
  end;
end;

function TMicroThreadScheduler.GetMicroThreadCount: Integer;
begin
  try
    FMicroThreadsLock.Acquire;
    Result := FMicroThreads.Count;
  finally
    FMicroThreadsLock.Release;
  end;
end;

function TMicroThreadScheduler.GetThreadPoolSize: Integer;
begin
  Result := FThreadPoolSize;
end;

procedure TMicroThreadScheduler.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then Start
    else Stop;
end;

procedure TMicroThreadScheduler.SetThreadPoolSize(const AValue: Integer);
var
  I: Integer;
  NewThread: TMicroThreadSchedulerPoolThread;
begin
  FThreadPoolSize := AValue;
  if FState = ssRunning then
    SetThreadPoolCount
end;

initialization

StaticManagers := TObjectList.Create;
MainScheduler := TMicroThreadScheduler.Create;

finalization

MainScheduler.Free;
StaticManagers.Free;

end.

