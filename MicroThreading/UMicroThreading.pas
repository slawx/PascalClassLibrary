(* Not implemented yet
- Stack limit checking
- measurement of cpu usage by micro threads
- microthread critical sections (no low level cpu blocking)
- wait for single and multiple objects
- micro thread priorty
*)

unit UMicroThreading;

{$mode Delphi}{$H+}
{$asmmode intel}

interface

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, ExtCtrls, SysUtils, Contnrs, SyncObjs, DateUtils, Dialogs, Forms, UPlatform;

const
  DefaultStackSize = $4000;

type
  TMicroThread = class;
  TMicroThreadScheduler = class;
  TMicroThreadManager = class;

  TMicroThreadState = (tsNone, tsWaiting, tsRunning, tsBlocked, tsSuspended);
  TMicroThreadBlockState = (tbsNone, tbsSleeping, tbsWaitFor, tbsTerminating,
    tbsTerminated);

  { TMicroThreadEvent }

  TMicroThreadEvent = class
  private
    FAutoReset: Boolean;
    FSignaled: Boolean;
    FMicroThreads: TObjectList;
    FMicroThreadsLock: TCriticalSection;
  public
    procedure SetEvent;
    procedure ResetEvent;
    procedure WaitFor(Duration: TDateTime);
    constructor Create;
    destructor Destroy; override;
    property Signaled: Boolean read FSignaled;
    property AutoReset: Boolean read FAutoReset write FAutoReset;
  end;

  { TMicroThread }

  TMicroThread = class
  private
    FFreeOnTerminate: Boolean;
    FExecutionStartTime: TDateTime;
    FExecutionEndTime: TDateTime;
    FExecutionTime: TDateTime;
    FExecutionCount: Integer;
    FStack: Pointer;
    FStackPointer: Pointer;
    FStackSize: Integer;
    FBasePointer: Pointer;
    FExecuted: Boolean; // At first go through Execute method, then switch context
    FBlockState: TMicroThreadBlockState;
    FBlockTime: TDateTime;
    FState: TMicroThreadState;
    FStatePending: TMicroThreadState;
    FScheduler: TMicroThreadScheduler;
    FManager: TMicroThreadManager;
    FId: Integer;
    procedure CallExecute;
    function GetStackUsed: Integer;
    function GetTerminated: Boolean;
    procedure SetManager(const AValue: TMicroThreadManager);
    procedure SetScheduler(const AValue: TMicroThreadScheduler);
    procedure CheckStack;
  public
    Name: string;
    Priority: Integer;
    Completion: Single; // Can be used for progress information usually in range <0, 1>
    procedure Execute; virtual;

    procedure Yield;
    procedure MTSleep(Duration: TDateTime); // No conflicting name to global Sleep procedure
    function WaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
    procedure WaitFor;
    procedure Terminate;
    procedure Start;
    procedure Resume;
    procedure Suspend;
    procedure Synchronize(AMethod: TThreadMethod);

    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property Id: Integer read FId;
    property State: TMicroThreadState read FState;
    property BlockState: TMicroThreadBlockState read FBlockState;
    property ExecutionTime: TDateTime read FExecutionTime;
    property ExecutionCount: Integer read FExecutionCount;
    property FreeOnTerminate: Boolean read FFreeOnTerminate
      write FFreeOnTerminate;
    property Terminated: Boolean read GetTerminated;
    property Scheduler: TMicroThreadScheduler read FScheduler
      write SetScheduler;
    property Manager: TMicroThreadManager read FManager write SetManager;
    property StackUsed: Integer read GetStackUsed;
  end;

  TMicroThreadMethod = procedure(MicroThread: TMicroThread) of object;

  { TMicroThreadSimple }

  TMicroThreadSimple = class(TMicroThread)
    Method: TMicroThreadMethod;
    procedure Execute; override;
  end;

  { TMicroThreadThread }

  TMicroThreadThread = class(TThread)
    Manager: TMicroThreadManager;
    ExecuteTerminated: Boolean;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
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
    FCurrentMicroThread: TMicroThread;
    FScheduler: TMicroThreadScheduler;
    FThread: TMicroThreadThread;
    FId: Integer;
    procedure SetCurrentMicroThread(const AValue: TMicroThread);
    function Execute(Count: Integer): Integer;
    property CurrentMicroThread: TMicroThread read FCurrentMicroThread
      write SetCurrentMicroThread;
  public
    procedure Yield;
    procedure Synchronize(AMethod: TThreadMethod);
    constructor Create;
    destructor Destroy; override;
    property Scheduler: TMicroThreadScheduler read FScheduler;
  end;

  TMicroThreadSchedulerState = (ssStopped, ssRunning, ssTerminating);

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    FActive: Boolean;
    FThreadPool: TObjectList;
    FThreadPoolLock: TCriticalSection;
    FThreadPoolSize: Integer;
    FRoundRobinIndex: Integer;
    FLastId: Integer;
    FFrequency: Int64;
    FMainThreadTerminated: Boolean;
    FMicroThreads: TObjectList; // TList<TMicroThread>
    FMainThreadManager: TMicroThreadManager;
    FMicroThreadsLock: TCriticalSection;
    FState: TMicroThreadSchedulerState;
    FUseMainThread: Boolean;
    FMainThreadStarter: TTimer;
    function GetMicroThreadCount: Integer;
    function GetThreadPoolCount: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetActive(const AValue: Boolean);
    procedure SetThreadPoolSize(const AValue: Integer);
    procedure GetNextMicroThread(Manager: TMicroThreadManager);
    procedure ReleaseMicroThread(MicroThread: TMicroThread);
    procedure SetUseMainThread(const AValue: Boolean);
    procedure Start;
    procedure Stop;
    procedure PoolThreadTerminated(Sender: TObject);
    procedure UpdateThreadPoolSize;
    procedure MainThreadStart(Sender: TObject);
  public
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadMethod): Integer;
    procedure Remove(MicroThread: TMicroThread);
    constructor Create;
    destructor Destroy; override;
    property ThreadPool: TObjectList read FThreadPool;
    property ThreadPoolSize: Integer read GetThreadPoolSize
      write SetThreadPoolSize;
    property ThreadPoolCount: Integer read GetThreadPoolCount;
    property MicroThreads: TObjectList read FMicroThreads;
    property MicroThreadsLock: TCriticalSection read FMicroThreadsLock;
    property MicroThreadCount: Integer read GetMicroThreadCount;
    property MainThreadManager: TMicroThreadManager read FMainThreadManager;
    property Active: Boolean read FActive write SetActive;
    property UseMainThread: Boolean read FUseMainThread write SetUseMainThread;
  end;

var
  MainScheduler: TMicroThreadScheduler;

const
  MicroThreadStateText: array[TMicroThreadState] of string = ('None', 'Waiting',
    'Running', 'Blocked', 'Suspended');
  MicroThreadBlockStateText: array[TMicroThreadBlockState] of string = ('None',
    'Sleeping', 'WaitFor', 'Terminating', 'Terminated');

function GetCurrentMicroThread: TMicroThread;
procedure MTSleep(Duration: TDateTime);
function MTWaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
procedure Log(Text: string);

const
  LogFileName: string = 'Log.txt';

implementation

//var
//  StaticManagers: TObjectList; // TList<TMicroThreadManager>;
//  StaticManager: TMicroThreadManager;
//  StaticMicroThread: TMicroThread;

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

function GetCurrentMicroThread: TMicroThread;
var
  I: Integer;
begin
  with MainScheduler do
  try
    FThreadPoolLock.Acquire;
    if MainThreadID = ThreadID then Result := MainThreadManager.CurrentMicroThread
    else begin
      I := 0;
      while (I < FThreadPool.Count) and (TMicroThreadThread(FThreadPool[I]).ThreadID <> ThreadID) do Inc(I);
      if I < FThreadPool.Count then Result := TMicroThreadThread(FThreadPool[I]).Manager.CurrentMicroThread
        else Result := nil;
    end;
  finally
    FThreadPoolLock.Release;
  end;
end;

procedure MTSleep(Duration: TDateTime);
var
  MT: TMicroThread;
begin
  MT := GetCurrentMicroThread;
  if Assigned(MT) then MT.MTSleep(Duration)
    else Sleep(Trunc(Duration / OneMillisecond));
end;

function MTWaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
var
  MT: TMicroThread;
begin
  MT := GetCurrentMicroThread;
  if Assigned(MT) then Result := MT.WaitForEvent(Event, Duration);
//    else Result := Event.WaitFor(Trunc(Duration / OneMillisecond));
end;

var
  LogLock: TCriticalSection;

procedure Log(Text: string);
var
  LogFile: TextFile;
begin
  try
    LogLock.Acquire;
    AssignFile(LogFile, LogFileName);
    if FileExists(LogFileName) then Append(LogFile)
      else Rewrite(LogFile);
    WriteLn(LogFile, Text);
    CloseFile(LogFile);
  finally
    LogLock.Release;
  end;
end;

{ TMicroThreadMethod }

procedure TMicroThreadEvent.SetEvent;
var
  I: Integer;
begin
  for I := 0 to FMicroThreads.Count - 1 do
    TMicroThread(FMicroThreads[I]).FStatePending := tsWaiting;
  if not FAutoReset then FSignaled := True;
end;

procedure TMicroThreadEvent.ResetEvent;
begin
  FSignaled := False;
end;

procedure TMicroThreadEvent.WaitFor(Duration: TDateTime);
var
  MT: TMicroThread;
begin
  MT := GetCurrentMicroThread;
  if Assigned(MT) then MT.WaitForEvent(Self, Duration);
end;

constructor TMicroThreadEvent.Create;
begin
  FMicroThreads := TObjectList.Create;
  FMicroThreads.OwnsObjects := False;
  FMicroThreadsLock := TCriticalSection.Create;
end;

destructor TMicroThreadEvent.Destroy;
begin
  FMicroThreadsLock.Free;
  FMicroThreads.Free;
  inherited Destroy;
end;

{ TMicroThreadManager }

procedure TMicroThreadManager.SetCurrentMicroThread(const AValue: TMicroThread
  );
begin
  if FCurrentMicroThread = AValue then Exit;
  if Assigned(FCurrentMicroThread) then
    FCurrentMicroThread.FManager := nil;
  FCurrentMicroThread := AValue;
  if Assigned(FCurrentMicroThread) then
    FCurrentMicroThread.FManager := Self;
end;

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
begin
  if Assigned(FCurrentMicroThread) then begin
    FCurrentMicroThread.FExecutionEndTime := NowPrecise;
    FCurrentMicroThread.FExecutionTime := FCurrentMicroThread.FExecutionTime +
      (FCurrentMicroThread.FExecutionEndTime - FCurrentMicroThread.FExecutionStartTime);
    asm
      // Store microthread stack
      mov ecx, Self
      mov eax, [ecx].TMicroThreadManager.FCurrentMicroThread
      mov edx, esp
      mov ebx, ebp
      mov [eax].TMicroThread.FStackPointer, edx
      mov [eax].TMicroThread.FBasePointer, ebx

      // Restore manager stack
      mov edx, [ecx].TMicroThreadManager.FStackPointer
      mov ebx, [ecx].TMicroThreadManager.FBasePointer
      mov esp, edx
      mov ebp, ebx
    end;
    FCurrentMicroThread.CheckStack;
    if FCurrentMicroThread = nil then
      raise Exception.Create('x');
    FScheduler.ReleaseMicroThread(FCurrentMicroThread);
  end;

  if FExecutedCount < FExecuteCount then begin
    FScheduler.GetNextMicroThread(Self);
    if Assigned(FCurrentMicroThread) then begin
      Inc(FExecutedCount);
      FCurrentMicroThread.FExecutionStartTime := NowPrecise;
      asm
        // Store manager stack
        mov eax, Self
        mov edx, esp
        mov ebx, ebp
        mov [eax].TMicroThreadManager.FStackPointer, edx
        mov [eax].TMicroThreadManager.FBasePointer, ebx
      end;
      if not FCurrentMicroThread.FExecuted then begin
        // First time micro thread execution
        FCurrentMicroThread.FExecuted := True;
        asm
          // Restore microthread stack
          mov ecx, Self
          mov eax, [ecx].TMicroThreadManager.FCurrentMicroThread
          mov edx, [eax].TMicroThread.FStackPointer
          mov ebx, [eax].TMicroThread.FBasePointer
          mov esp, edx
          mov ebp, ebx
          // We want to call virtual method Execute
          // but virtual methods can be called only statically
          // Then static method CallExecute is calling virtual method Execute
          call TMicroThread.CallExecute

          // Restore manager stack
          // ecx register is set by CallExecute to running micro thread
          mov eax, [ecx].TMicroThread.FManager
          mov edx, [eax].TMicroThreadManager.FStackPointer
          mov ebx, [eax].TMicroThreadManager.FBasePointer
          mov esp, edx
          mov ebp, ebx
        end;
        FCurrentMicroThread.CheckStack;
        FCurrentMicroThread.FExecutionEndTime := NowPrecise;
        FCurrentMicroThread.FExecutionTime := FCurrentMicroThread.FExecutionTime +
         (FCurrentMicroThread.FExecutionEndTime - FCurrentMicroThread.FExecutionStartTime);
        FCurrentMicroThread.FStatePending := tsBlocked;
        FCurrentMicroThread.FBlockState := tbsTerminated;
        if FCurrentMicroThread.FFreeOnTerminate then begin
          // Microthread is finished, remove it from queue
          with FScheduler do
          try
            FMicroThreadsLock.Acquire;
            FMicroThreads.Delete(FMicroThreads.IndexOf(FCurrentMicroThread));
          finally
            FMicroThreadsLock.Release;
          end;
        end else begin
          FScheduler.ReleaseMicroThread(FCurrentMicroThread);
        end;
        //FCurrentMicroThread.FManager := nil;
        //FScheduler.ReleaseMicroThread(FCurrentMicroThread);
        //FCurrentMicroThread := nil;
      end else
      begin
        // Regular selected microthread execution
        FCurrentMicroThread.CheckStack;
        asm
          // Restore microthread stack
          mov ecx, Self
          mov eax, [ecx].TMicroThreadManager.FCurrentMicroThread
          mov edx, [eax].TMicroThread.FStackPointer
          mov ebx, [eax].TMicroThread.FBasePointer
          mov esp, edx
          mov ebp, ebx
        end;
      end;
    end;
  end;
end;

procedure TMicroThreadManager.Synchronize(AMethod: TThreadMethod);
begin
  if Assigned(FThread) then
    FThread.Synchronize(FThread, AMethod);
end;

constructor TMicroThreadManager.Create;
begin
  FCurrentMicroThread := nil;
  FThread := nil;
end;

destructor TMicroThreadManager.Destroy;
begin
  inherited Destroy;
end;

{ TMicroThreadThread }

procedure TMicroThreadThread.Execute;
var
  ExecutedCount: Integer;
begin
  inherited Execute;
  try
    repeat
      ExecutedCount := Manager.Execute(100000);
      if ExecutedCount = 0 then Sleep(1);
    until Terminated;
  except
    on E: Exception do ;
      //ExceptionHandler(E);
  end;
end;

constructor TMicroThreadThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  ExecuteTerminated := False;
  Manager := TMicroThreadManager.Create;
end;

destructor TMicroThreadThread.Destroy;
begin
  Terminate;
  repeat
    Sleep(1);
  until ExecuteTerminated;

  Manager.Free;
  inherited Destroy;
end;

{ TMicroThreadSimple }

procedure TMicroThreadSimple.Execute;
begin
  inherited Execute;
  Method(Self);
end;


{ TMicroThread }

procedure TMicroThread.CallExecute;
begin
  Execute;
  asm
    mov ecx, Self
  end;
end;

function TMicroThread.GetStackUsed: Integer;
begin
  Result := FStack + FStackSize - FStackPointer;
end;

function TMicroThread.GetTerminated: Boolean;
begin
  Result := (FState = tsBlocked) and (FBlockState = tbsTerminated);
end;

procedure TMicroThread.SetManager(const AValue: TMicroThreadManager);
begin
  if FManager = AValue then Exit;
  if Assigned(FManager) then FManager.CurrentMicroThread := nil;
  FManager := AValue;
  if Assigned(FManager) then FManager.CurrentMicroThread := Self;
end;

procedure TMicroThread.SetScheduler(const AValue: TMicroThreadScheduler);
begin
  FScheduler := AValue;
end;

procedure TMicroThread.CheckStack;
begin
  if not ((FStackPointer > FStack) and (FStackPointer < (FStack + FStackSize)))
    then raise EStackOverflow.Create(Format('Microthread %d stack error', [FId]));
end;

procedure TMicroThread.Execute;
begin

end;

procedure TMicroThread.Yield;
begin
  if not Assigned(FManager) then
    raise Exception.Create('Manager reference lost');
  FStatePending := tsWaiting;
  FManager.Yield;
end;

procedure TMicroThread.WaitFor;
begin
  if GetMicroThreadId <> -1 then
  while not ((FState = tsBlocked) and (FBlockState = tbsTerminated)) do begin
    MTSleep(1);
  end;
end;

procedure TMicroThread.MTSleep(Duration: TDateTime);
begin
  FBlockTime := NowPrecise + Duration;
  FBlockState := tbsSleeping;
  FStatePending := tsBlocked;
  Yield;
end;

function TMicroThread.WaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
begin
  try
    Event.FMicroThreadsLock.Acquire;
    Event.FMicroThreads.Add(Self);
  finally
    Event.FMicroThreadsLock.Release;
  end;
  FBlockTime := NowPrecise + Duration;
  FBlockState := tbsWaitFor;
  FStatePending := tsBlocked;
  Yield;
  try
    Event.FMicroThreadsLock.Acquire;
    Event.FMicroThreads.Remove(Self);
  finally
    Event.FMicroThreadsLock.Release;
  end;
end;

constructor TMicroThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt = DefaultStackSize);
begin
  FStackSize := StackSize;
  FStack := GetMem(FStackSize);
  FBasePointer := FStack + FStackSize;
  FStackPointer := FBasePointer - SizeOf(Pointer);
  FExecutionTime := 0;
  FState := tsWaiting;
  FStatePending := tsNone;
  if CreateSuspended then begin
    FState := tsSuspended;
  end;
  FFreeOnTerminate := True;
end;

procedure TMicroThread.Terminate;
begin
  FBlockState := tbsTerminated;
  FStatePending := tsBlocked;
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
  if FState = tsSuspended then
    FStatePending := tsWaiting;
end;

procedure TMicroThread.Suspend;
begin
  FStatePending := tsSuspended;
  //Yield;
end;

procedure TMicroThread.Synchronize(AMethod: TThreadMethod);
begin
  FManager.Synchronize(AMethod);
end;


{ TMicroThreadScheduler }

function TMicroThreadScheduler.Add(MicroThread: TMicroThread): Integer;
begin
  Inc(FLastId);
  MicroThread.FScheduler := Self;
  MicroThread.FId := FLastId;
  try
    FMicroThreadsLock.Acquire;
    Result := FMicroThreads.Add(MicroThread);
  finally
    FMicroThreadsLock.Release;
  end;
end;

function TMicroThreadScheduler.AddMethod(Method: TMicroThreadMethod): Integer;
var
  NewMicroThread: TMicroThreadSimple;
begin
  NewMicroThread := TMicroThreadSimple.Create(False);
  NewMicroThread.Method := Method;
  NewMicroThread.FScheduler := Self;
  Result := Add(NewMicroThread);
end;

procedure TMicroThreadScheduler.Remove(MicroThread: TMicroThread);
begin
  try
    FMicroThreadsLock.Acquire;
    FMicroThreads.Remove(MicroThread);
  finally
    FMicroThreadsLock.Release;
  end;
end;

constructor TMicroThreadScheduler.Create;
begin
  FMainThreadStarter := TTimer.Create(nil);
  FMainThreadStarter.Enabled := False;
  FMainThreadStarter.Interval := 1;
  FMainThreadStarter.OnTimer := MainThreadStart;
  FMainThreadTerminated := True;
  FMicroThreadsLock := TCriticalSection.Create;
  FMicroThreads := TObjectList.Create;
  FThreadPool := TObjectList.Create;
  FThreadPoolLock := TCriticalSection.Create;
  FRoundRobinIndex := -1;
  FMainThreadManager := TMicroThreadManager.Create;
  FMainThreadManager.FScheduler := Self;
  UseMainThread := False;
end;

destructor TMicroThreadScheduler.Destroy;
begin
  Active := False;
  FMainThreadStarter.Free;
  FMainThreadManager.Free;
  FThreadPool.Free;
  FThreadPoolLock.Free;
  FMicroThreads.Free;
  FMicroThreadsLock.Free;
  inherited Destroy;
end;

procedure TMicroThreadScheduler.Start;
begin
  UpdateThreadPoolSize;
  FState := ssRunning;
  if FUseMainThread then
    FMainThreadStarter.Enabled := True;
end;

procedure TMicroThreadScheduler.Stop;
var
  I: Integer;
begin
  FState := ssTerminating;
  try
    FThreadPoolLock.Acquire;
    for I := 0 to FThreadPool.Count - 1 do begin
      TMicroThreadThread(FThreadPool[I]).Terminate;
    end;
  finally
    FThreadPoolLock.Release;
  end;

  // Wait for all thread managers to finish
  repeat
    Application.ProcessMessages;
    Sleep(1);
  until FMainThreadTerminated and (ThreadPoolCount = 0);
  FState := ssStopped;
end;

procedure TMicroThreadScheduler.PoolThreadTerminated(Sender: TObject);
var
  ThreadIndex: Integer;
begin
  TMicroThreadThread(Sender).ExecuteTerminated := True;
  try
    FThreadPoolLock.Acquire;
    FThreadPool.OwnsObjects := False;
    ThreadIndex := FThreadPool.IndexOf(Sender);
    if ThreadIndex = -1 then
      raise Exception.Create('Trying to free thread not found in thread pool');
    FThreadPool.Delete(ThreadIndex);
  finally
    FThreadPool.OwnsObjects := True;
    FThreadPoolLock.Release;
  end;
end;

procedure TMicroThreadScheduler.UpdateThreadPoolSize;
var
  NewThread: TMicroThreadThread;
begin
  try
    FThreadPoolLock.Acquire;
    if FThreadPoolSize > FThreadPool.Count then begin
      FThreadPool.Capacity := FThreadPoolSize;
      while FThreadPool.Count < FThreadPoolSize do begin
        NewThread := TMicroThreadThread.Create(True);
        NewThread.Manager.FScheduler := Self;
        NewThread.Manager.FId := FThreadPool.Count + 1;
        NewThread.Manager.FThread := NewThread;
        NewThread.OnTerminate := PoolThreadTerminated;
        NewThread.FreeOnTerminate := True;
        ThreadPool.Add(NewThread);
        NewThread.Resume;
      end;
    end else begin
      while FThreadPool.Count > FThreadPoolSize do begin
        FThreadPool.Delete(FThreadPool.Count - 1);
      end;
    end;
  finally
    FThreadPoolLock.Release;
  end;
end;

procedure TMicroThreadScheduler.MainThreadStart(Sender: TObject);
var
  Executed: Integer;
begin
  FMainThreadStarter.Enabled := False;
  FMainThreadTerminated := False;
  repeat
    Executed := FMainThreadManager.Execute(1);
    Application.ProcessMessages;
    if Executed = 0 then Sleep(1);
  until (FState <> ssRunning) or (not FUseMainThread);
  FMainThreadTerminated := True;
end;

procedure TMicroThreadScheduler.GetNextMicroThread(Manager: TMicroThreadManager);
var
  I: Integer;
  CurrentTime: TDateTime;
  Selected: TMicroThread;
begin
  try
    FMicroThreadsLock.Acquire;
    CurrentTime := NowPrecise;
    I := 0;
    Selected := nil;
    Inc(FRoundRobinIndex);
    if FRoundRobinIndex >= FMicroThreads.Count then
      FRoundRobinIndex := 0;
    while (I < FMicroThreads.Count) do
    with TMicroThread(FMicroThreads[FRoundRobinIndex]) do begin
      if (FState = tsWaiting) then Break
      else
      if (FState = tsBlocked) then begin
        // Wakeup sleeping threads
        if (FBlockState = tbsSleeping) and
          (FBlockTime < CurrentTime) then begin
            FState := tsWaiting;
            FBlockState := tbsNone;
            Break;
          end
        else
        // Unblock event waiting threads
        if (FBlockState = tbsWaitFor) and
          (FBlockTime < CurrentTime) then begin
            FState := tsWaiting;
            FBlockState := tbsNone;
            Break;
          end;
      end;
      // Go to next thread
      Inc(I);
      FRoundRobinIndex := (FRoundRobinIndex + 1) mod FMicroThreads.Count;
    end;
    if I < FMicroThreads.Count then begin
      if Assigned(Manager.FCurrentMicroThread) then
        raise Exception.Create('Manager have already have running microthread');
      Selected := TMicroThread(FMicroThreads[FRoundRobinIndex]);
      Selected.FState := tsRunning;
      Inc(Selected.FExecutionCount);
    end;
    Manager.CurrentMicroThread := Selected;
  finally
    FMicroThreadsLock.Release;
  end;
end;

procedure TMicroThreadScheduler.ReleaseMicroThread(MicroThread: TMicroThread);
begin
  if not Assigned(MicroThread) then
    raise Exception.Create('Can''t realease nil thread.');
  try
    FMicroThreadsLock.Acquire;
    if MicroThread.FStatePending <> tsNone then begin
      MicroThread.FState := MicroThread.FStatePending;
      MicroThread.FStatePending := tsNone;
    end;
    MicroThread.Manager := nil;
  finally
    FMicroThreadsLock.Release;
  end;
end;

procedure TMicroThreadScheduler.SetUseMainThread(const AValue: Boolean);
begin
  if FUseMainThread = AValue then Exit;
  FUseMainThread := AValue;
  if FState = ssRunning then begin
    if AValue then FMainThreadStarter.Enabled := True;
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

function TMicroThreadScheduler.GetThreadPoolCount: Integer;
begin
  try
    FThreadPoolLock.Acquire;
    Result := FThreadPool.Count;
  finally
    FThreadPoolLock.Release;
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
  NewThread: TMicroThreadThread;
begin
  FThreadPoolSize := AValue;
  if FState = ssRunning then
    UpdateThreadPoolSize;
end;

initialization

DeleteFile(LogFileName);
LogLock := TCriticalSection.Create;
MainScheduler := TMicroThreadScheduler.Create;

finalization

MainScheduler.Free;
LogLock.Free;

end.

