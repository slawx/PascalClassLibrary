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
    procedure CallExecute;
    function GetStackUsed: Integer;
    procedure SetScheduler(const AValue: TMicroThreadScheduler);
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
    procedure Synchronize(AMethod: TThreadMethod);

    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    property Id: Integer read FId;
    property State: TMicroThreadState read FState;
    property ExecutionTime: TDateTime read FExecutionTime;
    property FreeOnTerminate: Boolean read FFreeOnTerminate
      write FFreeOnTerminate;
    property Terminated: Boolean read FTerminated;
    property Scheduler: TMicroThreadScheduler read FScheduler
      write SetScheduler;
    property Manager: TMicroThreadManager read FManager;
    property StackUsed: Integer read GetStackUsed;
  end;

  TMicroThreadEvent = procedure(MicroThread: TMicroThread) of object;

  { TMicroThreadMethod }

  TMicroThreadMethod = class(TMicroThread)
    Method: TMicroThreadEvent;
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
    function Execute(Count: Integer): Integer;
  public
    Id: Integer;
    procedure Yield;
    procedure Synchronize(AMethod: TThreadMethod);
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
    function GetNextMicroThread: TMicroThread;
    procedure ReleaseMicroThread(MicroThread: TMicroThread);
    procedure SetUseMainThread(const AValue: Boolean);
    procedure Start;
    procedure Stop;
    procedure PoolThreadTerminated(Sender: TObject);
    procedure UpdateThreadPoolSize;
    procedure MainThreadStart(Sender: TObject);
  public
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadEvent): Integer;
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
  MicroThreadStateText: array[TMicroThreadState] of string = ('Waiting',
    'Running', 'Blocked', 'Suspended', 'Sleeping');

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
      mov [eax].TMicroThread.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThread.FBasePointer, edx

      // Restore FScheduler stack
      mov edx, [ecx].TMicroThreadManager.FStackPointer
      mov esp, edx
      mov edx, [ecx].TMicroThreadManager.FBasePointer
      mov ebp, edx
    end;
    FCurrentMicroThread.FManager := nil;
    FScheduler.ReleaseMicroThread(FCurrentMicroThread);
    FCurrentMicroThread := nil;
  end;

  if FExecutedCount < FExecuteCount then begin
    FCurrentMicroThread := FScheduler.GetNextMicroThread;

    if Assigned(FCurrentMicroThread) then begin
      Inc(FExecutedCount);
      FCurrentMicroThread.FExecutionStartTime := NowPrecise;
      FCurrentMicroThread.FManager := Self;
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
        asm
          // Restore microthread stack
          mov ecx, Self
          mov eax, [ecx].TMicroThreadManager.FCurrentMicroThread
          mov edx, [eax].TMicroThread.FStackPointer
          mov esp, edx
          mov edx, [eax].TMicroThread.FBasePointer
          mov ebp, edx
          // We want to call virtual method Execute
          // but virtual methods can be called only statically
          // Then static method CallExecute is calling virtual method Execute
          call TMicroThread.CallExecute

          // Restore FScheduler stack
          // ecx register is set by CallExecute to running micro thread
          mov eax, [ecx].TMicroThread.FManager
          mov edx, [eax].TMicroThreadManager.FStackPointer
          mov esp, edx
          mov edx, [eax].TMicroThreadManager.FBasePointer
          mov ebp, edx
        end;

        FCurrentMicroThread.FExecutionEndTime := NowPrecise;
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
        end else begin
          FCurrentMicroThread.FManager := nil;
          FScheduler.ReleaseMicroThread(FCurrentMicroThread);
        end;
        //FCurrentMicroThread.FManager := nil;
        //FScheduler.ReleaseMicroThread(FCurrentMicroThread);
        FCurrentMicroThread := nil;
      end else
      //if FCurrentMicroThread.State = tsWaiting then
      begin
        // Execute selected thread
        asm
          // Restore microthread stack
          mov ecx, Self
          mov eax, [ecx].TMicroThreadManager.FCurrentMicroThread
          mov edx, [eax].TMicroThread.FStackPointer
          mov esp, edx
          mov edx, [eax].TMicroThread.FBasePointer
          mov ebp, edx
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
      ExecutedCount := Manager.Execute(10);
      if ExecutedCount = 0 then Sleep(1);
    until Terminated;
  except
    on E: Exception do
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
    Application.ProcessMessages;
    Sleep(1);
  until ExecuteTerminated;

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

procedure TMicroThread.SetScheduler(const AValue: TMicroThreadScheduler);
begin
  FScheduler := AValue;
end;

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
  FWakeUpTime := NowPrecise + Duration;
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
  FStackPointer := FBasePointer - SizeOf(Pointer);
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

function TMicroThreadScheduler.AddMethod(Method: TMicroThreadEvent): Integer;
var
  NewMicroThread: TMicroThreadMethod;
begin
  NewMicroThread := TMicroThreadMethod.Create(False);
  NewMicroThread.Method := Method;
  NewMicroThread.FScheduler := Self;
  Result := Add(NewMicroThread);
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
  FMainThreadTerminated := False;
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
  until FMainThreadTerminated and (ThreadPoolSize = 0);
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
    if ThreadIndex <> -1 then FThreadPool.Delete(ThreadIndex);
    FThreadPool.OwnsObjects := True;
  finally
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
        NewThread.Manager.Id := FThreadPool.Count + 1;
        NewThread.Manager.FThread := NewThread;
        NewThread.OnTerminate := PoolThreadTerminated;
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
  repeat
    Executed := FMainThreadManager.Execute(1);
    Application.ProcessMessages;
    if Executed = 0 then Sleep(1);
  until (FState <> ssRunning) or (not FUseMainThread);
  FMainThreadTerminated := True;
end;

function TMicroThreadScheduler.GetNextMicroThread: TMicroThread;
var
  I: Integer;
  CurrentTime: TDateTime;
begin
  CurrentTime := NowPrecise;
  Result := nil;
  try
    FMicroThreadsLock.Acquire;
    I := 0;
    Inc(FRoundRobinIndex);
    if FRoundRobinIndex >= FMicroThreads.Count then
      FRoundRobinIndex := 0;
    while (I < FMicroThreads.Count) and
     (TMicroThread(FMicroThreads[FRoundRobinIndex]).FState <> tsWaiting) do begin
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
      Result.FState := tsRunning;
    end;
  finally
    FMicroThreadsLock.Release;
  end;
end;

procedure TMicroThreadScheduler.ReleaseMicroThread(MicroThread: TMicroThread);
begin
  try
    FMicroThreadsLock.Acquire;
    if MicroThread.FState = tsRunning then begin
      MicroThread.FState := tsWaiting;
    end;
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

//StaticManagers := TObjectList.Create;
//MainScheduler := TMicroThreadScheduler.Create;

finalization

//MainScheduler.Free;
//StaticManagers.Free;

end.

