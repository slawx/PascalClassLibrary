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
  Classes, ExtCtrls, SysUtils, Contnrs, SyncObjs, DateUtils, Dialogs, Forms,
  UPlatform, UMicroThreadList, UThreadEx;

const
  DefaultStackSize = $4000;

resourcestring
  SStackOverflow = 'Microthread %d stack error. Pointer %s , range < %s ; %s >';
  SNilThreadReference = 'Can''t release nil thread.';
  SManagerMicroThreadRunning = 'Manager already have running microthread';
  SManagerReferenceLost = 'Reference to manager lost';
  SCantDetermineThreadID = 'Can''t determine thread for id %d';
  SNotInThread = 'Not in thread';


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
    FExceptObjectStack: PExceptObject;
    FExceptAddrStack: PExceptAddr;
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

  TMicroThreadThreadState = (ttsReady, ttsRunning, ttsTerminated);

  { TMicroThreadThread }

  TMicroThreadThread = class(TThreadEx)
    Manager: TMicroThreadManager;
    State: TMicroThreadThreadState;
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
    FExceptObjectStack: PExceptObject;
    FExceptAddrStack: PExceptAddr;
    FExecuteCount: Integer;
    FExecutedCount: Integer;
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
    FMainThreadTerminated: Boolean;
    FMicroThreads: TObjectList; // TList<TMicroThread>
    FMainThreadManager: TMicroThreadManager;
    FMicroThreadsLock: TCriticalSection;
    FState: TMicroThreadSchedulerState;
    FUseMainThread: Boolean;
    FMainThreadStarter: TTimer;
    FEvents: TObjectList;
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
    procedure UpdateThreadPoolSize;
    procedure MainThreadStart(Sender: TObject);
    procedure MainThreadTick(Data: PtrInt);
  public
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadMethod): Integer;
    procedure Remove(MicroThread: TMicroThread; Free: Boolean = True);
    constructor Create;
    destructor Destroy; override;
    property ThreadPool: TObjectList read FThreadPool;
    property ThreadPoolLock: TCriticalSection read FThreadPoolLock;
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

  TMicroThreadList = class(TComponent)
  private
  public
    Form: TMicroThreadListForm;
    constructor Create(AOwner: TComponent); override;
  end;

  TMicroThreadExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

var
  MainScheduler: TMicroThreadScheduler;
  ExceptionHandler: TMicroThreadExceptionEvent;

const
  MicroThreadStateText: array[TMicroThreadState] of string = ('None', 'Waiting',
    'Running', 'Blocked', 'Suspended');
  MicroThreadBlockStateText: array[TMicroThreadBlockState] of string = ('None',
    'Sleeping', 'WaitFor', 'Terminating', 'Terminated');
  MicroThreadThreadStateText: array[TMicroThreadThreadState] of string = (
    'Ready', 'Running', 'Terminated');

function GetCurrentMicroThread: TMicroThread;
procedure MTSleep(Duration: TDateTime);
procedure MTSynchronize(Method: TThreadMethod);
function MTWaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
procedure Log(Text: string);
procedure Register;

const
  LogFileName: string = 'Log.txt';

implementation

//var
//  StaticManagers: TObjectList; // TList<TMicroThreadManager>;
//  StaticManager: TMicroThreadManager;
//  StaticMicroThread: TMicroThread;

procedure Register;
begin
  RegisterComponents('MicroThreading', [TMicroThreadList]);
end;

function GetMicroThreadId: Integer;
var
  MT: TMicroThread;
begin
  MT := GetCurrentMicroThread;
  if Assigned(MT) then Result := MT.Id else Result := -1;
end;

function GetCurrentMicroThread: TMicroThread;
var
  Thread: TThread;
begin
  with MainScheduler do
  try
    FMicroThreadsLock.Acquire;
    if MainThreadID = ThreadID then Result := MainThreadManager.CurrentMicroThread
      else begin
        Thread := TThreadEx.CurrentThread;
        if Assigned(Thread) then
          Result := TMicroThreadThread(Thread).Manager.CurrentMicroThread
          else Result := nil;
      end;
  finally
    FMicroThreadsLock.Release;
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

procedure MTSynchronize(Method: TThreadMethod);
var
  Thread: TThread;
begin
  if GetCurrentThreadId <> MainThreadID then begin
    Thread := TThreadEx.CurrentThread;
    if Assigned(Thread) then TThread.Synchronize(Thread, Method)
      else raise Exception.Create(Format(SCantDetermineThreadID, [GetCurrentThreadId]));
  end else Method;
end;

function MTWaitForEvent(Event: TMicroThreadEvent; Duration: TDateTime): TWaitResult;
var
  MT: TMicroThread;
begin
  MT := GetCurrentMicroThread;
  if Assigned(MT) then Result := MT.WaitForEvent(Event, Duration)
    else raise Exception.Create(SNotInThread);
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

{ TMicroThreadList }

constructor TMicroThreadList.Create(AOwner: TComponent);
begin
  inherited;
  Form := TMicroThreadListForm.Create(Self);
end;



{ TMicroThreadMethod }

procedure TMicroThreadEvent.SetEvent;
var
  I: Integer;
begin
  try
    MainScheduler.FMicroThreadsLock.Acquire;
    for I := 0 to FMicroThreads.Count - 1 do
    with TMicroThread(FMicroThreads[I]) do begin
      if (FState = tsBlocked) and (FBlockState = tbsWaitFor) then
        FState := tsWaiting;
    end;
    if not FAutoReset then FSignaled := True;
  finally
    MainScheduler.FMicroThreadsLock.Release;
  end;
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
  FAutoReset := True;
  FMicroThreads := TObjectList.Create;
  FMicroThreads.OwnsObjects := False;
  FMicroThreadsLock := TCriticalSection.Create;
  MainScheduler.FEvents.Add(Self);
end;

destructor TMicroThreadEvent.Destroy;
begin
  try
    MainScheduler.FEvents.OwnsObjects := False;
    MainScheduler.FEvents.Delete(MainScheduler.FEvents.IndexOf(Self));
  finally
    MainScheduler.FEvents.OwnsObjects := True;
  end;
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

    FCurrentMicroThread.FExceptObjectStack := GetExceptionObjectStack;
    FCurrentMicroThread.FExceptAddrStack := GetExceptionAddrStack;
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
    SetExceptionObjectStack(FExceptObjectStack);
    SetExceptionAddrStack(FExceptAddrStack);
    FCurrentMicroThread.CheckStack;
    FScheduler.ReleaseMicroThread(FCurrentMicroThread);
  end;

  if FExecutedCount < FExecuteCount then begin
    FScheduler.GetNextMicroThread(Self);
    if Assigned(FCurrentMicroThread) then begin
      Inc(FExecutedCount);
      FCurrentMicroThread.FExecutionStartTime := NowPrecise;
      FExceptObjectStack := GetExceptionObjectStack;
      FExceptAddrStack := GetExceptionAddrStack;
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
        SetExceptionObjectStack(FCurrentMicroThread.FExceptObjectStack);
        SetExceptionAddrStack(FCurrentMicroThread.FExceptAddrStack);
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
        SetExceptionObjectStack(FExceptObjectStack);
        SetExceptionAddrStack(FExceptAddrStack);
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
            FCurrentMicroThread.Manager := nil;
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
        SetExceptionObjectStack(FCurrentMicroThread.FExceptObjectStack);
        SetExceptionAddrStack(FCurrentMicroThread.FExceptAddrStack);
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
  try
    repeat
      State := ttsRunning;
      ExecutedCount := Manager.Execute(10);
      State := ttsReady;
      if ExecutedCount = 0 then Sleep(1);
    until Terminated;
  except
    on E: Exception do
      if Assigned(ExceptionHandler) then ExceptionHandler(Self, E);
  end;
end;

constructor TMicroThreadThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  State := ttsReady;
  Manager := TMicroThreadManager.Create;
end;

destructor TMicroThreadThread.Destroy;
begin
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
  try
    Execute;
  except
    on E: Exception do
      if Assigned(ExceptionHandler) then ExceptionHandler(Self, E);
  end;
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
    then raise EStackOverflow.Create(Format(SStackOverflow,
      [FId, IntToHex(Integer(FStackPointer), 8), IntToHex(Integer(FStack), 8),
      IntToHex(Integer(FStack + FStackSize), 8)]));
end;

procedure TMicroThread.Execute;
begin

end;

procedure TMicroThread.Yield;
begin
  if not Assigned(FManager) then
    raise Exception.Create(SManagerReferenceLost);
  if FStatePending = tsNone then
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
    FBlockTime := NowPrecise + Duration;
    FBlockState := tbsWaitFor;
    FStatePending := tsBlocked;
  finally
    Event.FMicroThreadsLock.Release;
  end;
  Yield;
  if FBlockTime < NowPrecise then
    Result := wrTimeout else Result := wrSignaled;

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
  MainScheduler.Add(Self);
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
  MainScheduler.Remove(Self, False);
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

procedure TMicroThreadScheduler.Remove(MicroThread: TMicroThread;
  Free: Boolean = True);
begin
  try
    FMicroThreadsLock.Acquire;
    if not Free then FMicroThreads.OwnsObjects := False;
    FMicroThreads.Remove(MicroThread);
    FMicroThreads.OwnsObjects := True;
  finally
    FMicroThreadsLock.Release;
  end;
end;

constructor TMicroThreadScheduler.Create;
begin
  FEvents := TObjectList.Create;
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
  FEvents.Free;
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
  // Wait for all thread managers to finish
  try
    FThreadPoolLock.Acquire;
    for I := 0 to FThreadPool.Count - 1 do begin
      TMicroThreadThread(FThreadPool[I]).Terminate;
    end;
    for I := 0 to FThreadPool.Count - 1 do begin
      TMicroThreadThread(FThreadPool[I]).WaitFor;
    end;
    FThreadPool.Clear;
  finally
    FThreadPoolLock.Release;
  end;

  repeat
    Application.ProcessMessages;
    Sleep(1);
  until FMainThreadTerminated or (not FUseMainThread);
  FState := ssStopped;
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
        //NewThread.OnTerminate := PoolThreadTerminated;
        NewThread.FreeOnTerminate := False;
        ThreadPool.Add(NewThread);
        NewThread.Resume;
      end;
    end else begin
      while FThreadPool.Count > FThreadPoolSize do begin
        TMicroThreadThread(FThreadPool[FThreadPool.Count - 1]).Terminate;
        TMicroThreadThread(FThreadPool[FThreadPool.Count - 1]).WaitFor;
        FThreadPool.Delete(FThreadPool.Count - 1);
      end;
    end;
  finally
    FThreadPoolLock.Release;
  end;
end;

procedure TMicroThreadScheduler.MainThreadStart(Sender: TObject);
begin
  FMainThreadStarter.Enabled := False;
  FMainThreadTerminated := False;
  Application.QueueAsyncCall(MainThreadTick, 0);
end;

procedure TMicroThreadScheduler.MainThreadTick(Data: PtrInt);
var
  Executed: Integer;
begin
//  try
    Executed := FMainThreadManager.Execute(1);
    if Executed = 0 then Sleep(1);
    // If not terminated then queue next tick else terminate
    if (FState = ssRunning) and FUseMainThread then
      Application.QueueAsyncCall(MainThreadTick, 0)
      else FMainThreadTerminated := True;
//  except
//    FMainThreadTerminated := True;
//    raise;
//  end;
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
        raise Exception.Create(SManagerMicroThreadRunning);
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
    raise Exception.Create(SNilThreadReference);
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

