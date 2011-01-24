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

  TMicroThreadState = (tsWaiting, tsRunning, tsBlocked, tsSuspended,
    tsSleeping);


  { TMicroThread }

  TMicroThread = class
  private
    FFreeOnTerminate: Boolean;
    FStack: Pointer;
    FStackSize: Integer;
    FExecutionStartTime: TDateTime;
    FExecutionEndTime: TDateTime;
    FExecutionTime: TDateTime;
    FStackPointer: Pointer;
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
    Scheduler: TMicroThreadScheduler;
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

  TThreadPool = class(TObjectList)

  end;

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    ThreadPool: TThreadPool;
    RoundRobinIndex: Integer;
    LastId: Integer;
    FMainStackPointer: Pointer;
    FMainBasePointer: Pointer;
    FSelected: TMicroThread;
    FTempPointer: Pointer;
    FFrequency: Int64;
    FExecuteCount: Integer;
    FExecutedCount: Integer;
    FTerminated: Boolean;
    FThreadPoolSize: Integer;
    function GetMicroThreadCount: Integer;
    function GetThreadPoolSize: Integer;
    procedure SetThreadPoolSize(const AValue: Integer);
    procedure Yield(MicroThread: TMicroThread);
  public
    MicroThreads: TObjectList; // TList<TMicroThread>
    Lock: TCriticalSection;
    CurrentMicroThread: TMicroThread;
    function GetNow: TDateTime;
    function Add(MicroThread: TMicroThread): Integer;
    function AddMethod(Method: TMicroThreadEvent): Integer;
    constructor Create;
    destructor Destroy; override;
    function Execute(Count: Integer): Integer;
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
  Scheduler.Yield(Self);
end;

procedure TMicroThread.WaitFor;
begin
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
  Terminate;
  WaitFor;
  // Microthread is finished, remove it from queue
  try
    Scheduler.Lock.Acquire;
    Scheduler.MicroThreads.Delete(Scheduler.MicroThreads.IndexOf(Self));
  finally
    Scheduler.Lock.Release;
  end;
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
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
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
end;

destructor TMicroThreadScheduler.Destroy;
begin
  FTerminated := True;
  ThreadPool.Free;
  MicroThreads.Free;
  Lock.Free;
  inherited Destroy;
end;

function TMicroThreadScheduler.Execute(Count: Integer): Integer;
begin
  FExecuteCount := Count;
  FExecutedCount := 0;
  Yield(nil);
  Result := FExecutedCount;
end;

procedure TMicroThreadScheduler.Start;
var
  Executed: Integer;
begin
  FTerminated := False;
  repeat
    Executed := Execute(10);
    Application.ProcessMessages;
    if Executed = 0 then Sleep(1);
  until FTerminated;
end;

procedure TMicroThreadScheduler.Stop;
begin
  FTerminated := True;
end;

var
  StaticMicroThread: TMicroThread;
  StaticScheduler: TMicroThreadScheduler;

procedure TMicroThreadScheduler.Yield(MicroThread: TMicroThread);
var
  I: Integer;
  Time: TDateTime;
begin
  Time := GetNow;
  if Assigned(MicroThread) then begin
    MicroThread.FExecutionEndTime := Time;
    MicroThread.FExecutionTime := MicroThread.FExecutionTime +
      (MicroThread.FExecutionEndTime - MicroThread.FExecutionStartTime);
    if MicroThread.State = tsRunning then
      MicroThread.State := tsWaiting;
    asm
      // Store microthread stack
      mov eax, MicroThread
      mov edx, esp
      mov [eax].TMicroThread.FStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThread.FBasePointer, edx
    end;
    StaticScheduler := MicroThread.Scheduler;
    asm
      // Restore scheduler stack
      mov eax, StaticScheduler  // Self is invalid before BP restore
      mov edx, [eax].TMicroThreadScheduler.FMainStackPointer
      mov esp, edx
      mov edx, [eax].TMicroThreadScheduler.FMainBasePointer
      mov ebp, edx
    end;
    CurrentMicroThread := nil;
  end;

  // Try to find new microthread for execution
  FSelected := nil;
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
      FSelected := TMicroThread(MicroThreads[RoundRobinIndex]);
    end;
  finally
    Lock.Release;
  end;

  if Assigned(FSelected) and (FExecutedCount < FExecuteCount) then begin
    Inc(FExecutedCount);
    CurrentMicroThread := FSelected;
    asm
      // Store scheduler stack
      mov eax, Self
      mov edx, esp
      mov [eax].TMicroThreadScheduler.FMainStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThreadScheduler.FMainBasePointer, edx
    end;
    if not FSelected.FExecuted then begin
      FSelected.FExecuted := True;
      FSelected.State := tsRunning;
      FSelected.FExecutionStartTime := Time;
      FTempPointer := FSelected.FStackPointer;
      asm
        // Restore microthread stack
        mov eax, Self
        mov edx, [eax].TMicroThreadScheduler.FTempPointer
        mov esp, edx
      end;
      StaticMicroThread := FSelected; // BP will be change and Self pointer will be invalid
      FTempPointer := FSelected.FBasePointer;
      asm
        mov eax, Self
        mov edx, [eax].TMicroThreadScheduler.FTempPointer
        mov ebp, edx
      end;
      StaticMicroThread.Execute;
      //FSelected.Method(FSelected);
      StaticScheduler := StaticMicroThread.Scheduler;
      asm
        // Restore scheduler stack
        mov eax, StaticScheduler // Self is invalid before BP restore
        mov edx, [eax].TMicroThreadScheduler.FMainStackPointer
        mov esp, edx
        mov edx, [eax].TMicroThreadScheduler.FMainBasePointer
        mov ebp, edx
      end;
      FSelected.FExecutionEndTime := Time;
      FSelected.FExecutionTime := FSelected.FExecutionTime +
       (FSelected.FExecutionEndTime - FSelected.FExecutionStartTime);
      FSelected.FFinished := True;
      if FSelected.FFreeOnTerminate then begin
        FSelected.Free;
      end;;
    end else
    if FSelected.State = tsWaiting then begin
      // Execute selected thread
      FSelected.State := tsRunning;
      FSelected.FExecutionStartTime := Time;
      FTempPointer := FSelected.FStackPointer;
      asm
        // Restore microthread stack
        mov eax, Self
        mov edx, [eax].TMicroThreadScheduler.FTempPointer
        mov esp, edx
      end;
      FTempPointer := FSelected.FBasePointer;
      asm
        mov eax, Self
        mov edx, [eax].TMicroThreadScheduler.FTempPointer
        mov ebp, edx
      end;
    end;
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

MainScheduler := TMicroThreadScheduler.Create;

finalization

MainScheduler.Free;

end.

