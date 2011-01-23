unit UMicroThreading;

{$mode objfpc}{$H+}
{$asmmode intel}

interface

uses
  Classes, SysUtils, Contnrs, SyncObjs, DateUtils,
  BaseUnix, UnixUtil, Unix;

type
  TMicroThread = class;
  TMicroThreadScheduler = class;

  TStartEvent = procedure(MicroThread: TMicroThread) of object;

  TCallerAddr = packed record
    case Boolean of
      True: (A: TStartEvent;);
      False: (B, C: Pointer;);
  end;

  TMicroThreadState = (tsReady, tsRunning, tsWaiting, tsBlocked, tsSuspended,
    tsSleeping, tsFinished);

  { TMicroThread }

  TMicroThread = class
  private
    FMethod: TStartEvent;
    FStack: Pointer;
    FStackSize: Integer;
    FExecutionStartTime: TDateTime;
    FExecutionEndTime: TDateTime;
    FExecutionTime: TDateTime;
    FStackPointer: Pointer;
    FBasePointer: Pointer;
    FWakeupTime: TDateTime;
  public
    Id: Integer;
    Name: string;
    Priority: Integer;
    State: TMicroThreadState;
    Scheduler: TMicroThreadScheduler;
    procedure Yield;
    procedure Sleep(Duration: TDateTime);
    constructor Create;
    destructor Destroy; override;
    property Method: TStartEvent read FMethod write FMethod;
    property ExecutionTime: TDateTime read FExecutionTime;
  end;

  TThreadPool = class(TObjectList)

  end;

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    FFreeMicroThreadOnFinish: Boolean;
    ThreadPool: TThreadPool;
    RoundRobinIndex: Integer;
    LastId: Integer;
    FMainStackPointer: Pointer;
    FMainBasePointer: Pointer;
    FSelected: TMicroThread;
    FTempPointer: Pointer;
    function GetMicroThreadCount: Integer;
    procedure Yield(MicroThread: TMicroThread);
  public
    MicroThreads: TObjectList; // TList<TMicroThread>
    Lock: TCriticalSection;
    function Add(Name: string; Method: TStartEvent): TMicroThread;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    property MicroThreadCount: Integer read GetMicroThreadCount;
    property FreeMicroThreadOnFinish: Boolean read FFreeMicroThreadOnFinish
      write FFreeMicroThreadOnFinish;
  end;

const
  MicroThreadStateText: array[TMicroThreadState] of string = ('Ready', 'Running',
    'Waiting', 'Blocked', 'Suspended', 'Sleeping', 'Finished');

implementation


function SystemTicks: Int64;
{$IFDEF Windows}
begin
  QueryPerformanceCounter(Result);
  //Result := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) * 1000) // an alternative Win32 timebase
{$ELSE}
var t : timeval;
begin
  fpgettimeofday(@t,nil);
   // Build a 64 bit microsecond tick from the seconds and microsecond longints
  Result := (Int64(t.tv_sec) * 1000000) + t.tv_usec;
{$ENDIF}
end;


{ TMicroThread }

procedure TMicroThread.Yield;
begin
  Scheduler.Yield(Self);
end;

procedure TMicroThread.Sleep(Duration: TDateTime);
begin
  FWakeUpTime := Now + Duration;
  State := tsSleeping;
  Yield;
end;

constructor TMicroThread.Create;
begin
  FStackSize := $10000;
  FStack := GetMem(FStackSize);
  FBasePointer := FStack + FStackSize;
  FStackPointer := FBasePointer - 20;
end;

destructor TMicroThread.Destroy;
begin
  FreeMem(FStack);
  inherited Destroy;
end;

{ TMicroThreadScheduler }

function TMicroThreadScheduler.Add(Name: string; Method: TStartEvent
  ): TMicroThread;
var
  NewMicroThread: TMicroThread;
begin
  NewMicroThread := TMicroThread.Create;
  NewMicroThread.Scheduler := Self;
  NewMicroThread.Name := Name;
  NewMicroThread.Method := Method;
  Inc(LastId);
  NewMicroThread.Id := LastId;
  MicroThreads.Add(NewMicroThread);
end;

constructor TMicroThreadScheduler.Create;
begin
  Lock := TCriticalSection.Create;
  MicroThreads := TObjectList.Create;
  ThreadPool := TThreadPool.Create;
  FFreeMicroThreadOnFinish := True;
end;

destructor TMicroThreadScheduler.Destroy;
begin
  ThreadPool.Free;
  MicroThreads.Free;
  Lock.Free;
  inherited Destroy;
end;

procedure TMicroThreadScheduler.Start;
begin
  RoundRobinIndex := -1;
  Yield(nil);
end;

var
  StaticMicroThread: TMicroThread;
  StaticScheduler: TMicroThreadScheduler;

procedure TMicroThreadScheduler.Yield(MicroThread: TMicroThread);
var
  I: Integer;
begin
  if Assigned(MicroThread) then begin
    MicroThread.FExecutionEndTime := Now;
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
  end;

  // Try to find new microthread for execution
  FSelected := nil;
  try
    Lock.Acquire;
    I := 0;
    Inc(RoundRobinIndex);
    if RoundRobinIndex >= MicroThreads.Count then
      RoundRobinIndex := 0;
    while (I < MicroThreads.Count) and (TMicroThread(MicroThreads[RoundRobinIndex]).State <> tsReady) and
(TMicroThread(MicroThreads[RoundRobinIndex]).State <> tsWaiting) do begin
      // WakeUp sleeping threads
      if (TMicroThread(MicroThreads[RoundRobinIndex]).State = tsSleeping) and
        (TMicroThread(MicroThreads[RoundRobinIndex]).FWakeupTime < Now) then
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

  if Assigned(FSelected) then begin
    asm
      // Store scheduler stack
      mov eax, Self
      mov edx, esp
      mov [eax].TMicroThreadScheduler.FMainStackPointer, edx
      mov edx, ebp
      mov [eax].TMicroThreadScheduler.FMainBasePointer, edx
    end;
    if FSelected.State = tsReady then begin
      FSelected.State := tsRunning;
      FSelected.FExecutionStartTime := Now;
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
      StaticMicroThread.Method(StaticMicroThread);
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
      if FFreeMicroThreadOnFinish then begin
        // Microthread is finished, remove it from queue
        try
          Lock.Acquire;
          MicroThreads.Delete(MicroThreads.IndexOf(FSelected));
        finally
          Lock.Release;
        end;
      end else FSelected.State := tsFinished;
    end else
    if FSelected.State = tsWaiting then begin
      // Execute selected thread
      FSelected.State := tsRunning;
      FSelected.FExecutionStartTime := Now;
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

end.

