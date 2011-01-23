unit UMicroThreading;

{$mode objfpc}{$H+}
{$asmmode intel}

interface

uses
  Classes, SysUtils, Contnrs, SyncObjs;

type
  TMicroThread = class;
  TMicroThreadScheduler = class;

  TStartEvent = procedure(MicroThread: TMicroThread) of object;

  TCallerAddr = packed record
    case Boolean of
      True: (A: TStartEvent;);
      False: (B, C: Pointer;);
  end;

  TMicroThreadState = (tsReady, tsRunning, tsSleeping, tsBlocked, tsSuspended);

  { TMicroThread }

  TMicroThread = class
  private
    FMethod: TStartEvent;
    FStack: Pointer;
    FStackSize: Integer;
    FInstructionPointer: Pointer;
    FExecutionStartTime: TDateTime;
    FExecutionEndTime: TDateTime;
    FWakeupTime: TDateTime;
    procedure Finish;
    procedure SaveContext;
    procedure RestoreContext;
    procedure Init;
  public
    FStackPointer: Pointer;
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
  end;

  TThreadPool = class(TObjectList)

  end;

  { TMicroThreadScheduler }

  TMicroThreadScheduler = class
  private
    ThreadPool: TThreadPool;
    MicroThreads: TObjectList; // TList<TMicroThread>
    Lock: TCriticalSection;
    RoundRobinIndex: Integer;
    LastId: Integer;
    FStackPointer: Pointer;
    procedure Yield(MicroThread: TMicroThread);
  public
    function Add(Name: string; Method: TStartEvent): TMicroThread;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
  end;

implementation

{ TMicroThread }

procedure TMicroThread.Yield;
begin
  Scheduler.Yield(Self);
end;

procedure TMicroThread.Sleep(Duration: TDateTime);
begin
  FWakeupTime := Now + Duration;
  State := tsBlocked;
  Yield;
end;

constructor TMicroThread.Create;
begin
  FStackSize := $10000;
  FStack := GetMem(FStackSize);
  FStackPointer := FStack + FStackSize;
end;

destructor TMicroThread.Destroy;
begin
  FreeMem(FStack);
  inherited Destroy;
end;

procedure TMicroThread.SaveContext; assembler; nostackframe;
asm
  mov eax, Self
  mov edx, esp
  mov [eax].FStackPointer, edx
  pop edx
  mov [eax].FInstructionPointer, edx
end;

procedure TMicroThread.RestoreContext; assembler; nostackframe;
asm
  mov eax, Self
  mov edx, [eax].FStackPointer
  mov esp, edx
  mov edx, [eax].FInstructionPointer
  push edx
  ret
end;

procedure TMicroThread.Init;
var
  FProc: TCallerAddr;
begin
  FProc.A := Method;
  FInstructionPointer := FProc.B;
end;

procedure TMicroThread.Finish;
begin
  // Microthread is finished, remove it from queue
  try
    Scheduler.Lock.Acquire;
    Scheduler.MicroThreads.Delete(Scheduler.MicroThreads.IndexOf(Self));
  finally
    Scheduler.Lock.Release;
  end;
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
  NewMicroThread.Init;
  MicroThreads.Add(NewMicroThread);
end;

constructor TMicroThreadScheduler.Create;
begin
  Lock := TCriticalSection.Create;
  MicroThreads := TObjectList.Create;
  ThreadPool := TThreadPool.Create;
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
  Yield(nil);
end;

procedure TMicroThreadScheduler.Yield(MicroThread: TMicroThread);
var
  I: Integer;
begin
  if Assigned(MicroThread) then begin
    MicroThread.FExecutionStartTime := Now;
    MicroThread.SaveContext;
    MicroThread.State := tsSleeping;
    asm
      // Restore scheduler stack
      mov eax, Self
      mov edx, [eax].FStackPointer
      mov esp, edx
    end;
  end;

  // Try to find new microthread for execution
  try
    Lock.Acquire;
    I := 0;
    while (I < MicroThreads.Count) and (TMicroThread(MicroThreads[I]).State <> tsReady) do
      Inc(I);
    if I < MicroThreads.Count then begin
      MicroThread := TMicroThread(MicroThreads[I]);
    end;
  finally
    Lock.Release;
  end;

  if Assigned(MicroThread) then begin
    asm
      // Store scheduler stack
      mov eax, Self
      mov edx, esp
      mov [eax].FStackPointer, edx
    end;
    if MicroThread.State = tsReady then begin
      MicroThread.State := tsRunning;
      MicroThread.FExecutionStartTime := Now;
      asm
        // Restore microthread stack
        mov eax, MicroThread
        mov edx, [eax].FStackPointer
        mov esp, edx
      end;
      MicroThread.Method(MicroThread);
      asm
        // Restore scheduler stack
        mov eax, Self
        mov edx, [eax].FStackPointer
        mov esp, edx
      end;
    end else
    if MicroThread.State = tsSleeping then begin
      // Execute selected thread
      MicroThread.State := tsRunning;
      MicroThread.FExecutionStartTime := Now;
      MicroThread.RestoreContext;
    end;
  end;
end;

end.

