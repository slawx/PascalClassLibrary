// Date: 2011-02-01

unit UThreadEx;

interface

uses
  Classes, SysUtils, SyncObjs, Contnrs;
  
const
  Quantum = 10; // ms

type

  { TThreadEx }

  TThreadEx = class(TThread)
  private
  public
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    procedure Sleep(Delay: Cardinal);
    class function CurrentThread: TThread;
  end;

var
  ThreadList: TObjectList;
  ThreadListLock: TCriticalSection;

implementation

class function TThreadEx.CurrentThread: TThread;
var
  I: Integer;
begin
  try
    ThreadListLock.Acquire;
    if MainThreadID = GetThreadId then Result := nil
    else begin
      I := 0;
      while (I < ThreadList.Count) and (TThread(ThreadList[I]).ThreadID <> GetThreadID) do
        Inc(I);
      if I < ThreadList.Count then Result := TThread(ThreadList[I])
        else Result := nil;
    end;
  finally
    ThreadListLock.Release;
  end;
end;

constructor TThreadEx.Create(CreateSuspended: Boolean; const StackSize: SizeUInt
  );
begin
  inherited;
  try
    ThreadListLock.Acquire;
    ThreadList.Add(Self);
  finally
    ThreadListLock.Release;
  end;
end;

destructor TThreadEx.Destroy;
begin
  try
    ThreadListLock.Acquire;
    ThreadList.Remove(Self);
  finally
    ThreadListLock.Release;
  end;
  inherited Destroy;
end;

procedure TThreadEx.Sleep(Delay: Cardinal);
var
  I: Integer;
begin
  SysUtils.Sleep(Delay mod Quantum);
  for I := 1 to (Delay div Quantum) do begin
    if Terminated then Break;
    SysUtils.Sleep(Quantum);
  end;
end;

initialization

ThreadList := TObjectList.Create;
ThreadList.OwnsObjects := False;
ThreadListLock := TCriticalSection.Create;

finalization

ThreadListLock.Free;
ThreadList.Free;

end.
