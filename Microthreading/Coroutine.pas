unit Coroutine;

{$mode Delphi}{$H+}

{

Author: Bart van der Werf
Date: 19 Feb 2006
Summary:

Quick and dirty coroutine for delphi.

Limitations:

Does not support continuation invoke chains that include the same instance twice.
Uses the assumption that pages are 4kb in size
Uses the assumtion that delphi can build its own SEH chain from scratch.
Limited to 32k instances because of the 64kb minimum stacksize. (or 48k if 3gb mode is used)
}

{$asmmode intel}

interface

uses
  SysUtils, SyncObjs, Classes;

type
{
Coroutine support class

Note: recursive calling the same instance of the coroutine not supported
Note: not threadsafe
Note: no thread affinity
}
  TCoroutine = class
  private
    FStackBuffer: Pointer;
    FStackSize: Cardinal;

    FInCoroutine: Boolean;
    FActive: Boolean;
    FTerminating: Boolean;

    FStackBase: Pointer;
    FStackLimit: Pointer;
    FSEH: Pointer;
    FStack: Pointer;

    FCurrentSEH: Pointer;
    FCurrentStack: Pointer;
    FCurrentInstruction: Pointer;

    FCallerSEH: Pointer;
    FCallerStackBase: Pointer;
    FCallerStackLimit: Pointer;
    FCallerStack: Pointer;
    FCallerInstruction: Pointer;

    FExceptionRaised: Exception;

    procedure Setup;
    procedure Reset;
    procedure BackToCaller;
    procedure Enter;
  protected
    procedure Yield; //call me from the Execute method to return the thread to the call of Invoke, unless IsTerminating is true then an exception is raised
    function IsTerminating: Boolean; //signal to Execute that it should not call Yield and that it should cleanup its resources and return from the method

    procedure Execute; virtual; abstract; // override me, either return from this method or call yield
  public
    constructor Create(const StackSize: Cardinal = $10000);
    {
    Destruction:
    If the Coroutine is currently active then:
    IsTerminating is set to true
    Then Invoke is called and we want the Execute method to return
    Yield throws an exception to enforce this.
    }

   destructor Destroy; override;

   procedure Invoke; //call me to run/continue the Execute method
end;


implementation

{ TCoroutine }

function GetCurrentAddress: Pointer; assembler; nostackframe;
asm
  mov eax, [esp]
  ret
end;

(*procedure FpuInit;
const
  Default8087CW: Word = $1332 { $133F};
asm
  FNINIT
  FWAIT
  FLDCW Default8087CW
end;*)

procedure TCoroutine.Enter;
var
  Me: TCoroutine;
begin
  Me := Self;
  asm
    pushad
    cld
    //call FpuInit
    mov eax, Me
    (*mov ecx, 0
    mov edx, fs:[ecx]
    mov [EAX].TCoroutine.FCallerSEH, edx
    mov ecx, 4
    mov edx, fs:[ecx]
    mov [EAX].TCoroutine.FCallerStackBase, edx
    mov ecx, 8
    mov edx, fs:[ecx]
    mov [EAX].TCoroutine.FCallerStackLimit, edx
    *)
    mov edx, esp
    mov [EAX].TCoroutine.FCallerStack, edx
    mov edx, offset @A
    mov [EAX].TCoroutine.FCallerInstruction, edx


    (*mov ecx, 0
    mov edx, [EAX].TCoroutine.FCurrentSEH
    mov fs:[ecx], edx
    mov ecx, 4
    mov edx, [EAX].TCoroutine.FStackBase
    mov fs:[ecx], edx
    mov ecx, 8
    mov edx, [EAX].TCoroutine.FStackLimit
    mov fs:[ecx], edx
    *)
    mov edx, [EAX].TCoroutine.FCurrentStack
    mov esp, edx
    mov edx, [EAX].TCoroutine.FCurrentInstruction
    push edx
    ret
  @A:
    popad
  end;
end;

procedure TCoroutine.Setup;
begin
  try
    Execute;
  except
    on e: Exception do
    begin
      FExceptionRaised := Exception(e.ClassType.NewInstance);
      FExceptionRaised.Message := e.Message;
      FExceptionRaised.HelpContext := e.HelpContext;
    end;
  end;
  Reset;
  BackToCaller;
end;

constructor TCoroutine.Create(const StackSize: Cardinal = $10000);
begin
  Assert(StackSize >= $10000, 'A stack is atleast 64kb large');
  Assert((StackSize and $ffff) = 0, 'Use a multiple of 64kb');

  FInCoroutine := False;

  FStackSize := StackSize;
  //FStackBuffer := VirtualAlloc(nil, FStackSize, MEM_RESERVE, PAGE_READWRITE);
  FStackBuffer := GetMem(FStackSize);
  if not Assigned(FStackBuffer) then
    //RaiseLastWin32Error
  ;

  FStackLimit := FStackBuffer;
  FStackBase := Pointer(Cardinal(FStackBuffer) + FStackSize);
  FStack := FStackBase;

  //if not Assigned(VirtualAlloc(Pointer(Cardinal(FStackBase) - 4096), 4096, MEM_COMMIT, PAGE_READWRITE)) then
  //  RaiseLastWin32Error;
  //if not Assigned(VirtualAlloc(Pointer(Cardinal(FStackBase) - 2 * 4096), 4096, MEM_COMMIT, PAGE_READWRITE + PAGE_GUARD)) then
  //  RaiseLastWin32Error;

  FSEH := nil;

  Reset;
end;

destructor TCoroutine.Destroy;
begin
  Assert(not FInCoroutine);
  if FActive then
  begin
    FTerminating := True;
    FInCoroutine := True;
    Enter;
    FInCoroutine := False;
    Assert(not FActive);
  end;
  if Assigned(FStackBuffer) then
    FreeMem(FStackBuffer);
    //if not VirtualFree(FStackBuffer, 0, MEM_RELEASE) then
      //RaiseLastWin32Error;
end;

procedure TCoroutine.Invoke;
var
  E: Exception;
begin
  Assert(Assigned(Self));
  Assert(not FInCoroutine);

  FActive := True;
  FInCoroutine := True;
  Enter;
  FInCoroutine := False;

  if Assigned(FExceptionRaised) then
  begin
    if FExceptionRaised.ClassName = 'EStackOverflow' then
      //if not Assigned(VirtualAlloc(FStackBuffer, 4096, MEM_COMMIT, PAGE_READWRITE + PAGE_GUARD)) then
        //RaiseLastWin32Error
    ;
    E := FExceptionRaised;
    FExceptionRaised := nil;
    raise E;
  end;
end;

procedure TCoroutine.Reset;
type
  TBlip = packed record
    case Blap: Boolean of
      True: (A: TThreadMethod;);
      False: (B, C: Pointer;);
  end;
var
  FProc: TBlip;
begin
  FProc.A := Setup;
  FCurrentInstruction := FProc.B;
  FCurrentSEH := FSEH;
  FCurrentStack := FStack;
  FActive := False;
end;

procedure TCoroutine.BackToCaller;
var
  Me: TCoroutine;
begin
  Me := Self;
  asm
    mov eax, Me
    cld
    //call FpuInit
    (*mov ecx, 0
    mov edx, [EAX].TCoroutine.FCallerSEH
    mov fs:[ecx], edx
    mov ecx, 4
    mov edx, [EAX].TCoroutine.FCallerStackBase
    mov fs:[ecx], edx
    mov ecx, 8
    mov edx, [EAX].TCoroutine.FCallerStackLimit
    mov fs:[ecx], edx
    *)
    mov edx, [EAX].TCoroutine.FCallerStack
    mov esp, edx
    mov edx, [EAX].TCoroutine.FCallerInstruction
    push edx
    ret
  end;
end;

procedure TCoroutine.Yield;
var
  Me: TCoroutine;
begin
  Assert(FInCoroutine);
  if FTerminating then
    raise Exception.Create('Cannot yield, terminating');

  Me := Self;
  asm
    pushad
    cld
    //call FpuInit
    mov eax, Me
(*    mov ecx, 0
    mov edx, fs:[ecx]
    mov [EAX].TCoroutine.FCurrentSEH, edx
  *)
    mov edx, esp
    mov [EAX].TCoroutine.FCurrentStack, edx
    mov edx, offset @A
    mov [EAX].TCoroutine.FCurrentInstruction, edx

    (*mov ecx, 0
    mov edx, [EAX].TCoroutine.FCallerSEH
    mov fs:[ecx], edx
    mov ecx, 4
    mov edx, [EAX].TCoroutine.FCallerStackBase
    mov fs:[ecx], edx
    mov ecx, 8
    mov edx, [EAX].TCoroutine.FCallerStackLimit
    mov fs:[ecx], edx
    *)
    mov edx, [EAX].TCoroutine.FCallerStack
    mov esp, edx
    mov edx, [EAX].TCoroutine.FCallerInstruction
    push edx
    ret
  @A:
    popad
  end;
end;

function TCoroutine.IsTerminating: Boolean;
begin
  Result := FTerminating;
end;

end. 
