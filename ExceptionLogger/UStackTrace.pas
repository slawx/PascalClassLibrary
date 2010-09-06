unit UStackTrace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, CustomLineInfo;

type
  TStackFrameInfo = class
    Index: Integer;
    LineNumber: Integer;
    Address: Integer;
    FunctionClassName: string;
    FunctionName: string;
    Source: string;
    procedure GetFrameInfo(Addr: Pointer);
  end;

  { TStackTrace }

  TStackTrace = class(TObjectList)
    MaxDepth: Integer;
    procedure GetExceptionBackTrace;
    procedure GetCallStack;
    constructor Create;
  end;


implementation

procedure TStackFrameInfo.GetFrameInfo(Addr: Pointer);
var
  Func: shortstring;
  SourceStr: shortstring;
  Line: LongInt;
  Store: TBackTraceStrFunc;
  Success: Boolean;
begin
  // Reset to prevent infinite recursion if problems inside the code PM
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  Success := GetLineInfo(ptruint(Addr), Func, SourceStr, Line);
  Address := Integer(Addr);
  FunctionName := Func;
  if Pos('__', FunctionName) > 0 then begin
    FunctionClassName := Copy(FunctionName, 1, Pos('__', FunctionName) - 1);
    Delete(FunctionName, 1, Length(FunctionClassName) + 2);
  end else FunctionClassName := '';
  LineNumber := Line;
  Source := SourceStr;
  BackTraceStrFunc := Store;
end;

procedure TStackTrace.GetCallStack;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
  StackFrameInfo: TStackFrameInfo;
begin
  Clear;
  //routine adapted from fpc source

  bp := get_frame;
  //This trick skip SendCallstack item
  // bp := get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    //is_dev:=do_isdevice(textrec(f).Handle);
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       StackFrameInfo := TStackFrameInfo.Create;
       StackFrameInfo.GetFrameInfo(CallerAddress);
       StackFrameInfo.Index := I + 1;
       Add(StackFrameInfo);
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
end;

constructor TStackTrace.Create;
begin
  inherited;
  MaxDepth := 20;
end;

procedure TStackTrace.GetExceptionBackTrace;
var
  FrameCount: Integer;
  Frames: PPointer;
  FrameNumber: Integer;
  StackFrameInfo: TStackFrameInfo;
begin
  Clear;
  StackFrameInfo := TStackFrameInfo.Create;
  StackFrameInfo.GetFrameInfo(ExceptAddr);
  StackFrameInfo.Index := 1;
  Add(StackFrameInfo);
  FrameCount := ExceptFrameCount;
  Frames := ExceptFrames;
  if FrameCount > MaxDepth then FrameCount := MaxDepth;
  for FrameNumber := 0 to FrameCount - 1 do begin
    StackFrameInfo := TStackFrameInfo.Create;
    StackFrameInfo.GetFrameInfo(Frames[FrameNumber]);
    StackFrameInfo.Index := FrameNumber + 1;
    Add(StackFrameInfo);
  end;
end;



end.

