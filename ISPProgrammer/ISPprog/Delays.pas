unit Delays;

{$MODE Delphi}

interface

procedure WaitS(x:integer);
procedure WaitMS(x:integer);
procedure WaitUS(x:integer);
procedure WaitNS(x:integer);

procedure Tic(var t:Int64);
function TocMS(t:Int64):Int64;
function TocUS(t:Int64):Int64;
function TocNS(t:Int64):Int64;

implementation

uses
  {$IFDEF Windows}Windows,{$ENDIF}LCLIntf, SysUtils, DateUtils, Classes;

var
  persec:Int64;

procedure WaitS(x:integer);
begin
  Sleep(x * 1000);
end;

procedure WaitMS(x:integer);
begin
  Sleep(x);
end;

procedure WaitUS(x:integer);
var
  f1,f2,c:Int64;
begin
  {$IFDEF Windows}
  f1:=0; f2:=0;
  QueryPerformanceCounter(f1);
  c:=f1+(persec*x) div 1000000;
  if c = f1 then
    c:=f1 + 1;
  repeat
    QueryPerformanceCounter(f2);
  until f2>=c;
  {$ELSE}
  Sleep(X div 1000);
  {$ENDIF}
end;

procedure WaitNS(x:integer);
var
  f1,f2,c:Int64;
begin
  {$IFDEF Windows}
  f1:=0; f2:=0;
  QueryPerformanceCounter(f1);
  c:=f1+(persec*x) div 1000000000;
  if c = f1 then
    c:=f1 + 1;
  repeat
    QueryPerformanceCounter(f2);
  until f2>=c;
  {$ELSE}
  Sleep(X div 1000);
  {$ENDIF}
end;

procedure Tic(var t:Int64);
begin
  {$IFDEF Windows}
  QueryPerformanceCounter(t);
  {$ELSE}
  T := Trunc(Now / OneMillisecond);
  {$ENDIF}
end;

function TocMS(t:Int64):Int64;  {returns time difference in ms}
var
  t2:Int64;
begin
  {$IFDEF Windows}
  t2:=0;
  QueryPerformanceCounter(t2);
  Result:=((t2 - t) * 1000) div persec;
  {$ELSE}
  T := Trunc(Now / OneMillisecond);
  {$ENDIF}
end;

function TocUS(t:Int64):Int64;  {returns time difference in us}
var
  t2:Int64;
begin
  {$IFDEF Windows}
  t2:=0;
  QueryPerformanceCounter(t2);
  Result:=((t2 - t) * 1000000) div persec;
  {$ELSE}
  T := Trunc(Now / OneMillisecond) * 1000;
  {$ENDIF}
end;

function TocNS(t:Int64):Int64;  {returns time difference in ns}
var
  t2:Int64;
begin
  {$IFDEF Windows}
  t2:=0;
  QueryPerformanceCounter(t2);
  Result:=((t2 - t) * 1000000000) div persec;
  {$ELSE}
  T := Trunc(Now / OneMillisecond) * 1000000;
  {$ENDIF}
end;

initialization
  persec:=0;
{$IFDEF Windows}
  QueryPerformanceFrequency(persec);
{$ENDIF}

end.
