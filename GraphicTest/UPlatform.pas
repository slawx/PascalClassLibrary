unit UPlatform;

{$mode Delphi}{$H+}

interface

uses
  {$IFDEF Windows}Windows,{$ENDIF}
  {$IFDEF Linux}BaseUnix, UnixUtil, Unix,{$ENDIF}
  Classes, SysUtils, DateUtils, SyncObjs;

function NowPrecise: TDateTime;
function GetLogicalProcessorCount: Integer;

implementation

{$IFDEF Windows}
var
  PerformanceFrequency: Int64;
{$ENDIF}

var
  NowPreciseLock: TCriticalSection;

function NowPrecise: TDateTime;
var
  {$IFDEF Linux}T: TimeVal;{$ENDIF}
  {$IFDEF Windows}TimerValue: Int64;{$ENDIF}
begin
//  Result := Now;
  //try
    //NowPreciseLock.Acquire;
    {$IFDEF Windows}
    QueryPerformanceCounter(TimerValue);
    //Result := Int64(TimeStampToMSecs(DateTimeToTimeStamp(Now)) * 1000) // an alternative Win32 timebase
    Result := TimerValue / PerformanceFrequency;
    {$ENDIF}

    {$IFDEF Linux}
    fpgettimeofday(@t, nil);
     // Build a 64 bit microsecond tick from the seconds and microsecond longints
    Result := t.tv_sec + t.tv_usec / 1000000;
    {$ENDIF}

    Result := Result * OneSecond;
    //Result := (Trunc(Now / OneSecond) + Frac(Result)) * OneSecond;
  //finally
    //NowPreciseLock.Release;
  //end;
end;

function GetLogicalProcessorCount: Integer;
{$IFDEF Windows}
var
  SystemInfo: _SYSTEM_INFO;
  {$ENDIF}
begin
  {$IFDEF Windows}
  GetSystemInfo(SystemInfo);
  Result := SystemInfo.dwNumberOfProcessors;
  {$ENDIF}
end;

initialization

{$IFDEF Windows}
QueryPerformanceFrequency(PerformanceFrequency);
{$ENDIF}
NowPreciseLock := TCriticalSection.Create;

finalization

NowPreciseLock.Free;

end.

