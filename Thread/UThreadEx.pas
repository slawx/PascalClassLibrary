// Date: 2010-12-17

unit UThreadEx;

interface

uses
  Classes, SysUtils;
  
const
  Quantum = 10;

type
  TThreadEx = class(TThread)
    procedure SleepTerm(Delay: Integer);
  end;

implementation


procedure TThreadEx.SleepTerm(Delay: Integer);
var
  I: Integer;
begin
  Sleep(Delay mod Quantum);
  for I := 1 to (Delay div Quantum) do begin
    if Terminated then Break;
    Sleep(Quantum);
  end;
end;

end.
