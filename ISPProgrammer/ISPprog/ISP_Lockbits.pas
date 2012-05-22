unit ISP_Lockbits;

{$MODE Delphi}

interface

function  ISPReadLockBits(var lb:byte):integer;
function  ISPWriteLockBits(lb:byte):integer;
function  AnyLock:boolean;

implementation

uses Delays, Globals, Processors, SPI;

function ISPReadLockBits(var lb:byte):integer;
var data:array[0..2] of byte;
begin
  Result:=0;
  case Signatures[devicenr].algo_lb of
    ALGO_LB_2323:
    begin
      data[0] := $58;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      lb:=ReadByte;
      lb:=$f9 or ((lb and $40) shr 4) or ((lb and $80) shr 6);
    end;
    ALGO_LB_TINY, ALGO_LB_2333, ALGO_LB_MEGA:
    begin
      data[0] := $58;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      lb:=ReadByte;
    end;
    ALGO_LB_89S51:
    begin
      data[0] := $24;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      lb:=ReadByte xor $ff;
    end;
    ALGO_LB_89S8253, ALGO_LB_89S2051:
    begin
      data[0] := $24;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      lb:=ReadByte;
    end;
  else
    Result:=-1;  // no ISP command to read lock bits
  end;
end;

function ISPWriteLockBits(lb:byte):integer;
var data:array[0..3] of byte;
begin
  Result:=0;
  case Signatures[devicenr].algo_lb of
    ALGO_LB_89x:                    // 89S53/8252
    begin
      data[0] := $ac;
      data[1] := $07 or (lb and $e0);
      data[2] := $00;
      WriteBytes(@data, 3);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_89S51:                  // 89S51/52
    begin
      // Mode 1, no lock protection
      data[0] := $ac;
      data[1] := $e0;
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
      if (lb and $04) = 0 then
      begin
        // Mode 2, lock bit 1 activated
        data[0] := $ac;
        data[1] := $e1;
        data[2] := $00;
        data[3] := $00;
        WriteBytes(@data, 4);
        Sync;
        WaitMS(Signatures[devicenr].prog_time);
      end;
      if (lb and $08) = 0 then
      begin
        // Mode 3, lock bit 2 activated
        data[0] := $ac;
        data[1] := $e2;
        data[2] := $00;
        data[3] := $00;
        WriteBytes(@data, 4);
        Sync;
        WaitMS(Signatures[devicenr].prog_time);
      end;
      if (lb and $10) = 0 then
      begin
        // Mode 4, lock bit 3 activated
        data[0] := $ac;
        data[1] := $e3;
        data[2] := $00;
        data[3] := $00;
        WriteBytes(@data, 4);
        Sync;
        WaitMS(Signatures[devicenr].prog_time);
      end;
    end;
    ALGO_LB_STD, ALGO_LB_TINY,      // AVR
    ALGO_LB_2323, ALGO_LB_2333:
    begin
      data[0] := $ac;
      data[1] := $f9 or (lb and $06);
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_MEGA:                   // ATmega
    begin
      data[0] := $ac;
      data[1] := $e0;
      data[2] := $00;
      data[3] := $c0 or (lb and $3f);
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_89S8253, ALGO_LB_89S2051:  // AT89S8253, AT89S2051/4051
    begin
      data[0] := $ac;
      data[1] := $e0 or (lb and $07);
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
  else
    Result:=-1;  // no ISP command to write lock bits
  end;
end;

function AnyLock:boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to 7 do
    if (Signatures[devicenr].lockbits[i] <> '') then
      Exit;
  Result:=false;
end;

end.
