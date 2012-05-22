unit ISP_EEPROM;

{$MODE Delphi}

interface

procedure WaitForReadyEEPROM(adres:integer; value:byte);
function  ISPReadEEPROM_MakeRequest(adres:integer; buf:pointer):integer;
function  ISPReadEEPROM(adres:integer):byte;
procedure ISPReadEEPROMPage(adres:integer; dane:pointer);
procedure ISPWriteEEPROM(adres:integer; dana:byte);
procedure ISPWriteEEPROMPage(adres:integer; dane:pointer);

implementation

uses Delays, Globals, ISP, MemBuffer, Processors, SPI;

procedure WaitForReadyEEPROM(adres:integer; value:byte);
var
  b:byte;
const
  t1:Int64 = 0;
begin
  case Signatures[devicenr].algo_busy of
    ALGO_BUSY_WAIT:
      WaitMS(Signatures[devicenr].prog_time);

    ALGO_BUSY_POLL_00FF:
    begin
      if (value <> $00) and (value <> $ff) then
      begin
        WaitMS (1);
        Tic(t1);
        repeat
          b:=ISPReadEEPROM(adres);
        until (b = value) or (TocMS(t1) > 100);
      end
      else
        WaitMS(Signatures[devicenr].prog_time);
    end;

    ALGO_BUSY_POLL_FF, ALGO_BUSY_POLL_NMSB:
    begin
      if value <> $ff then
      begin
        WaitMS (1);
        Tic(t1);
        repeat
          b:=ISPReadEEPROM(adres);
        until (b = value) or (TocMS(t1) > 100);
      end
      else
        WaitMS(Signatures[devicenr].prog_time);
    end;

    ALGO_BUSY_POLL_RDYBSY:
    begin
      Tic(t1);
      repeat until (ISPPollReady) or (TocMS(t1) > 100);
    end;
  end;
end;

function ISPReadEEPROM_MakeRequest(adres:integer; buf:pointer):integer;
var data:array[0..3] of byte;
begin
  Result := 0;
  if (proctype = PROC_TYPE_AVR) or (proctype = PROC_TYPE_NEW51) or (proctype = PROC_TYPE_S8253) then
  begin
    data[0] := $A0;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := 0;
    Result := 4;
  end
  else if proctype = PROC_TYPE_OLD51 then
  begin
    // AT89S53 / AT89S8252
    data[0] := (Hi(word(adres)) shl 3) or 5;
    data[1] := Lo(word(adres));
    data[2] := 0;
    Result := 3;
  end;
  Move (data, buf^, Result);
end;

function ISPReadEEPROM(adres:integer):byte;
var
  data:array[0..3] of byte;
  len:integer;
begin
  Result:=$ff;
  len:=ISPReadEEPROM_MakeRequest(adres, @data);
  if len > 0 then
  begin
    WriteBytes(@data, len - 1);
    Result:=ReadByte;
  end;
end;

procedure ISPReadEEPROMPage(adres:integer; dane:pointer);
begin
  if proctype = PROC_TYPE_AVR then
  begin
    ISPReadMemoryBlock(BUF_EEPROM, adres, dane, Signatures[devicenr].epagesize);
  end;
end;

procedure ISPWriteEEPROM(adres:integer; dana:byte);
var data:array[0..3] of byte;
begin
  if (proctype = PROC_TYPE_AVR) or (proctype = PROC_TYPE_NEW51) or (proctype = PROC_TYPE_S8253) then
  begin
    // AVR / AT89S8253
    data[0] := $C0;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := dana;
    WriteBytes(@data, 4);
    Sync;
  end
  else
  begin
    // AT89S53 / AT89S8252
    data[0] := (Hi(word(adres)) shl 3) or 6;
    data[1] := Lo(word(adres));
    data[2] := dana;
    WriteBytes(@data, 3);
    Sync;
  end;
  WaitForReadyEEPROM(adres,dana);
end;

procedure ISPWriteEEPROMPage(adres:integer; dane:pointer);
var
  pagesize, pagemask, raddr:integer;
  data:array[0..3] of byte;
  ptr:^byte;
begin
  if proctype = PROC_TYPE_AVR then
  begin
    pagesize:=Signatures[devicenr].epagesize;
    pagemask:=pagesize - 1;
    ptr:=dane;
    for raddr:=adres to adres + pagesize - 1 do
    begin
      // Load EEPROM Memory Page
      data[0] := $c1;
      data[1] := Hi(word(raddr and pagemask));
      data[2] := Lo(word(raddr and pagemask));
      data[3] := ptr^;
      WriteBytes(@data, 4);
      Inc(ptr);
    end;
    // Write EEPROM Memory Page
    data[0] := $c2;
    data[1] := Hi(word(adres and (not pagemask)));
    data[2] := Lo(word(adres and (not pagemask)));
    data[3] := 0;
    WriteBytes(@data, 4);
    Sync;
    ptr:=dane;
    WaitForReadyEEPROM(adres, ptr^);
  end;
end;

end.
