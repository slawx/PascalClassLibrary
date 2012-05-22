unit ISP_Flash;

{$MODE Delphi}

interface

procedure WaitForReadyFlash(adres:integer; value:byte);
procedure ISPLoadExtendedAddress(adres:integer);
function  ISPReadFlash_MakeRequest(adres:integer; buf:pointer):integer;
function  ISPReadFlash(adres:integer):byte;
procedure ISPReadFlashPage(adres:integer; dane:pointer);
procedure ISPWriteFlash(adres:integer; dana:byte);
procedure ISPWriteFlashPage(adres:integer; dane:pointer);
procedure ISPWriteProgramPage(adres:integer; value:byte);

implementation

uses DataFlash, Delays, Globals, ISP, MemBuffer, Processors,
  SerialFlash, SPI;

procedure WaitForReadyFlash(adres:integer; value:byte);
var
  b:byte;
const
  t1:Int64 = 0;
begin
  case Signatures[devicenr].algo_busy of
    ALGO_BUSY_WAIT:
      WaitMS(Signatures[devicenr].prog_time);

    ALGO_BUSY_POLL_00FF, ALGO_BUSY_POLL_FF, ALGO_BUSY_POLL_NMSB:
    begin
      if value <> $ff then
      begin
        WaitMS (1);
        Tic(t1);
        repeat
          b:=ISPReadFlash(adres);
        until (b = value) or (TocMS(t1) > 100);
      end
      else
        WaitMS(Signatures[devicenr].prog_time);
    end;

    ALGO_BUSY_POLL_RDYBSY:
    begin
      Tic(t1);
      repeat until (ISPPollReady) or (TocMS(t1) > 100);
//      MainWindow.Caption:=IntToStr(TocMS(t1));
    end;
  end;
end;

procedure ISPLoadExtendedAddress(adres:integer);
var data:array[0..3] of byte;
begin
  data[0] := $4D;
  data[1] := $00;
  data[2] := (adres shr 17) and $ff;
  data[3] := $00;
  WriteBytes(@data, 4);
  Sync;
end;

function ISPReadFlash_MakeRequest(adres:integer; buf:pointer):integer;
var data:array[0..3] of byte;
begin
  Result := 0;
  if proctype = PROC_TYPE_AVR then
  begin
    data[0] := $20 or ((adres and 1) shl 3);
    data[1] := Hi(word(adres div 2));
    data[2] := Lo(word(adres div 2));
    data[3] := 0;
    Result := 4;
  end
  else if (proctype = PROC_TYPE_NEW51) or (proctype = PROC_TYPE_S8253) or
    (proctype = PROC_TYPE_S2051) then
  begin
    data[0] := $20;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := 0;
    Result := 4;
  end
  else if proctype = PROC_TYPE_OLD51 then
  begin
    // AT89S53/8252
    data[0] := ((adres shr 5) and $f8) or ((adres shr 11) and $04) or $01;
    data[1] := Lo(word(adres));
    data[2] := 0;
    Result := 3;
  end;
  Move (data, buf^, Result);
end;

function ISPReadFlash(adres:integer):byte;
var
  data:array[0..3] of byte;
  len:integer;
begin
  Result:=$ff;
  len:=ISPReadFlash_MakeRequest(adres, @data);
  if len > 0 then
  begin
    WriteBytes(@data, len - 1);
    Result:=ReadByte;
  end;
end;

procedure ISPReadFlashPage(adres:integer; dane:pointer);
var
  pagesize, pagemask:integer;
  data:array[0..7] of byte;
begin
  if (proctype = PROC_TYPE_S8253) or (proctype = PROC_TYPE_S2051) or (proctype = PROC_TYPE_NEW51) then
  begin
    pagesize:=Signatures[devicenr].fpagesize;
    pagemask:=pagesize - 1;
    data[0] := $30;
    data[1] := Hi(word(adres));
    if proctype = PROC_TYPE_NEW51 then
      WriteBytes(@data, 2)
    else
    begin
      data[2] := Lo(word(adres)) and (pagemask xor $ff);
      WriteBytes(@data, 3);
    end;
    ReadBytes(dane, pagesize);
  end
  else if proctype = PROC_TYPE_AVR then
  begin
    pagesize:=Signatures[devicenr].fpagesize;
    ISPReadMemoryBlock(BUF_FLASH, adres, dane, pagesize);
  end
  else if proctype = PROC_TYPE_DATAFLASH then
  begin
    if Signatures[devicenr].algo = ALGO_SERIALFLASH then
      SerialflashReadFlashPage(adres, dane)
    else
      DataflashReadFlashPage(adres, dane);
  end
  else if proctype = PROC_TYPE_SERIALFLASH then
  begin
    SerialflashReadFlashPage(adres, dane);
  end;
end;

procedure ISPWriteFlash(adres:integer; dana:byte);
var data:array[0..3] of byte;
begin
  if proctype = PROC_TYPE_AVR then
  begin
    // AVR / ATmega
    data[0] := $40 or ((adres and 1) shl 3);
    data[1] := Hi(word(adres div 2));
    data[2] := Lo(word(adres div 2));
    data[3] := dana;
    WriteBytes(@data, 4);
    Sync;
    if Signatures[devicenr].algo = ALGO_STD then
      WaitForReadyFlash(adres, dana);
  end
  else if (proctype = PROC_TYPE_NEW51) or (proctype = PROC_TYPE_S8253) or
    (proctype = PROC_TYPE_S2051) then
  begin
    // AT89S51 / AT89S52 / AT89S8253 / AT89S2051 / AT89S4051
    data[0] := $40;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := dana;
    WriteBytes(@data, 4);
    Sync;
    WaitForReadyFlash(adres, dana);
  end
  else
  begin
    // AT89S53 / AT89S8252
    data[0] := ((adres shr 5) and $f8) or ((adres shr 11) and $04) or $02;
    data[1] := Lo(word(adres));
    data[2] := dana;
    WriteBytes(@data, 3);
    Sync;
    WaitForReadyFlash(adres, dana);
  end;
end;

procedure ISPWriteFlashPage(adres:integer; dane:pointer);
var
  pagesize, pagemask, raddr:integer;
  data:array[0..3] of byte;
  ptr:^byte;
begin
  if (proctype = PROC_TYPE_S8253) or (proctype = PROC_TYPE_S2051) or (proctype = PROC_TYPE_NEW51) then
  begin
    pagesize:=Signatures[devicenr].fpagesize;
    pagemask:=pagesize - 1;
    data[0] := $50;
    data[1] := Hi(word(adres));
    if proctype = PROC_TYPE_NEW51 then
      WriteBytes(@data, 2)
    else
    begin
      data[2] := Lo(word(adres)) and (pagemask xor $ff);
      WriteBytes(@data, 3);
    end;
    WriteBytes(dane, pagesize);
    Sync;
    WaitMS(Signatures[devicenr].prog_time);
  end
  else if proctype = PROC_TYPE_AVR then
  begin
    pagesize:=Signatures[devicenr].fpagesize;
    ptr:=dane;
    for raddr:=adres to adres + pagesize - 1 do
    begin
      // Load Program Memory Page
      data[0] := $40 or ((raddr and 1) shl 3);
      data[1] := Hi(word(raddr div 2));
      data[2] := Lo(word(raddr div 2));
      data[3] := ptr^;
      WriteBytes(@data, 4);
      Inc(ptr);
    end;
    // Write Program Memory Page
    data[0] := $4c;
    data[1] := Hi(word(adres div 2));
    data[2] := Lo(word(adres div 2));
    data[3] := 0;
    WriteBytes(@data, 4);
    Sync;
    ptr:=dane;
    WaitForReadyFlash(adres, ptr^);
  end
  else if proctype = PROC_TYPE_DATAFLASH then
  begin
    if Signatures[devicenr].algo = ALGO_SERIALFLASH then
      SerialflashWriteFlashPage(adres, dane)
    else
      DataflashWriteFlashPage(adres, dane);
  end
  else if proctype = PROC_TYPE_SERIALFLASH then
  begin
    SerialflashWriteFlashPage(adres, dane);
  end;
end;

procedure ISPWriteProgramPage(adres:integer; value:byte);
var data:array[0..3] of byte;
begin
  data[0] := $4c;
  data[1] := Hi(word(adres div 2));
  data[2] := Lo(word(adres div 2));
  data[3] := 0;
  WriteBytes(@data, 4);
  Sync;
  WaitForReadyFlash(adres, value);
end;

end.
