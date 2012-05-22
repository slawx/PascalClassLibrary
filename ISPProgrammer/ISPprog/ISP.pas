unit ISP;

{$MODE Delphi}

interface

const
  // Error codes
  ERROR_STOP = -1;
  ERROR_TIMEOUT = -2;
  ERROR_PROGRAM = -3;

procedure ISPEnable;
function  ISPPollReady:boolean;
function  ISPErase:integer;
procedure ISPReadSign(s:pointer);
procedure ISPReadOscCalBytes(var b0,b1,b2,b3:byte);
procedure ISPReadMemoryBlock(memtype:integer; adres:integer; buf:pointer; len:integer);

implementation

uses DataFlash, Delays, Globals, ISP_EEPROM, ISP_Flash, ISP_UserSign, MemBuffer,
  PinsIO, Processors, SerialFlash, SPI;

procedure ISPEnable;
var
  data: array[0..3] of byte;
begin
  data[0] := $AC;
  data[1] := $53;
  data[2] := $00;
  data[3] := $00;
  if proctype = PROC_TYPE_OLD51 then
    WriteReadBytes(@data, @data, 3)
  else
    WriteReadBytes(@data, @data, 4);
  Sync;
  WaitMS(30);
//  MainWindow.Caption:=IntToHex(data[0],2)+','+IntToHex(data[1],2)+','+IntToHex(data[2],2)+','+IntToHex(data[3],2);
end;

function ISPPollReady:boolean;
const data:array[0..2] of byte = ($F0, 0, 0);
begin
  WriteBytes(@data, 3);
  Result:=((ReadByte and $01) = 0);
end;

function ISPErase:integer;
var
  data:array[0..3] of byte;
begin
  Result:=0;
  if (proctype = PROC_TYPE_AVR) or (proctype = PROC_TYPE_NEW51) or
    (proctype = PROC_TYPE_S8253) or (proctype = PROC_TYPE_S2051) then
  begin
    if Signatures[devicenr].algo_erase = ALGO_ERASE_STD then
    begin
      data[0] := $AC;
      data[1] := $80;
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
    end
    else if Signatures[devicenr].algo_erase = ALGO_ERASE_TWICE then
    begin
      // first chip erase
      data[0] := $AC;
      data[1] := $80;
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
      // write $FF to address $00 in the flash
      data[0] := $40;
      data[1] := $00;
      data[2] := $00;
      data[3] := $FF;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
      // second chip erase
      data[0] := $AC;
      data[1] := $80;
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end
  end
  else if proctype = PROC_TYPE_OLD51 then
  begin
    data[0] := $AC;
    data[1] := $04;
    data[2] := $00;
    WriteBytes(@data, 3);
    Sync;
  end
  else if proctype = PROC_TYPE_DATAFLASH then
  begin
    if Signatures[devicenr].algo = ALGO_SERIALFLASH then
      Result:=SerialflashErase
    else
      DataflashErase;
    Exit;
  end
  else if proctype = PROC_TYPE_SERIALFLASH then
  begin
    Result:=SerialflashErase;
    Exit;
  end;

  if proctype = PROC_TYPE_NEW51 then
    WaitMS(500)
  else
    WaitMS(32);

  StrobeOff;
  RstOff;
  WaitMS(100);
end;

procedure ISPReadSign(s:pointer);
var
  adres, b:byte;
  ptr:^byte;
  data:array[0..2] of byte;
begin
  ptr:=s;
  if proctype = PROC_TYPE_DATAFLASH then
    DataflashReadSign(s)
  else
  if proctype = PROC_TYPE_SERIALFLASH then
    SerialflashReadSign(s)
  else
  for adres:=0 to 2 do
  begin
    b:=$ff;
    if proctype = PROC_TYPE_AVR then
    begin
      // AVR
      data[0] := $30;
      data[1] := 0;
      data[2] := adres;
      WriteBytes(@data, 3);
      b:=ReadByte;
    end
    else if proctype = PROC_TYPE_S8253 then
    begin
      // AT89S8253
      data[0] := $28;
      data[1] := 0;
      data[2] := adres + $30;
      WriteBytes(@data, 3);
      b:=ReadByte;
    end
    else if proctype = PROC_TYPE_S2051 then
    begin
      // AT89S2051 / AT89S4051
      data[0] := $28;
      data[1] := 0;
      data[2] := adres;
      WriteBytes(@data, 3);
      b:=ReadByte;
    end
    else if proctype = PROC_TYPE_NEW51 then
    begin
      // AT89S51/52
      data[0] := $28;
      data[1] := adres;
      data[2] := 0;
      WriteBytes(@data, 3);
      b:=ReadByte;
    end;
    ptr^:=b;
    Inc(ptr);
  end;
end;

procedure ISPReadOscCalBytes(var b0,b1,b2,b3:byte);
var data:array[0..2] of byte;
begin
  b0:=0;
  b1:=0;
  b2:=0;
  b3:=0;

  if Signatures[devicenr].osccal > 0 then
  begin
    data[0] := $38;
    data[1] := $00;
    data[2] := $00;
    WriteBytes(@data, 3);
    b0:=ReadByte;
  end;

  if Signatures[devicenr].osccal > 1 then
  begin
    data[0] := $38;
    data[1] := $00;
    data[2] := $01;
    WriteBytes(@data, 3);
    b1:=ReadByte;
  end;

  if Signatures[devicenr].osccal > 2 then
  begin
    data[0] := $38;
    data[1] := $00;
    data[2] := $02;
    WriteBytes(@data, 3);
    b2:=ReadByte;
  end;

  if Signatures[devicenr].osccal > 3 then
  begin
    data[0] := $38;
    data[1] := $00;
    data[2] := $03;
    WriteBytes(@data, 3);
    b3:=ReadByte;
  end;
end;

procedure ISPReadMemoryBlock(memtype:integer; adres:integer; buf:pointer; len:integer);
var
  data:array[0..4096] of byte;
  cnt, i, n, l:integer;
  ptr, bbuf:^byte;
begin
  n := 0;
  bbuf := buf;
  while len > 0 do
  begin
    // wyznaczamy rozmiar bloku
    if len > 1024 then
      cnt := 1024
    else
      cnt := len;
    // tworzymy zlecenie transferu IN/OUT
    ptr := @data;
    l := 0;
    for i := 1 to cnt do
    begin
      if memtype = BUF_FLASH then
        n := ISPReadFlash_MakeRequest(adres, ptr)
      else if memtype = BUF_EEPROM then
        n := ISPReadEEPROM_MakeRequest(adres, ptr)
      else if memtype = BUF_USERSIG then
        n := ISPReadUserSign_MakeRequest(adres, ptr);
      Inc (adres);
      Inc (ptr, n);
      Inc (l, n);
    end;
    // wykonujemy zlecenie
    WriteReadBytes(@data, @data, l);
    // kopiujemy odebrane dane do bufora
    for i := 1 to cnt do
    begin
      ptr := bbuf;
      ptr^ := data[i * n - 1];
      Inc (bbuf);
    end;
    Dec (len, cnt);
  end;
end;

end.

