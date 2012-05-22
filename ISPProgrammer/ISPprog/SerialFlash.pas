unit SerialFlash;

{$MODE Delphi}

interface

const
  // Error codes
  ERROR_STOP = -1;
  ERROR_TIMEOUT = -2;
  ERROR_PROGRAM = -3;

procedure SerialflashUnprotectAll;
function SerialflashErase:integer;
procedure SerialflashReadFlashPage(adres:integer; dane:pointer);
function SerialflashWriteFlashPage(adres:integer; dane:pointer):integer;
procedure SerialflashReadSign(s:pointer);

implementation

uses Delays, Globals, // MainWnd
  PinsIO, Processors, SPI;

const
  // Status Register bit masks
  STATUS_SPRL = $80;  // Sector Protection Registers Locked
  STATUS_RES  = $40;
  STATUS_EPE  = $20;  // Erase/Program Error
  STATUS_WPP  = $10;  // Write Protect (/WP) Pin Status
  STATUS_SWP  = $0C;  // Software Protection Status
  STATUS_WEL  = $02;  // Write Enable Latch Status
  STATUS_BUSY = $01;  // Ready/Busy Status

  // Commands
  AT25_CMD_READ_ARRAY_FAST = $0B;
  AT25_CMD_READ_ARRAY_SLOW = $03;
  AT25_CMD_CHIP_ERASE = $60;
  AT25_CMD_64KB_BLOCK_ERASE = $D8;
  AT25_CMD_PAGE_PROGRAM = $02;
  AT25_CMD_WRITE_ENABLE = $06;
  AT25_CMD_WRITE_DISABLE = $04;
  AT25_CMD_READ_STATUS = $05;
  AT25_CMD_WRITE_STATUS = $01;
  AT25_CMD_READ_DEVICE_ID = $15;

  AT25F_CMD_CHIP_ERASE = $62;

function SerialflashReadStatus:byte;
begin
  ChipselectOn;
  WriteByte(AT25_CMD_READ_STATUS);
  Result:=ReadByte;
  ChipselectOff;
end;

procedure SerialflashReadDeviceID(var b1,b2:byte);
begin
  ChipselectOn;
  WriteByte(AT25_CMD_READ_DEVICE_ID);
  b1:=ReadByte;
  b2:=ReadByte;
  ChipselectOff;
end;

procedure SerialflashWriteEnable;
begin
  ChipselectOn;
  WriteByte(AT25_CMD_WRITE_ENABLE);
  ChipselectOff;
end;

procedure SerialflashWriteDisable;
begin
  ChipselectOn;
  WriteByte(AT25_CMD_WRITE_DISABLE);
  ChipselectOff;
end;

procedure SerialflashUnprotectAll;
var
  i:integer;
begin
  // repeat twice (first clear SPRL, next do Global Unprotect)
  for i:=0 to 1 do
  begin
    // Write Enable
    SerialflashWriteEnable;
    // Write Status Register
    ChipselectOn;
    WriteByte(AT25_CMD_WRITE_STATUS);
    WriteByte($00);  // SPRL=0, Global Unprotect
    ChipselectOff;
    WaitMS(100);     // tSR (Status Register Write Cycle Time)
  end;
end;

function SerialflashWaitForReady(timeout:integer):integer;
var
  t1:Int64;
  status:byte;
begin
  Result:=ERROR_TIMEOUT;
  t1:=0;
  Tic(t1);
  repeat
    if devicenr < 1 then
    begin
      Result:=ERROR_STOP;
      Exit;
    end;
    if TocMS(t1) > timeout then
      Exit;
    status:=SerialflashReadStatus;
  until (status and STATUS_BUSY) = 0;
  if (status and STATUS_EPE) <> 0 then
  begin
    Result:=ERROR_PROGRAM;
    Exit;
  end;
  Result:=0;
end;

function SerialflashErase:integer;
var
  blocks, i, adres:integer;
  data:array[0..3] of byte;
begin
  if devicenr < 1 then
  begin
    Result:=ERROR_STOP;
    Exit;
  end;
  blocks:=Signatures[devicenr].fsize div 65536;
  adres:=0;
  Result:=0;
  // Global Unprotect
  SerialflashUnprotectAll;
  if blocks >= 16 then
  begin
    for i:=0 to blocks - 1 do
    begin
      if devicenr < 1 then
      begin
        Result:=ERROR_STOP;
        Exit;
      end;
      // Write Enable
      SerialflashWriteEnable;
      // 64KB Block Erase
      ChipselectOn;
      data[0] := AT25_CMD_64KB_BLOCK_ERASE;
      data[1] := (adres shr 16) and $FF;
      data[2] := (adres shr 8) and $FF;
      data[3] := adres and $FF;
      WriteBytes(@data, 4);
      Sync;
      ChipselectOff;
      // wait for ready
      WaitMS(100);
      Result := SerialflashWaitForReady((Signatures[devicenr].prog_time + blocks - 1) div blocks);
      if Result <> 0 then
        Exit;
      Inc(adres, 65536);
      //MainWindow.Progress (adres, Signatures[devicenr].fsize);
      end;
  end
  else
  begin
    // Write Enable
    SerialflashWriteEnable;
    // Chip Erase
    ChipselectOn;
    if Pos('AT25F', Signatures[devicenr].name) = 1 then
      WriteByte(AT25F_CMD_CHIP_ERASE)
    else
      WriteByte(AT25_CMD_CHIP_ERASE);
    ChipselectOff;
    // wait for ready
    WaitMS(100);
    Result := SerialflashWaitForReady(Signatures[devicenr].prog_time);
  end;
end;

procedure SerialflashReadFlashPage(adres:integer; dane:pointer);
var
  data:array[0..4] of byte;
begin
  if devicenr < 1 then
    Exit;
  ChipselectOn;
  data[0] := AT25_CMD_READ_ARRAY_SLOW; //AT25_CMD_READ_ARRAY_FAST;
  data[1] := (adres shr 16) and $FF;
  data[2] := (adres shr 8) and $FF;
  data[3] := adres and $FF;
  //data[4] := $00;  // Dummy byte needed for AT25_CMD_READ_ARRAY_FAST
  WriteBytes(@data, 4 {5});
  ReadBytes(dane, Signatures[devicenr].fpagesize);
  ChipselectOff;
end;

function SerialflashWriteFlashPage(adres:integer; dane:pointer):integer;
var
  data:array[0..3] of byte;
  len, pagelen:integer;
  b:^byte;
begin
  b := dane;
  len := Signatures[devicenr].fpagesize;
  pagelen := 1 shl Signatures[devicenr].fpage;
  repeat
    if devicenr < 1 then
    begin
      Result:=ERROR_STOP;
      Exit;
    end;
    // Write Enable
    SerialflashWriteEnable;
    // Page Program
    ChipselectOn;
    data[0] := AT25_CMD_PAGE_PROGRAM;
    data[1] := (adres shr 16) and $FF;
    data[2] := (adres shr 8) and $FF;
    data[3] := adres and $FF;
    WriteBytes(@data, 4);
    WriteBytes(b, pagelen);
    Sync;
    ChipselectOff;
    // wait for ready
    Result := SerialflashWaitForReady(60000);  // 60ms max
    if Result <> 0 then
      Exit;
    Inc(adres, pagelen);
    Inc(b, pagelen);
    Dec(len, pagelen);
  until len = 0;
end;

procedure SerialflashReadSign(s:pointer);
var
  ptr:^byte;
const
  b1:byte = 0;
  b2:byte = 0;
begin
  SerialflashReadDeviceID(b1, b2);
  ptr:=s;
  if b1 = $1f then
    ptr^:=$25   // constant for AT25F memories
  else
    ptr^:=$ff;  // $FF and no Manufacturer/Device ID
  Inc(ptr);
  ptr^:=b1;
  Inc(ptr);
  ptr^:=b2;
end;

end.
