unit DataFlash;

{$MODE Delphi}

interface

function DataflashReadStatus:byte;
procedure DataflashErase;
procedure DataflashReadFlashPage(adres:integer; dane:pointer);
procedure DataflashWriteFlashPage(adres:integer; dane:pointer);
procedure DataflashReadSign(s:pointer);

implementation

uses Delays, Globals, PinsIO, Processors, SPI;

function DataflashReadStatus:byte;
begin
  ChipselectOn;
  WriteByte($d7);
  Result:=ReadByte;
  ChipselectOff;
end;

procedure DataflashReadDeviceID(var b1,b2,b3:byte);
begin
  ChipselectOn;
  WriteByte($9f);
  b1:=ReadByte;
  b2:=ReadByte;
  b3:=ReadByte;
  ChipselectOff;
end;

procedure DataflashWaitForReady;
const
  t1:Int64 = 0;
begin
  Tic(t1);
  repeat until ((DataflashReadStatus and $80) = $80) or (TocMS(t1) > 100);
end;

procedure DataflashErase;
var
  pagesize_big, pages, blocks, blockshift, i:integer;
  data:array[0..3] of byte;
begin
  if devicenr < 1 then
    Exit;
  pagesize_big:=(1 shl (Signatures[devicenr].fpage - 1)) + (1 shl (Signatures[devicenr].fpage - 6));
  pages:=Signatures[devicenr].fsize div pagesize_big;
  blocks:=pages div 8;
  blockshift:=Signatures[devicenr].fpage - 5;
  for i:=0 to blocks - 1 do
  begin
    ChipselectOn;
    data[0] := $50;
    data[1] := Hi(word(i shl blockshift));
    data[2] := Lo(word(i shl blockshift));
    data[3] := $00;
    WriteBytes(@data, 4);
    Sync;
    ChipselectOff;
    DataflashWaitForReady;
  end;
end;

procedure DataflashReadFlashPage(adres:integer; dane:pointer);
var
  pagesize, pagenum:integer;
  data:array[0..7] of byte;
begin
  if devicenr < 1 then
    Exit;
  pagesize:=Signatures[devicenr].fpagesize;
  pagenum:=adres div pagesize;
  adres:=pagenum shl Signatures[devicenr].fpage;
  ChipselectOn;
  data[0] := $D2;
  data[1] := (adres shr 16) and $FF;
  data[2] := (adres shr 8) and $FF;
  data[3] := adres and $FF;
  data[4] := $00;
  data[5] := $00;
  data[6] := $00;
  data[7] := $00;
  WriteBytes(@data, 8);
  ReadBytes(dane, pagesize);
  ChipselectOff;
end;

procedure DataflashWriteFlashPage(adres:integer; dane:pointer);
var
  pagesize:integer;
  data:array[0..3] of byte;
begin
  if devicenr < 1 then
    Exit;
  pagesize:=Signatures[devicenr].fpagesize;
  adres:=(adres div pagesize) shl Signatures[devicenr].fpage;
  ChipselectOn;
  data[0] := $82;
  data[1] := (adres shr 16) and $FF;
  data[2] := (adres shr 8) and $FF;
  data[3] := adres and $FF;
  WriteBytes(@data, 4);
  WriteBytes(dane, pagesize);
  Sync;
  ChipselectOff;
  DataflashWaitForReady;
end;

procedure DataflashReadSign(s:pointer);
var
  b:byte;
  ptr:^byte;
const
  data:array[0..2] of byte = (0, 0, 0);
begin
  ptr:=s;
  b:=DataflashReadStatus;
  DataflashReadDeviceID(data[0], data[1], data[2]);
  if (data[0] <> $ff) and (data[1] <> $ff) and (data[2] <> $ff) then
    ptr^:=b and $3d   // memory size mask and page size bit
  else if b <> $ff then
    ptr^:=b and $3c   // memory size mask only
  else
    ptr^:=b;          // $FF and no Manufacturer/Device ID
  Inc(ptr);
  ptr^:=data[0];
  Inc(ptr);
  ptr^:=data[1];
end;

end.
