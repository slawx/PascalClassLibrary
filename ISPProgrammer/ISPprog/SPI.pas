unit SPI;

{$MODE Delphi}

interface

function  WriteReadByte(n:byte):byte;
procedure WriteByte(n:byte);
function  ReadByte:byte;
procedure WriteBytes(ptr:pointer; len:integer);
procedure ReadBytes(ptr:pointer; len:integer);
procedure WriteReadBytes(writeptr, readptr:pointer; len:integer);
procedure Sync;

implementation

uses Globals, Delays, PinsIO;

procedure WaitCLK(i:integer);
begin
  if MCUfreq <> 0 then
  begin
    if proctype = PROC_TYPE_AVR then  // AVR
      WaitNS(i*tCLK_AVR)
    else
      WaitNS(i*tCLK_8252);
  end;
end;

function WriteReadByte(n:byte):byte;
var
  x,y:byte;
begin
  x:=0;
  for y:=0 to 7 do
  begin
    if (n and 128) = 128 then
      Send1
    else
      Send0;
    if proctype = PROC_TYPE_AVR then
      WaitCLK(2)
    else
      WaitCLK(1);
    ClkHi;
    if proctype = PROC_TYPE_AVR then
      WaitCLK(4)
    else
      WaitCLK(1);
    x:=x shl 1;
    if ReadBit then
      x:=x or 1;
    WaitCLK(1); ////
    ClkLo;
    WaitCLK(1);
    n:=(n shl 1) and 255;
  end;
  Result:=x;
end;

procedure WriteByte(n:byte);
begin
  WriteReadByte(n);
end;

function ReadByte:byte;
var
  x,y:byte;
begin
  x:=0;
  for y:=0 to 7 do
  begin
    ClkHi;
    if proctype = PROC_TYPE_AVR then
      WaitCLK(4)
    else
      WaitCLK(1);
    x:=x shl 1;
    if ReadBit then
      x:=x or 1;
    WaitCLK(1); ////
    ClkLo;
    WaitCLK(3);
  end;
  Result:=x;
end;

procedure WriteBytes(ptr:pointer; len:integer);
var
  b:^byte;
begin
  b := ptr;
  while len > 0 do
  begin
    WriteByte (b^);
    Inc (b);
    Dec (len);
  end;
end;

procedure ReadBytes(ptr:pointer; len:integer);
var
  b:^byte;
begin
  b := ptr;
  while len > 0 do
  begin
    b^ := ReadByte;
    Inc (b);
    Dec (len);
  end;
end;

procedure WriteReadBytes(writeptr, readptr:pointer; len:integer);
var
  wb, rb:^byte;
begin
  wb := writeptr;
  rb := readptr;
  while len > 0 do
  begin
    rb^ := WriteReadByte (wb^);
    Inc (wb);
    Inc (rb);
    Dec (len);
  end;
end;

procedure Sync;
begin
  //
end;

end.
