unit ISP_Fusebits;

{$MODE Delphi}

interface

function  ISPReadFuseBits(var fblo,fbhi,fbext:byte):integer;
procedure ISPWriteFuseLoBits(fuse:byte);
procedure ISPWriteFuseHiBits(fuse:byte);
procedure ISPWriteFuseExtBits(fuse:byte);
function  AnyFuseLo:boolean;
function  AnyFuseHi:boolean;
function  AnyFuseExt:boolean;
function  AnyFuse:boolean;

implementation

uses Delays, Globals, Processors, SPI;

function ISPReadFuseBits(var fblo,fbhi,fbext:byte):integer;
var data:array[0..2] of byte;
begin
  if not AnyFuse then
  begin
    Result:=-1;
    Exit;  // no fuse bits accessible by ISP
  end;
  Result:=0;
  fblo:=0;
  fbhi:=0;
  fbext:=0;
  case Signatures[devicenr].algo_lb of
    ALGO_LB_89S8253, ALGO_LB_89S2051:
    begin
      data[0] := $21;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      fblo:=ReadByte and $0f;
    end;
    ALGO_LB_2323:
    begin
      data[0] := $58;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      fblo:=ReadByte and $01;
    end;
    ALGO_LB_2333:
    begin
      data[0] := $a0;
      data[1] := $00;
      data[2] := $00;
      WriteBytes(@data, 3);
      fblo:=ReadByte;
    end;
    ALGO_LB_MEGA, ALGO_LB_TINY:
    begin
      if AnyFuseLo then
      begin
        data[0] := $50;
        data[1] := $00;
        data[2] := $00;
        WriteBytes(@data, 3);
        fblo:=ReadByte;
      end;
      if AnyFuseHi then
      begin
        data[0] := $58;
        data[1] := $08;
        data[2] := $00;
        WriteBytes(@data, 3);
        fbhi:=ReadByte;
      end;
      if AnyFuseExt then
      begin
        data[0] := $50;
        data[1] := $08;
        data[2] := $00;
        WriteBytes(@data, 3);
        fbext:=ReadByte;
      end;
    end;
  else
    Result:=-1;  // no ISP command to read fuse bits
  end;
end;

procedure ISPWriteFuseLoBits(fuse:byte);
var data:array[0..3] of byte;
begin
  if not AnyFuseLo then
    Exit;
  case Signatures[devicenr].algo_lb of
    ALGO_LB_89S8253, ALGO_LB_89S2051:
    begin
      data[0] := $ac;
      data[1] := $10 or (fuse and $0f);
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_MEGA, ALGO_LB_TINY:
    begin
      data[0] := $ac;
      data[1] := $a0;
      data[2] := $00;
      data[3] := fuse;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_2323:
    begin
      data[0] := $ac;
      data[1] := $be or (fuse and $01);
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
    ALGO_LB_2333:
    begin
      data[0] := $ac;
      data[1] := $a0 or (fuse and $1f);
      data[2] := $00;
      data[3] := $00;
      WriteBytes(@data, 4);
      Sync;
      WaitMS(Signatures[devicenr].prog_time);
    end;
  end;
end;

procedure ISPWriteFuseHiBits(fuse:byte);
var data:array[0..3] of byte;
begin
  if not AnyFuseHi then
    Exit;
  if Signatures[devicenr].algo = ALGO_MEGA then
  begin
    data[0] := $ac;
    data[1] := $a8;
    data[2] := $00;
    data[3] := fuse;
    WriteBytes(@data, 4);
    Sync;
    WaitMS(Signatures[devicenr].prog_time);
  end;
end;

procedure ISPWriteFuseExtBits(fuse:byte);
var data:array[0..3] of byte;
begin
  if not AnyFuseExt then
    Exit;
  if Signatures[devicenr].algo = ALGO_MEGA then
  begin
    data[0] := $ac;
    data[1] := $a4;
    data[2] := $00;
    data[3] := fuse;
    WriteBytes(@data, 4);
    Sync;
    WaitMS(Signatures[devicenr].prog_time);
  end;
end;

function AnyFuseLo:boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to 7 do
    if Signatures[devicenr].fusebitslo[i] <> '' then
      Exit;
  Result:=false;
end;

function AnyFuseHi:boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to 7 do
    if Signatures[devicenr].fusebitshi[i] <> '' then
      Exit;
  Result:=false;
end;

function AnyFuseExt:boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to 7 do
    if Signatures[devicenr].fusebitsext[i] <> '' then
      Exit;
  Result:=false;
end;

function AnyFuse:boolean;
var i:integer;
begin
  Result:=true;
  for i:=0 to 7 do
    if (Signatures[devicenr].fusebitslo[i] <> '') or
       (Signatures[devicenr].fusebitshi[i] <> '') or
       (Signatures[devicenr].fusebitsext[i] <> '') then
      Exit;
  Result:=false;
end;

end.
