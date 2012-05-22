unit ISP_UserSign;

{$MODE Delphi}

interface

function  ISPReadUserSign_MakeRequest(adres:integer; buf:pointer):integer;
function  ISPReadUserSign(adres:integer):byte;
procedure ISPWriteUserSign(adres:integer; dana:byte);

implementation

uses Delays, Globals, Processors, SPI;

function ISPReadUserSign_MakeRequest(adres:integer; buf:pointer):integer;
var data:array[0..3] of byte;
begin
  Result := 0;
  if proctype = PROC_TYPE_S2051 then
  begin
    // AT89S2051/4051 User Signature Bytes
    data[0] := $22;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := 0;
    Result := 4;
  end;
  Move (data, buf^, Result);
end;

function ISPReadUserSign(adres:integer):byte;
var
  data:array[0..3] of byte;
  len:integer;
begin
  Result:=$ff;
  len:=ISPReadUserSign_MakeRequest(adres, @data);
  if len > 0 then
  begin
    WriteBytes(@data, len - 1);
    Result:=ReadByte;
  end;
end;

procedure ISPWriteUserSign(adres:integer; dana:byte);
var data:array[0..3] of byte;
begin
  if proctype = PROC_TYPE_S2051 then
  begin
    // AT89S2051/4051 User Signature Bytes
    data[0] := $42;
    data[1] := Hi(word(adres));
    data[2] := Lo(word(adres));
    data[3] := dana;
    WriteBytes(@data, 4);
    Sync;
  end;
  WaitMS(Signatures[devicenr].prog_time);
end;

end.
