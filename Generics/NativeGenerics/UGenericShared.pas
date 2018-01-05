unit UGenericShared;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils; 

function FastCompareMem(P1, P2: Pointer; Length: cardinal): Boolean; inline;
function FastCompareMem2(P1, P2: Pointer; Length: cardinal): Boolean; inline;

implementation

function FastCompareMem(P1, P2: Pointer; Length: cardinal): Boolean;
var
  i: cardinal;
begin
  Result:=True;
  I:=0;
  If (P1)<>(P2) then
    While Result and (i<Length) do
      begin
      Result:=PByte(P1)^=PByte(P2)^;
      Inc(I);
      Inc(pchar(P1));
      Inc(pchar(P2));
      end;
end;

function FastCompareMem2(P1, P2: Pointer; Length: cardinal): Boolean;
var
  i: cardinal;
begin
  Result:=True;
  I:=0;
  If (P1)<>(P2) then
    While Result and (i<Length) do
      begin
      Result:=PByte(P1)^=PByte(P2)^;
      Inc(I);
      Inc(pchar(P1));
      Inc(pchar(P2));
      end;
end;


end.

