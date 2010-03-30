unit UIPAddress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIPAddress }

  TIPAddress = class
    Address: Cardinal;
    Prefix: Byte;
    function GetNetMask: Cardinal;
    function GetString: string;
    procedure SetString(Value: string);
    constructor Create;
    property AsString: string read GetString write SetString;
  end;

implementation

{ TIPAddress }

function TIPAddress.GetNetMask: Cardinal;
begin
  Result := Cardinal((1 shl (32 - Prefix)) - 1) xor $ffffffff;
end;

function TIPAddress.GetString: string;
begin
  Result := IntToStr((Address shr 24) and $ff) + '.' + IntToStr((Address shr 16) and $ff) + '.' +
    IntToStr((Address shr 8) and $ff) + '.' + IntToStr((Address shr 0) and $ff);
  if(Prefix <> 32) then Result := Result + '/' + IntToStr(Prefix);
end;

procedure TIPAddress.SetString(Value: string);
var
  Part: string;
begin
  if Pos('/', Value) > 0 then begin
    Prefix := StrToInt(Copy(Value, Pos('/', Value) + 1, Length(Value)));
    Delete(Value, Pos('/', Value), Length(Value));
  end else Prefix := 32;
  Part := Copy(Value, 1, Pos('.', Value) - 1);
  Delete(Value, 1, Pos('.', Value));
  Address := Cardinal(StrToInt(Part) and $ff) shl 24;
  Part := Copy(Value, 1, Pos('.', Value) - 1);
  Delete(Value, 1, Pos('.', Value));
  Address := Address or Cardinal((StrToInt(Part) and $ff) shl 16);
  Part := Copy(Value, 1, Pos('.', Value) - 1);
  Delete(Value, 1, Pos('.', Value));
  Address := Address or Cardinal((StrToInt(Part) and $ff) shl 8);
  Address := Address or Cardinal((StrToInt(Value) and $ff) shl 0);
end;

constructor TIPAddress.Create;
begin
  Address := 0;
  Prefix := 32;
end;

end.

