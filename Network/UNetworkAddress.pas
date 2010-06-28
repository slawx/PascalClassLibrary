unit UNetworkAddress;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommon, UStringListEx;

type
  TDomainAddress = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    Levels: array of string;
    property AsString: string read GetAsString write SetAsString;
  end;

  TAddrClass = (acA, acB, acC, acD, acE);

  TIpAddress = class(TPersistent)
  private
    function GetAddrClass: TAddrClass;
    function GetAsCardinal: Cardinal;
    function GetAsString: string;
    function GetBroadcast: Boolean;
    procedure SetBroadcast(const Value: Boolean);
    procedure SetAsCardinal(const Value: Cardinal);
    procedure SetAsString(const Value: string);
  public
    Octets: array[0..3] of Byte;
    procedure Assign(Source: TPersistent); override;
    property AsCardinal: Cardinal read GetAsCardinal write SetAsCardinal;
    property AsString: string read GetAsString write SetAsString;
    property AddrClass: TAddrClass read GetAddrClass;
    property Broadcast: Boolean read GetBroadcast write SetBroadcast;
  end;

  THostAddressState = (asDomainName, asIpAddress);
  THostAddress = class(TPersistent)
  private
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  public
    State: THostAddressState;
    DomainName: TDomainAddress;
    IpAddress: TIpAddress;
    constructor Create;
    destructor Destroy; override;
    property AsString: string read GetAsString write SetAsString;
  end;

implementation

{ TIpAddress }

procedure TIpAddress.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Assigned(Source) then begin
    if Source is TIpAddress then begin
      for I := 0 to High(Octets) do
        Octets[I] := TIpAddress(Source).Octets[I];
    end else inherited;
  end else inherited;
end;

function TIpAddress.GetAddrClass: TAddrClass;
begin
  if (Octets[3] and $80) = 0 then Result := acA
  else begin
    if (Octets[3] and $40) = 0 then Result := acB
    else begin
      if (Octets[3] and $20) = 0 then Result := acC
      else begin
        if (Octets[3] and $10) = 0 then Result := acD
        else Result := acE;
      end;
    end;
  end;
end;

function TIpAddress.GetAsCardinal: Cardinal;
begin
  Result := Octets[0] or (Octets[1] shl 8) or (Octets[2] shl 16) or (Octets[3] shl 24);
end;

function TIpAddress.GetAsString: string;
begin
  Result := IntToStr(Octets[3]) + '.' + IntToStr(Octets[2]) + '.' +
    IntToStr(Octets[1]) + '.' + IntToStr(Octets[0]);
end;

function TIpAddress.GetBroadcast: Boolean;
begin
  Result := AsCardinal = High(Cardinal);
end;

procedure TIpAddress.SetAsCardinal(const Value: Cardinal);
begin
  Octets[0] := Byte(Value);
  Octets[1] := Byte(Value shr 8);
  Octets[2] := Byte(Value shr 16);
  Octets[3] := Byte(Value shr 24);
end;

procedure TIpAddress.SetAsString(const Value: string);
var
  Parts: TStringListEx;
begin
  Parts.Explode('.', Value);
  try
//    if Length(Parts) = 4 then begin
    Octets[0] := StrToInt(Parts[3]);
    Octets[1] := StrToInt(Parts[2]);
    Octets[2] := StrToInt(Parts[1]);
    Octets[3] := StrToInt(Parts[0]);
//    end else raise EConvertError.Create('String to IP address conversion error');
  except
    raise EConvertError.Create('String to IP address conversion error');
  end;
end;

procedure TIpAddress.SetBroadcast(const Value: Boolean);
begin
  AsCardinal := High(Cardinal);
end;

{ TDomainAddress }

function TDomainAddress.GetAsString: string;
var
  I: Integer;
begin
  Result := '';
  for I := High(Levels) downto 0 do
    Result := Result + '.' + Levels[I];
  Delete(Result, 1, 1);
end;

procedure TDomainAddress.SetAsString(const Value: string);
var
  StrArray: TStringListEx;
  I: Integer;
begin
  StrArray := TStringListEx.Create;
  StrArray.Explode('.', Value);
  SetLength(Levels, Length(StrArray.Text));
  for I := 0 to Length(StrArray.Text) - 1 do
    Levels[Length(StrArray.Text) - 1 - I] := StrArray[I];
  StrArray.Destroy;
end;

{ THostAddress }

constructor THostAddress.Create;
begin
  DomainName := TDomainAddress.Create;
  IpAddress := TIpAddress.Create;
  State := asDomainName;
  DomainName.AsString := 'localhost';
end;

destructor THostAddress.Destroy;
begin
  DomainName.Free;
  IpAddress.Free;
  inherited;
end;

function THostAddress.GetAsString: string;
begin
  case State of
    asDomainName: Result := DomainName.AsString;
    asIpAddress: Result := IpAddress.AsString;
  end;
end;

procedure THostAddress.SetAsString(const Value: string);
begin
  State := asIpAddress;
  try
    IpAddress.AsString := Value;
  except
    on EConvertError do State := asDomainName;
  end;
  if State = asDomainName then DomainName.AsString := Value;
end;

end.
