unit UCommTelnetComPortOption;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommTelnet, USerialPort,
  SpecializedList, UBinarySerializer;

type
  TComPortOptionCommand = (cpcSetBaudRate = 1, cpcSetDataSize = 2,
    cpcSetParity = 3, cpcSetStopSize = 4, cpcSetControl = 5, cpcNotifyLineState = 6,
    cpcNotifyModeState = 7, cpcFlowControlSuspend = 8, cpcFlowControlResume = 9,
    cpcLineStateMask = 10, cpcModemStateMask = 11, cpcPurgeData = 12);

  { TTelnetOptionComPort }

  TTelnetOptionComPort = class(TTelnetOption)
  private
    FBaudRate: Cardinal;
    FDTR: Boolean;
    FRTS: Boolean;
    function GetDataBits: TDataBits;
    function GetDTR: Boolean;
    function GetFlowControl: TFlowControl;
    function GetParity: TParity;
    function GetRTS: Boolean;
    function GetStopBits: TStopBits;
    function GetBaudRate: Cardinal;
    procedure SetBaudRate(Value: Cardinal);
    procedure SetDataBits(AValue: TDataBits);
    procedure SetDTR(AValue: Boolean);
    procedure SetFlowControl(AValue: TFlowControl);
    procedure SetParity(AValue: TParity);
    procedure SetRTS(AValue: Boolean);
    procedure SetStopBits(AValue: TStopBits);
  protected
    procedure SetActive(AValue: Boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TTelnetOption); override;
    property FlowControl: TFlowControl read GetFlowControl write SetFlowControl;
    property DataBits: TDataBits read GetDataBits write SetDataBits;
    property StopBits: TStopBits read GetStopBits write SetStopBits;
    property Parity: TParity read GetParity write SetParity;
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;
    property RTS: Boolean read GetRTS write SetRTS;
    property DTR: Boolean read GetDTR write SetDTR;
  end;


implementation

{ TTelnetOptionComPort }

function TTelnetOptionComPort.GetDataBits: TDataBits;
begin

end;

function TTelnetOptionComPort.GetDTR: Boolean;
begin

end;

function TTelnetOptionComPort.GetFlowControl: TFlowControl;
begin

end;

function TTelnetOptionComPort.GetParity: TParity;
begin

end;

function TTelnetOptionComPort.GetRTS: Boolean;
begin

end;

function TTelnetOptionComPort.GetStopBits: TStopBits;
begin

end;

procedure TTelnetOptionComPort.SetActive(AValue: Boolean);
begin
  inherited;
  if AValue then begin
    SetBaudRate(FBaudRate);
    SetDTR(FDTR);
    SetRTS(FRTS);
    SetFlowControl(fcNone);
  end;
end;

procedure TTelnetOptionComPort.SetBaudRate(Value: Cardinal);
var
  Request: TBinarySerializer;
begin
  FBaudRate := Value;
  if Telnet.Active then
  try
    Request := TBinarySerializer.Create;
    Request.Endianness := enBig;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.WriteByte(Byte(cpcSetBaudRate));
    Request.WriteCardinal(Value);
    Telnet.SendSubCommand(tmComPortControlOption, Request.List, nil);
  finally
    Request.Free;
  end;
end;

function TTelnetOptionComPort.GetBaudRate: Cardinal;
var
  Request: TBinarySerializer;
  Response: TBinarySerializer;
begin
  if Telnet.Active then
  try
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.Endianness := enBig;
    Response := TBinarySerializer.Create;
    Response.List := TListByte.Create;
    Response.OwnsList := True;
    Response.Endianness := enBig;
    Request.WriteByte(Byte(cpcSetBaudRate));
    Request.WriteCardinal(0);
    Telnet.SendSubCommand(tmComPortControlOption, Request.List, Response.List);
    Response.Position := 0;
    Result := Response.ReadCardinal;
  finally
    Response.Free;
    Request.Free;
  end else Result := 0;
end;

procedure TTelnetOptionComPort.SetDataBits(AValue: TDataBits);
begin

end;

procedure TTelnetOptionComPort.SetDTR(AValue: Boolean);
var
  Request: TBinarySerializer;
begin
  FDTR := AValue;
  if Telnet.Active then
  try
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.WriteByte(Byte(cpcSetControl));
    if AValue then Request.WriteByte(8)
      else Request.WriteByte(9);
    Telnet.SendSubCommand(tmComPortControlOption, Request.List, nil);
  finally
    Request.Free;
  end;
end;

procedure TTelnetOptionComPort.SetFlowControl(AValue: TFlowControl);
var
  Request: TBinarySerializer;
begin
  if Telnet.Active then
  try
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.WriteByte(Byte(cpcSetControl));
    case AValue of
      fcNone: Request.WriteByte(1);
      fcSoftware: Request.WriteByte(2);
      fcHardware: Request.WriteByte(3);
    end;
    Telnet.SendSubCommand(tmComPortControlOption, Request.List, nil);
  finally
    Request.Free;
  end;
end;

procedure TTelnetOptionComPort.SetParity(AValue: TParity);
begin

end;

procedure TTelnetOptionComPort.SetRTS(AValue: Boolean);
var
  Request: TBinarySerializer;
begin
  FRTS := AValue;
  if Telnet.Active then
  try
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.WriteByte(Byte(cpcSetControl));
    if AValue then Request.WriteByte(11)
      else Request.WriteByte(12);
    Telnet.SendSubCommand(tmComPortControlOption, Request.List, nil);
  finally
    Request.Free;
  end;
end;

procedure TTelnetOptionComPort.SetStopBits(AValue: TStopBits);
begin

end;

constructor TTelnetOptionComPort.Create;
begin
  Code := tmComPortControlOption;
end;

destructor TTelnetOptionComPort.Destroy;
begin
  inherited Destroy;
end;

procedure TTelnetOptionComPort.Assign(Source: TTelnetOption);
begin
  FBaudRate := TTelnetOptionComPort(Source).FBaudRate;
  FDTR := TTelnetOptionComPort(Source).FDTR;
  FRTS := TTelnetOptionComPort(Source).FRTS;
  inherited Assign(Source);
end;

end.

