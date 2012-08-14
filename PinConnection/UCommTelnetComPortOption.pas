unit UCommTelnetComPortOption;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommPin, UCommTelnet, USerialPort, UStreamHelper,
  SpecializedList, UBinarySerializer;

type
  TComPortOptionCommand = (cpcSetBaudRate = 1, cpcSetDataSize = 2,
    cpcSetParity = 3, cpcSetStopSize = 4, cpcSetControl = 5, cpcNotifyLineState = 6,
    cpcNotifyModeState = 7, cpcFlowControlSuspend = 8, cpcFlowControlResume = 9,
    cpcLineStateMask = 10, cpcModemStateMask = 11, cpcPurgeData = 12);

  { TTelnetOptionComPort }

  TTelnetOptionComPort = class(TTelnetOption)
  private
  public
    procedure SetBaudRate(Value: Cardinal);
    function GetBaudRate: Cardinal;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TTelnetOptionComPort }

procedure TTelnetOptionComPort.SetBaudRate(Value: Cardinal);
var
  Request: TBinarySerializer;
begin
  try
    Request := TBinarySerializer.Create;
    Request.List := TListByte.Create;
    Request.OwnsList := True;
    Request.WriteByte(Byte(cpcSetBaudRate));
    Request.WriteCardinal(Value);
//    Telnet.SendSubCommand(tmComPortControlOption, Request, nil);
  finally
    Request.Free;
  end;
end;

function TTelnetOptionComPort.GetBaudRate: Cardinal;
var
  Request: TStreamHelper;
  Response: TStreamHelper;
begin
  try
    Request := TStreamHelper.Create;
    Response := TStreamHelper.Create;
    Request.WriteByte(Byte(cpcSetBaudRate));
    Request.WriteCardinal(0);
//    Telnet.SendSubCommand(tmComPortControlOption, Request, Response);
    Response.Position := 0;
    Result := Response.ReadCardinal;
  finally
    Response.Free;
    Request.Free;
  end;
end;

constructor TTelnetOptionComPort.Create;
begin
end;

destructor TTelnetOptionComPort.Destroy;
begin
  inherited Destroy;
end;

end.

