unit UPinConnection;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;


implementation

uses
  UCommMark, UCommSerialPort, UCommTCPServer, UCommTCPClient, UCommFrame,
  UPacketBurst, UCommThread, UCommDelay, UCommHub, UCommConcentrator,
  UCommTelnet;

procedure Register;
begin
  RegisterComponents('PinConnection', [TCommMark, TCommSerialPort, TCommTCPServer,
    TCommTCPClient, TCommFrame, TPacketBurst, TCommThread, TCommDelay, TCommHub,
    TCommConcentrator, TCommTelnet]);
end;

end.

