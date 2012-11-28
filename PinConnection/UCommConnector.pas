unit UCommConnector;

{$mode delphi}

interface

uses
  Classes, SysUtils, UCommPin, UCommSerialPort, UCommTCPClient, UCommThread,
  UCommHub, UCommTCPServer, UCommTelnet,
  UCommTelnetComPortOption;

type
  TConnectionType = (ctNone, ctSerialPort, ctNetworkClient, ctDemo, ctNetworkServer);

  { TDeviceConnector }

  TDeviceConnector = class
  private
    FActive: Boolean;
    FConnectionType: TConnectionType;
    FTelnetComControl: Boolean;
    function GetActive: Boolean;
    function GetPin: TCommPin;
    function GetConnectionType: TConnectionType;
    procedure SetConnectionType(const AValue: TConnectionType);
    procedure SetTelnetComControl(AValue: Boolean);
    procedure UpdateConnectionType(const ConnectionType: TConnectionType;
      const ATelnetComControl: Boolean);
    procedure CommTCPServerConnect(Sender: TCommTCPServer; Pin: TCommPin);
  protected
    procedure SetActive(const AValue: Boolean);
  public
    Name: string;
    TelnetOptionComPort: TTelnetOptionComPort;
    CommTelnet: TCommTelnet;
    CommSerial: TCommSerialPort;
    CommTCPClient: TCommTCPClient;
    CommTCPServer: TCommTCPServer;
    CommDemo: TCommThread;
    CommHub: TCommHub;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TDeviceConnector); virtual;
    property ConnectionType: TConnectionType read GetConnectionType
      write SetConnectionType;
    property Pin: TCommPin read GetPin;
    property TelnetComControl: Boolean read FTelnetComControl
      write SetTelnetComControl;
    property Active: Boolean read FActive write SetActive;
  end;

implementation

function TDeviceConnector.GetConnectionType: TConnectionType;
begin
  Result := FConnectionType;
end;

function TDeviceConnector.GetActive: Boolean;
begin
  Result := FActive;
end;

function TDeviceConnector.GetPin: TCommPin;
begin
  Result := TCommPin(CommHub.Pins.Last);
end;

procedure TDeviceConnector.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    case FConnectionType of
      ctSerialPort: CommSerial.Active := AValue;
      ctNetworkClient: CommTCPClient.Active := AValue;
      ctNetworkServer: CommTCPServer.Active := AValue;
      ctDemo: CommDemo.Active := AValue;
      ctNone: ;
    end;
    CommTelnet.Active := AValue;
  end else begin
    CommTelnet.Active := False;
    CommSerial.Active := False;
    CommTCPClient.Active := False;
    CommTCPServer.Active := False;
    CommDemo.Active := False;
  end;
  CommHub.Active := AValue;
  inherited;
end;

procedure TDeviceConnector.SetConnectionType(const AValue: TConnectionType);
begin
  UpdateConnectionType(AValue, FTelnetComControl);
end;

procedure TDeviceConnector.SetTelnetComControl(AValue: Boolean);
begin
  UpdateConnectionType(FConnectionType, AValue);
end;

procedure TDeviceConnector.UpdateConnectionType(const ConnectionType: TConnectionType;
  const ATelnetComControl: Boolean);
begin
  if (FTelnetComControl = ATelnetComControl) and (FConnectionType = ConnectionType) then
    Exit;
  FTelnetComControl := ATelnetComControl;
  FConnectionType := ConnectionType;

  CommSerial.Active := False;
  CommTCPClient.Active := False;
  CommTCPServer.Active := False;
  CommDemo.Active := False;
  CommTelnet.Active := False;
  case ConnectionType of
    ctSerialPort: begin
      TCommPin(CommHub.Pins.First).Connect(CommSerial.Pin);
      CommSerial.Active := FActive;
    end;
    ctNetworkClient: begin
      if FTelnetComControl then begin
        TCommPin(CommHub.Pins.First).Connect(CommTelnet.TelnetPin);
        CommTCPClient.Pin.Connect(CommTelnet.RawPin);
        CommTCPClient.Active := FActive;
        CommTelnet.Active := FActive;
      end else begin
        TCommPin(CommHub.Pins.First).Connect(CommTCPClient.Pin);
        CommTCPClient.Active := FActive;
      end;
    end;
    ctNetworkServer: begin
      CommTCPServer.OnConnect := CommTCPServerConnect;
      CommTCPServer.Active := FActive;
    end;
    ctDemo: begin
      TCommPin(CommHub.Pins.First).Connect(CommDemo.Pin);
      CommDemo.Active := FActive;
    end;
    ctNone: begin
      TCommPin(CommHub.Pins.First).Disconnect;
    end;
  end;
end;

procedure TDeviceConnector.CommTCPServerConnect(Sender: TCommTCPServer; Pin: TCommPin);
begin
  TCommPin(CommHub.Pins.First).Connect(Pin);
end;


constructor TDeviceConnector.Create;
begin
  inherited;
  TelnetOptionComPort := TTelnetOptionComPort.Create;
  CommTelnet := TCommTelnet.Create(nil);
  CommTelnet.Register(TelnetOptionComPort);
  CommSerial := TCommSerialPort.Create(nil);
  CommTCPClient := TCommTCPClient.Create(nil);
  CommTCPServer := TCommTCPServer.Create(nil);
  CommDemo := TCommThread.Create(nil);
  CommHub := TCommHub.Create(nil);
  CommHub.Pins.AddNew;
  CommHub.Pins.AddNew;
  ConnectionType := ctNone;
end;

destructor TDeviceConnector.Destroy;
begin
  Active := False;
  FreeAndNil(CommHub);
  FreeAndNil(CommSerial);
  FreeAndNil(CommTCPClient);
  FreeAndNil(CommTCPServer);
  FreeAndNil(CommDemo);
  CommTelnet.Unregister(TelnetOptionComPort);
  FreeAndNil(TelnetOptionComPort);
  FreeAndNil(CommTelnet);
  inherited;
end;

procedure TDeviceConnector.Assign(Source: TDeviceConnector);
begin
  //FActive := Source.FActive;
  Name := Source.Name;
  CommSerial.Assign(Source.CommSerial);
  CommTelnet.Assign(Source.CommTelnet);
  FConnectionType := Source.FConnectionType;
  FTelnetComControl := Source.FTelnetComControl;
  //CommSocket.Assign(Source.CommSocket);
end;



end.

