unit UCommPin;

{$mode Delphi}{$H+}{$M+}

interface

uses
  Classes;

type
  TDataDiretion = (ddReceive, ddSend);
  TOnLogDataEvent = procedure (Stream: TStream; Direction: TDataDiretion) of object;
  TOnStreamEvent = procedure (Stream: TStream) of object;

  { TCommPin }

  TCommPin = class
  private
    FOnLogData: TOnLogDataEvent;
    FOnReceive: TOnStreamEvent;
    function GetConnected: Boolean;
  public
    RemotePin: TCommPin;
    destructor Destroy; override;
    procedure Connect(Pin: TCommPin);
    procedure Disconnect;
    procedure Send(Stream: TStream);
    procedure Receive(Stream: TStream);
    property OnReceive: TOnStreamEvent read FOnReceive write FOnReceive;
    property Connected: Boolean read GetConnected;
    property OnLogData: TOnLogDataEvent read FOnLogData write FOnLogData;
  end;


implementation

{ TCommPin }

procedure TCommPin.Connect(Pin: TCommPin);
begin
  if Assigned(Pin) then Disconnect;
  Self.RemotePin := Pin;
  Pin.RemotePin := Self;
end;

destructor TCommPin.Destroy;
begin
  Disconnect;
  inherited;
end;

procedure TCommPin.Disconnect;
begin
  if Assigned(RemotePin) then begin
    RemotePin.RemotePin := nil;
    RemotePin := nil;
  end;
end;

function TCommPin.GetConnected: Boolean;
begin
  Result := Assigned(RemotePin);
end;

procedure TCommPin.Receive(Stream: TStream);
begin
  if Assigned(FOnLogData) then FOnLogData(Stream, ddReceive);
  Stream.Position := 0;
  if Assigned(FOnReceive) then FOnReceive(Stream);
end;

procedure TCommPin.Send(Stream: TStream);
begin
  if Assigned(FOnLogData) then FOnLogData(Stream, ddSend);
  if Assigned(RemotePin) then RemotePin.Receive(Stream);
end;


end.
