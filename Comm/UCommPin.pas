unit UCommPin;

{$mode Delphi}{$H+}{$M+}

interface

uses
  Classes;

type
  TCommPin = class;

  TDataDiretion = (ddReceive, ddSend);
  TOnLogDataEvent = procedure (Stream: TStream; Direction: TDataDiretion) of object;
  TOnStreamEvent = procedure (Sender: TCommPin; Stream: TStream) of object;

  { TCommPin }

  TCommPin = class
  private
    FOnLogData: TOnLogDataEvent;
    FOnReceive: TOnStreamEvent;
    FDataTxCount: Integer;
    FDataRxCount: Integer;
    FFrameTxCount: Integer;
    FFrameRxCount: Integer;
    function GetConnected: Boolean;
  public
    RemotePin: TCommPin;
    constructor Create;
    destructor Destroy; override;
    procedure Connect(Pin: TCommPin);
    procedure Disconnect;
    procedure Send(Stream: TStream);
    procedure Receive(Stream: TStream);
    procedure ResetCounters;
    property OnReceive: TOnStreamEvent read FOnReceive write FOnReceive;
    property Connected: Boolean read GetConnected;
    property OnLogData: TOnLogDataEvent read FOnLogData write FOnLogData;
    property DataTxCount: Integer read FDataTxCount;
    property DataRxCount: Integer read FDataRxCount;
    property FrameTxCount: Integer read FFrameTxCount;
    property FrameRxCount: Integer read FFrameRxCount;
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

constructor TCommPin.Create;
begin
  RemotePin := nil;
end;

procedure TCommPin.Receive(Stream: TStream);
begin
  Inc(FDataRxCount, Stream.Size);
  Inc(FFrameRxCount);
  if Assigned(FOnLogData) then FOnLogData(Stream, ddReceive);
  Stream.Position := 0;
  if Assigned(FOnReceive) then FOnReceive(Self, Stream);
end;

procedure TCommPin.ResetCounters;
begin
  FDataTxCount := 0;
  FDataRxCount := 0;
  FFrameTxCount := 0;
  FFrameRxCount := 0;
end;

procedure TCommPin.Send(Stream: TStream);
begin
  Inc(FDataTxCount, Stream.Size);
  Inc(FFrameTxCount);
  if Assigned(FOnLogData) then FOnLogData(Stream, ddSend);
  if Assigned(RemotePin) then RemotePin.Receive(Stream);
end;


end.
