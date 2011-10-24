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
  TOnSetStatus = procedure (Sender: TCommPin; Status: Integer) of object;

  { TCommPin }

  TCommPin = class
  private
    FOnLogData: TOnLogDataEvent;
    FOnReceive: TOnStreamEvent;
    FDataTxCount: Integer;
    FDataRxCount: Integer;
    FFrameTxCount: Integer;
    FFrameRxCount: Integer;
    FOnSetStatus: TOnSetStatus;
    FStatus: Integer;
    function GetConnected: Boolean;
    procedure SetStatus(AValue: Integer);
  protected
    procedure Receive(Stream: TStream);
    procedure ReceiveStatus(AValue: Integer);
  public
    RemotePin: TCommPin;
    constructor Create;
    destructor Destroy; override;
    procedure Connect(Pin: TCommPin);
    procedure Disconnect;
    procedure Send(Stream: TStream);
    procedure ResetCounters;
    property Connected: Boolean read GetConnected;
    property OnLogData: TOnLogDataEvent read FOnLogData write FOnLogData;
    property DataTxCount: Integer read FDataTxCount;
    property DataRxCount: Integer read FDataRxCount;
    property FrameTxCount: Integer read FFrameTxCount;
    property FrameRxCount: Integer read FFrameRxCount;
    property Status: Integer read FStatus write SetStatus; // Used for general status bits such as parity bit
    property OnReceive: TOnStreamEvent read FOnReceive write FOnReceive;
    property OnSetSatus: TOnSetStatus read FOnSetStatus write FOnSetStatus;
  end;


implementation

{ TCommPin }

procedure TCommPin.Connect(Pin: TCommPin);
begin
  if Pin <> RemotePin then begin
    Pin.Disconnect;
    Disconnect;
    Self.RemotePin := Pin;
    if Assigned(Pin) then begin
      Pin.RemotePin := Self;
      RemotePin.ReceiveStatus(FStatus);
    end;
  end;
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

procedure TCommPin.SetStatus(AValue: Integer);
begin
  FStatus := AValue;
  if Assigned(RemotePin) then RemotePin.ReceiveStatus(AValue);
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

procedure TCommPin.ReceiveStatus(AValue: Integer);
begin
  if Assigned(FOnSetStatus) then FOnSetStatus(Self, AValue);
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
