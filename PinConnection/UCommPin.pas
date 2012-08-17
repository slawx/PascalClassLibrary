unit UCommPin;

{$mode Delphi}{$H+}{$M+}

interface

uses
  Classes, SpecializedList;

type
  TCommPin = class;
  TCommNode = class;

  TDataDiretion = (ddReceive, ddSend);
  TOnLogDataEvent = procedure (Stream: TListByte; Direction: TDataDiretion) of object;
  TOnStreamEvent = procedure (Sender: TCommPin; Stream: TListByte) of object;
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
    procedure Receive(Stream: TListByte);
    procedure ReceiveStatus(AValue: Integer);
  public
    RemotePin: TCommPin;
    Node: TCommNode;
    constructor Create;
    destructor Destroy; override;
    procedure Connect(Pin: TCommPin);
    procedure Disconnect;
    procedure Send(Stream: TListByte);
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

  { TCommNode }

  TCommNode = class(TComponent)
  private
  protected
    FActive: Boolean;
    procedure SetActive(const AValue: Boolean); virtual;
  public
    property Active: Boolean read FActive write SetActive;
  end;


implementation

{ TCommNode }

procedure TCommNode.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
end;

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

procedure TCommPin.Receive(Stream: TListByte);
begin
  Inc(FDataRxCount, Stream.Count);
  Inc(FFrameRxCount);
  if Assigned(FOnLogData) then FOnLogData(Stream, ddReceive);
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

procedure TCommPin.Send(Stream: TListByte);
begin
  Inc(FDataTxCount, Stream.Count);
  Inc(FFrameTxCount);
  if Assigned(FOnLogData) then FOnLogData(Stream, ddSend);
  if Assigned(RemotePin) then RemotePin.Receive(Stream);
end;


end.
