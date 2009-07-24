unit USerialPort;

interface

uses
  Classes, CommPort, UPin, UMemoryStreamEx;

type
  TSerialPort = class(TCommPort)
  private
    FTxCount: Integer;
    FRxCount: Integer;
    procedure Receive(Stream: TStream);
    procedure CommportDataAvail(Sender: TObject; Count: Word);
  public
    DataPin: TPin;
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    property TxCount: Integer read FTxCount;
    property RxCount: Integer read FRxCount;
  end;


implementation


{ TSerialPort }

procedure TSerialPort.CommportDataAvail(Sender: TObject; Count: Word);
var
  Data: TMemoryStreamEx;
  Buffer: array of Byte;
  C: Integer;
begin
  if Count > 0 then begin
    Inc(FRxCount, Count);
    Data := TMemoryStreamEx.Create;
    SetLength(Buffer, Count);
    C := GetBlock(Buffer[0], Count);
    Data.Write(Buffer[0], C);
    DataPin.Send(Data);
    Data.Free;
  end;
end;

constructor TSerialPort.Create(AOwner: TComponent);
begin
  inherited;
  DataPin := TPin.Create;
  DataPin.OnReceive := Receive;
  OnTriggerAvail := CommportDataAvail;;
end;

destructor TSerialPort.Destroy;
begin
  DataPin.Free;
  inherited;
end;

procedure TSerialPort.Receive(Stream: TStream);
var
  C: Integer;
begin
  Inc(FTxCount, Stream.Size);
  //repeat
    C := PutStream(Stream)
  //until C = 0;
end;

end.
