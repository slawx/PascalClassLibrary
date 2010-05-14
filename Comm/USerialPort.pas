unit USerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, SynaSer, StdCtrls, Windows, Dialogs;

type
  TBaudRate = (br110, br300, br600, br1200, br2400, br4800,
    br9600, br14400, br19200, br38400, br56000,
    br57600, br115200, br128000, br256000);
  TParity = (paNone, paOdd, paEven, paMark, paSpace);
  TStopbits = (sb1_0, sb1_5, sb2_0);
  TDatabits = Integer;
  TFlowControl = (fcNone, fcSoftware, fcHardware);

  TSerialPort = class;
  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TSerialPortReceiveThread }

  TSerialPortReceiveThread = class(TThread)
    Parent: TSerialPort;
    Stream: TMemoryStream;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TSerialPort }

  TSerialPort = class(TBlockSerial)
  private
    FRTS: Boolean;
    FDTR: Boolean;
    FActive: Boolean;
    FBaudRate: TBaudRate;
    FDataBits: TDataBits;
    FFlowControl: TFlowControl;
    FName: string;
    FOnReceiveData: TReceiveDataEvent;
    FParity: TParity;
    FStopBits: TStopBits;
    FReceiveThread: TSerialPortReceiveThread;
    function GetBaudRateNumeric: Integer;
    function GetName: string;
    procedure SetBaudRate(const AValue: TBaudRate);
    procedure SetBaudRateNumeric(const AValue: Integer);
    procedure SetDataBits(const AValue: TDataBits);
    procedure SetDTR(const AValue: Boolean);
    procedure SetFlowControl(const AValue: TFlowControl);
    procedure SetActive(const AValue: Boolean);
    procedure SetName(const AValue: string);
    procedure SetParity(const AValue: TParity);
    procedure SetRTS(const AValue: Boolean);
    procedure SetStopBits(const AValue: TStopBits);
    procedure Open;
    procedure Close;
  public
    property Name: string read GetName write SetName;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl;
    property DataBits: TDataBits read FDataBits write SetDataBits;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    property Parity: TParity read FParity write SetParity;
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate;
    property Active: Boolean read FActive write SetActive;
    property RTS: Boolean read FRTS write SetRTS;
    property DTR: Boolean read FDTR write SetDTR;

    property BaudRateNumeric: Integer read GetBaudRateNumeric write SetBaudRateNumeric;
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    constructor Create;
    destructor Destroy; override;
  end;

const
  BaudRateNumericTable: array[TBaudRate] of Integer = (
    110, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 38400, 56000,
    57600, 115200, 128000, 256000);
  INVALID_HANDLE_VALUE = DWORD(-1);

implementation

{ TSerialPort }

procedure TSerialPort.SetActive(const AValue: Boolean);
begin
  if not FActive and AValue then begin
    FActive := True;
    Open;
    FActive := FHandle <> INVALID_HANDLE_VALUE
  end else
  if FActive and not AValue then begin
    FActive := False;
    Close;
  end;
end;

procedure TSerialPort.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TSerialPort.SetParity(const AValue: TParity);
begin
  FParity := AValue;
  if FActive then begin
    GetCommState;
    DCB.Parity := Integer(AValue);
    SetCommState;
  end;
end;

procedure TSerialPort.SetRTS(const AValue: Boolean);
begin
  FRTS := AValue;
  if FActive then SetRTSF(FRTS);
end;

procedure TSerialPort.SetStopBits(const AValue: TStopBits);
begin
  FStopBits := AValue;
  if FActive then begin
    GetCommState;
    DCB.StopBits := Integer(AValue);
    SetCommState;
  end;
end;

procedure TSerialPort.Open;
var
  DefaultCommConfig: COMMCONFIG;
  Size: LongWord;
  Port: PChar;
begin
  Connect(FName);
  SetBaudRate(FBaudRate);
  SetParity(FParity);
  SetStopBits(FStopBits);
  SetDataBits(FDataBits);
  SetFlowControl(FFlowControl);
  SetDTR(FDTR);
  SetRTS(FRTS);
end;

procedure TSerialPort.Close;
begin
  CloseSocket;
end;

constructor TSerialPort.Create;
begin
  inherited Create;
  FBaudRate := br9600;
  FName := 'COM1';
  FDataBits := 8;
  FStopBits := sb1_0;
  FParity := paNone;
  FlowControl := fcNone;
  FDTR := False;
  FRTS := False;
  Active := False;

  FReceiveThread := TSerialPortReceiveThread.Create(True);
  FReceiveThread.FreeOnTerminate := False;
  FReceiveThread.Parent := Self;
  FReceiveThread.Resume;
end;

destructor TSerialPort.Destroy;
begin
  FReceiveThread.Terminate;
  FReceiveThread.WaitFor;
  FReceiveThread.Destroy;
  inherited Destroy;
end;

procedure TSerialPort.SetBaudRate(const AValue: TBaudRate);
begin
  FBaudRate := AValue;
  if FActive then begin
    GetCommState;
    DCB.BaudRate := BaudRateNumeric;
    SetCommState;
  end;
end;

function TSerialPort.GetBaudRateNumeric: Integer;
begin
  Result := BaudRateNumericTable[FBaudRate];
end;

function TSerialPort.GetName: string;
begin
  Result := FName;
end;

procedure TSerialPort.SetBaudRateNumeric(const AValue: Integer);
begin
  case AValue of
    110: BaudRate := br110;
    300: BaudRate := br300;
    600: BaudRate := br600;
    1200: BaudRate := br1200;
    2400: BaudRate := br2400;
    4800: BaudRate := br4800;
    9600: BaudRate := br9600;
    14400: BaudRate := br14400;
    19200: BaudRate := br19200;
    38400: BaudRate := br38400;
    56000: BaudRate := br56000;
    57600: BaudRate := br57600;
    115200: BaudRate := br115200;
    128000: BaudRate := br128000;
    256000: BaudRate := br256000;
    else raise Exception.Create('Wrong numeric baud rate');
  end;
end;

procedure TSerialPort.SetDataBits(const AValue: TDataBits);
begin
  if (AValue >= 5) and (AValue <= 8) then FDataBits := AValue
    else raise Exception.Create('Wrong data bits number');
  if FActive then begin
    GetCommState;
    DCB.ByteSize := AValue;
    SetCommState;
  end;
end;

procedure TSerialPort.SetDTR(const AValue: Boolean);
begin
  FDTR := AValue;
  if FActive then SetDTRF(FDTR);
end;

procedure TSerialPort.SetFlowControl(const AValue: TFlowControl);
begin
  FFlowControl := AValue;
  if FActive then begin
    GetCommState;
    case AValue of
      fcNone: DCB.flags := 0;
      fcSoftware: DCB.flags := DCB.Flags or dcb_OutX or dcb_InX;
      fcHardware: DCB.flags := DCB.Flags
        or dcb_OutxCtsFlow or dcb_OutxDsrFlow
        or dcb_DtrControlHandshake  or dcb_RtsControlHandshake;
    end;
    SetCommState;
  end;
end;

{ TSerialPortReceiveThread }

procedure TSerialPortReceiveThread.Execute;
var
  InBufferUsed: Integer;
  Buffer: array of Byte;
begin
  InBufferUsed := 0;
  with Parent do
  repeat
    if InBufferUsed = 0 then Sleep(1);
    if Active then begin
      InBufferUsed := WaitingData;
      if InBufferUsed > 0 then begin
        SetLength(Buffer, InBufferUsed);
        RecvBuffer(Buffer, Length(Buffer));

        Stream.Size := Length(Buffer);
        Stream.Position := 0;
        Stream.Write(Buffer[0], Length(Buffer));
        if Assigned(Parent.FOnReceiveData) then
          Parent.FOnReceiveData(Stream);
      end else InBufferUsed := 0;
    end else InBufferUsed := 0;
  until Terminated;
end;

constructor TSerialPortReceiveThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Stream := TMemoryStream.Create;
end;

destructor TSerialPortReceiveThread.Destroy;
begin
  Stream.Destroy;
  inherited;
end;

end.

