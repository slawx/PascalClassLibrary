unit USerialPort;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, SynaSer, StdCtrls, Dialogs, UCommon, UThreading,
  DateUtils, FileUtil, SpecializedList;

type
  TBaudRate = (br110, br300, br600, br1200, br2400, br4800,
    br9600, br14400, br19200, br38400, br56000,
    br57600, br115200, br128000, br256000);
  TParity = (paNone, paOdd, paEven, paMark, paSpace);
  TStopbits = (sb1_0, sb1_5, sb2_0);
  TDatabits = Integer;
  TFlowControl = (fcNone, fcSoftware, fcHardware);

  TSerialPort = class;
  TReceiveDataEvent = procedure(Stream: TListByte) of object;

  { TSerialPortReceiveThread }

  TSerialPortReceiveThread = class(TListedThread)
  public
    Parent: TSerialPort;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  { TSerialPort }

  TSerialPort = class(TBlockSerial)
  private
    FRTS: Boolean;
    FDTR: Boolean;
    FActive: Boolean;
    FBaudRate: Integer;
    FDataBits: TDataBits;
    FFlowControl: TFlowControl;
    FName: string;
    FOnReceiveData: TReceiveDataEvent;
    FParity: TParity;
    FStopBits: TStopBits;
    FReceiveThread: TSerialPortReceiveThread;
    FReceiveBuffer: TListByte;
    function GetName: string;
    procedure SetBaudRate(const AValue: Integer);
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
    property BaudRate: Integer read FBaudRate write SetBaudRate;
    property Active: Boolean read FActive write SetActive;
    property RTS: Boolean read FRTS write SetRTS;
    property DTR: Boolean read FDTR write SetDTR;
    property ReceiveBuffer: TListByte read FReceiveBuffer;

    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    procedure LoadAvailableToStrings(Strings: TStrings; Check: Boolean = False);
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TObject);
  end;

const
  BaudRateNumericTable: array[TBaudRate] of Integer = (
    110, 300, 600, 1200, 2400, 4800, 9600, 14400, 19200, 38400, 56000,
    57600, 115200, 128000, 256000);
  INVALID_HANDLE_VALUE = DWORD(-1);

implementation

resourcestring
  SAssignmentError = 'Assignment error';
  SWrongNumericBaudRate = 'Wrong numeric baud rate %s';
  SWrongDataBitsNumber = 'Wrong data bits number %s';

{ TSerialPort }

procedure TSerialPort.SetActive(const AValue: Boolean);
begin
  if not FActive and AValue then begin
    FActive := True;
    Open;
    FActive := FHandle <> INVALID_HANDLE_VALUE;
    if not FActive then FreeAndNil(FReceiveThread);
  end else
  if FActive and not AValue then begin
    FActive := False;
    Close;
  end;
end;

procedure TSerialPort.SetName(const AValue: string);
var
  LastState: Boolean;
begin
  if FName = AValue then Exit;
  LastState := FActive;
  Active := False;
  FName := AValue;
  Active := LastState;
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
begin
  Connect(FName);
  //set_fDtrControl(DCB, 1);
  //DCB.flags := ;
  SetBaudRate(FBaudRate);
  SetParity(FParity);
  SetStopBits(FStopBits);
  SetDataBits(FDataBits);
  SetFlowControl(FFlowControl);
  SetDTR(FDTR);
  SetRTS(FRTS);

  FReceiveThread := TSerialPortReceiveThread.Create(True);
  FReceiveThread.FreeOnTerminate := False;
  FReceiveThread.Parent := Self;
  FReceiveThread.Name := 'SerialPort';
  FReceiveThread.Start;
end;

procedure TSerialPort.Close;
begin
  FreeAndNil(FReceiveThread);
  CloseSocket;
end;

procedure TSerialPort.LoadAvailableToStrings(Strings: TStrings; Check: Boolean = False);
var
  I: Integer;
  TestPort: TSerialPort;
  Files: TStringList;
begin
  Strings.Clear;
  {$IFDEF Windows}
  if Check then
  try
    TestPort := TSerialPort.Create;
    for I := 0 to Strings.Count - 1 do
    with TestPort do begin
      Name := Strings[I];
      Active := True;
      if Active then begin
        Strings.AddObject(Name, TObject(I));
      end;
      Active := False;
    end;
  finally
    TestPort.Free;
  end else begin
    for I := 1 to 255 do
      Strings.AddObject('COM' + IntToStr(I), nil);
  end;
  {$ENDIF}
  {$IFDEF Linux}
  if Check then begin
    Files := FindAllFiles('/dev', 'tty*', False);
    Strings.Assign(Files);
    Files.Free;
  end else begin
    for I := 1 to 63 do
      Strings.AddObject('/dev/ttyS' + IntToStr(I), nil);
  end;
  {$ENDIF}
end;

constructor TSerialPort.Create;
begin
  inherited Create;
  FReceiveBuffer := TListByte.Create;
  FBaudRate := 9600;
  FName := 'COM1';
  FDataBits := 8;
  FStopBits := sb1_0;
  FParity := paNone;
  FlowControl := fcNone;
  FDTR := False;
  FRTS := False;
  Active := False;
end;

destructor TSerialPort.Destroy;
begin
  Active := False;
  FReceiveThread.Free;
  ReceiveBuffer.Free;
  inherited Destroy;
end;

procedure TSerialPort.Assign(Source:TObject);
begin
  if Source is TSerialPort then begin
    Name := TSerialPort(Source).Name;
    BaudRate := TSerialPort(Source).BaudRate;
    Parity := TSerialPort(Source).Parity;
    StopBits := TSerialPort(Source).StopBits;
    DataBits := TSerialPort(Source).DataBits;
    FlowControl := TSerialPort(Source).FlowControl;
    DTR := TSerialPort(Source).DTR;
    RTS := TSerialPort(Source).RTS;
  end else raise Exception.Create(SAssignmentError);
end;

procedure TSerialPort.SetBaudRate(const AValue: Integer);
begin
  FBaudRate := AValue;
  if FActive then begin
    GetCommState;
    DCB.BaudRate := AValue;
    SetCommState;
  end;
end;

function TSerialPort.GetName: string;
begin
  Result := FName;
end;

procedure TSerialPort.SetDataBits(const AValue: TDataBits);
begin
  if (AValue >= 5) and (AValue <= 8) then FDataBits := AValue
    else raise Exception.CreateFmt(SWrongDataBitsNumber, [IntToStr(AValue)]);
  if FActive then begin
    GetCommState;
    DCB.ByteSize := AValue;
    SetCommState;
  end;
end;

procedure TSerialPort.SetDTR(const AValue: Boolean);
begin
  FDTR := AValue;
  if FFlowControl = fcNone then
    DCB.flags := DCB.flags and (not (dcb_DtrControlEnable * 3)) or
    (dcb_DtrControlEnable * Byte(AValue));
  if FActive then begin
    if FFlowControl = fcNone then SetCommState
    else SetDTRF(FDTR);
  end;
end;

procedure TSerialPort.SetFlowControl(const AValue: TFlowControl);
begin
  FFlowControl := AValue;
  if FActive then begin
    GetCommState;
    case AValue of
      fcNone: DCB.flags := 0;
      fcSoftware: DCB.flags := dcb_OutX or dcb_InX or
        dcb_DtrControlEnable or dcb_RtsControlEnable;
      fcHardware: DCB.flags := dcb_OutxCtsFlow or dcb_OutxDsrFlow
        or dcb_DtrControlHandshake or dcb_RtsControlHandshake;
    end;
    SetCommState;
  end;
end;

{ TSerialPortReceiveThread }

procedure TSerialPortReceiveThread.Execute;
var
  InBufferUsed: Integer;
  Buffer: array of Byte;
  Read: Integer;
begin
  InBufferUsed := 0;
  with Parent do repeat
      if InBufferUsed = 0 then Sleep(1);
        //else Yield;
      if Active then begin
        InBufferUsed := WaitingData;
        if InBufferUsed > 0 then begin
          SetLength(Buffer, InBufferUsed);
          Read := RecvBuffer(Buffer, Length(Buffer));
          SetLength(Buffer, Read);

          Parent.FReceiveBuffer.Count := Length(Buffer);
          Parent.FReceiveBuffer.ReplaceBuffer(0, PByte(Buffer)^, Length(Buffer));
          if Assigned(Parent.FOnReceiveData) then
            Parent.FOnReceiveData(Parent.FReceiveBuffer);
        end else InBufferUsed := 0;
      end else InBufferUsed := 0;
  until Terminated;
end;

destructor TSerialPortReceiveThread.Destroy;
begin
  inherited;
end;

end.

