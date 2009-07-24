{ TCommPort Component for Delphi 3. }
{ For Win95 & WinNT tested. v.1.00  }
{ (c) Boris Loboda  2:461/256       }
{     barry@audit.kharkov.com       }

unit CommPort;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls;

type
  TC32Event = procedure(Sender: TObject; Status: dword) of object;
  TC32EventState = (evBreak, evCTS, evDSR, evError, evRing,
    evRlsd, evRxChar, evRxFlag, evTxEmpty);
  TC32EventType = set of TC32EventState;

  TCustomCommPort = class;
  BuffArray = array[0..32767] of char;
  PBuffArray = ^BuffArray;
  TFlowControl = (fcNone, fcSoftware, fcHardware);

  TStatusThread = class(TThread)
  private
    FComPort: TCustomCommPort;
    FComHandle: THandle;
    FStatus: dword;
    FOnSignal: TC32Event;
    SOL: TOverlapped;
  protected
    procedure Execute; override;
    procedure DoOnDataAvail;
    procedure DoOnSignal;
    procedure DoOnDTRSignal;
    procedure DoOnRingSignal;
  public
    constructor Create(ComPort: TCustomCommPort; Events: TC32EventType);
    destructor Destroy; override;
    property OnSignal: TC32Event read FOnSignal write FOnSignal;
  end;

  TWriteThread = class(TThread)
  protected
    FComHandle: THandle;
    FComPort: TCustomCommPort;
    WOL: TOverlapped;
  public
    Constructor Create(ComPort: TCustomCommPort);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TReadThread = class(TThread)
  protected
    FComHandle: THandle;
    FComPort: TCustomCommPort;
    ROL: TOverlapped;
    procedure DoOnDataAvail;
  public
    Constructor Create(ComPort: TCustomCommPort);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TBaudRate = (cbr110, cbr300, cbr600, cbr1200, cbr2400, cbr4800,
               cbr9600, cbr14400, cbr19200, cbr38400, cbr56000,
               cbr57600, cbr115200, cbr128000, cbr256000);
  TParity = (paNone, paOdd, paEven, paMark, paSpace);
  TStopbits = (sb1_0, sb1_5, sb2_0);
  TDatabits = Integer;

  TTriggerAvailEvent = procedure(Sender: TObject; Count: word) of object;
  TC32ErrorEvent = procedure(Sender: TObject; Errors: Integer) of object;
  TC32OpenEvent = procedure(Sender: TObject; Error: Integer) of object;

  TCustomCommPort = class(TComponent)
  private
    FComHandle: THandle;
//    FMsgHandler: HWnd;
    FDCB: TDCB;
    StatusThread : TStatusThread;
    WriteThread : TWriteThread;
    ReadThread  : TReadThread;
    FComNumber: integer;
    FBaudRate: TBaudRate;
    FFlowControl: TFlowControl;
    FParity: TParity;
    FStopbits: TStopbits;
    FDatabits: TDatabits;
    FMonitorEvents: TC32EventType;
    FOnOpen: TC32OpenEvent;
    FOnBreakSignal: TNotifyEvent;
    FOnCTSSignal: TNotifyEvent;
    FOnDSRSignal: TNotifyEvent;
    FOnErrorSignal: TC32ErrorEvent;
    FOnRingSignal: TNotifyEvent;
    FOnRLSDSignal: TNotifyEvent;
    FOnTriggerAvail: TTriggerAvailEvent;
    FOnTxEmptySignal: TNotifyEvent;
    FOnDTRSignal: TNotifyEvent;
    FOnRxDSignal: TNotifyEvent;
    FOnTxDSignal: TNotifyEvent;
    FDTR: Boolean;
    FRTS: Boolean;
    FDCD: Boolean;
    FCTS: Boolean;
    FDSR: Boolean;
    FOpen: Boolean;
    ReadReadyEvent : THandle;
    sEvent, rEvent, wEvent : THandle;
    FKillThreads   : Boolean;
//    function GetModemState(Index: Integer): Boolean;
    procedure HandleTC32Event(Sender: TObject; Status: dword);
    procedure SetBaudRate(Value: TBaudRate);
    procedure SetParity(Value: TParity);
    procedure SetStopbits(Value: TStopBits);
    procedure SetDatabits(Value: TDatabits);
    function GetInBuffUsed: integer;
    function GetInBuffFree: integer;
    function GetOutBuffUsed: integer;
    function GetOutBuffFree: integer;
    procedure SetOpen(o: boolean);
    function GetOpen: Boolean;
    procedure SetDTR(State: Boolean);
    procedure SetRTS(State: Boolean);
    procedure SetBaud(B: integer);
    function GetBaud: integer;
    function GetOutQueCount: integer;
    procedure SetFlowControl(const Value: TFlowControl);
  protected
    WriteSection : TRTLCriticalSection;
    ReadSection  : TRTLCriticalSection;
    OBuffer: PBuffArray;
    OBuffSize: Cardinal;
    OBuffUsed: Cardinal;
    IBuffer: PBuffArray;
    IBuffSize: Cardinal;
    IBuffUsed: Cardinal;
    property BaudRate: TBaudRate read FBaudRate write SetBaudRate;
    property FlowControl: TFlowControl read FFlowControl write SetFlowControl;
    property Baud: integer read GetBaud write SetBaud;
    property Parity: TParity read FParity write SetParity;
    property Stopbits: TStopbits read FStopbits write SetStopbits;
    property Databits: TDatabits read FDatabits write SetDatabits;
    property MonitorEvents: TC32EventType read FMonitorEvents write FMonitorEvents;
    {Comm signal events}
    property OnOpen: TC32OpenEvent read FOnOpen write FOnOpen;
    property OnBreakSignal: TNotifyEvent read FOnBreakSignal write FOnBreakSignal;
    property OnCTSSignal: TNotifyEvent read FOnCTSSignal write FOnCTSSignal;
    property OnDSRSignal: TNotifyEvent read FOnDSRSignal write FOnDSRSignal;
    property OnRingSignal: TNotifyEvent read FOnRingSignal write FOnRingSignal;
    property OnRLSDSignal: TNotifyEvent read FOnRLSDSignal write FOnRLSDSignal;
    property OnErrorSignal: TC32ErrorEvent read FOnErrorSignal write FOnErrorSignal;
    property OnTriggerAvail: TTriggerAvailEvent read FOnTriggerAvail write FOnTriggerAvail;
    property OnTxEmptySignal: TNotifyEvent read FOnTxEmptySignal write FOnTxEmptySignal;
    property OnDTRSignal: TNotifyEvent read FOnDTRSignal write FOnDTRSignal;
    property OnRxDSignal: TNotifyEvent read FOnRxDSignal write FOnRxDSignal;
    property OnTxDSignal: TNotifyEvent read FOnTxDSignal write FOnTxDSignal;
    procedure OpenPort;
    procedure ClosePort;
    //Comm escape functions
    function Set_BREAK(State: Boolean): boolean;
  public
    FRxD: Boolean;
    FTxD: Boolean;
    FERR: Boolean;
    FRing : Boolean;
    RingDateTime: TDateTime;
    ErrorDateTime: TDateTime;
    TxDDateTime: TDateTime;
    RxDDateTime: TDateTime;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Enabled: Boolean;
    { Com-port is open. }

    function PutBlock(const Buf; Count: Cardinal): Integer;
    { Put data in COM-port. Check: Count <= OutBuffFree }

    function GetBlock(var Buf; Count: Cardinal): Integer;
    { Get data from COM-port. Check: Count <= InBuffUsed }

    function PutStream(Stream: TStream): Integer;
    { Put data stream in COM-port. Check: Count <= OutBuffFree }

    procedure FlushInBuffer;
    { Flush input data-queue }

    procedure FlushOutBuffer;
    { Flush output data-queue }

    //Comm status flags
    property CTS: Boolean read FCTS;
    { CTS signal state }

    property DSR: Boolean read FDSR;
    { DSR signal state }

    property RING: Boolean read FRing;
    { RING signal detected }

    //index 3 read GetModemState;
    property DCD:Boolean read FDCD;
    { DCD signal state }

    property RxD: Boolean read FRxD; // not implemented

    property TxD: Boolean read FTxD; // not implemented

    property DTR: Boolean read FDTR write SetDTR;
    { Set/Clear DTR signal }

    property RTS: Boolean read FRTS write SetRTS;
    { Set/Clear RTS signal }

    property ERROR: Boolean read FErr;

    //Reference to internal device handle
    property InSize: Cardinal read IBuffSize write IBuffSize;
    { Input buffer size }

    property OutSize: Cardinal read OBuffSize write OBuffSize;
    { Output buffer size }

    property ComHandle: THandle read FComHandle;
    { COM-port Handle (if open) }

    property InBuffFree: integer read GetInBuffFree;
    { Free size in input buffer }

    property InBuffUsed: integer read GetInBuffUsed;
    { Used size in input buffer }

    property OutBuffFree: integer read GetOutBuffFree;
    { Free size in output buffer }

    property OutBuffUsed: integer read GetOutBuffUsed;
    { Used size in output buffer }

    property OutQueCount: integer read GetOutQueCount;
    { Internal output queue count (in Win32 API buffer) }

    property ComNumber: integer read FComNumber write FComNumber;
    { COM-port number. 1-COM1, 2-COM2... }

    property Open: Boolean read GetOpen write SetOpen;
    { Open/Close COM-port }

    function CharReady: Boolean;
    { Input buffer is not empty. }

    function GetChar: Char;
    { Get char from input buffer.               }
    {  Check: CharReady=True or  InBuffUsed > 0 }

    procedure PutChar(Ch: Char);
    { Put char to output buffer. Check: OutBuffFree > 0 }
    procedure PutString(const s: String);
    { Put string into output buffer.       }
    { Check: OutBuffFree >= Length(string) }
    procedure SendBreak(Ticks : Word; Yield : Boolean);
    { Set Break signal. Ticks - ~1/18 sec. }
  end;

  TCommPort = class(TCustomCommPort)
  published
    property ComNumber;
    { Com-port number: 1 - COM1, 2 - COM2... }

    property BaudRate;
    { Baud rate - 300, 600, 1200... }

    property FlowControl;
    { Flow control - fcNone, fcSoftware, fcHardware }

    property Parity;
    property Stopbits;
    property Databits;
    property InSize;
    { Input buffer size }

    property OutSize;
    { Output buffer size }

    property Open;
    property DTR;
    { True - set DTR, False - clear DTR }

    property RTS;
    { True - set RTS, False - clear RTS }

    property Baud;
    property MonitorEvents;
    property OnOpen;
    property OnBreakSignal;

    property OnCTSSignal;
    { CTS signal change... }

    property OnDSRSignal;
    { DSR signal change... }

    property OnErrorSignal;
    property OnRingSignal;
    { Hardware Ring detected... }

    property OnRLSDSignal;
    { DCD signal change... }

    property OnTriggerAvail;
    { Input data received... }

    property OnTxEmptySignal;
    { Output buffer is empty... }

    property OnDTRSignal;
    { DTR signal change. }

    property OnRxDSignal;
    { !!! not implemented. }

    property OnTxDSignal;
    { !!! not implemented. }

  end;


procedure Register;

function SearchComPort(const C : TComponent) : TCommPort;

function ErrorMsg(const ErrorCode : SmallInt) : ShortString;


implementation
{ $DEFINE DEBUG}
{$IFDEF DEBUG}
Uses DebugLog, Main_Com;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('Samples', [TCommPort]);
end;

  function SearchComPort(const C : TComponent) : TCommPort;
    {-Search for a comport in the same form as TComponent}

    function FindComPort(const C : TComponent) : TCommPort;
    var
      I  : Integer;
    begin
      Result := nil;
      if not Assigned(C) then
        Exit;

      {Look through all of the owned components}
      for I := 0 to C.ComponentCount - 1 do begin
        if C.Components[I] is TCommPort then begin
          Result := TCommPort(C.Components[I]);
          Exit;
        end;

        {If this isn't one, see if it owns other components}
        Result := FindComPort(C.Components[I]);
      end;
    end;

  begin
    {Search the entire form}
    Result := FindComPort(C);
  end;

const
  MaxMessageLen = 80;               {All error and status strings less than 80}
  ErrorBase              = 11000;
  {Error string table base}
  ErrorMsgBase : Word = ErrorBase;   {Base in string table}

  function MapCodeToStringID(Code : Integer) : Word;
    {-Return a string table index for Code}
    {-The thousandth's place is moved to the hundreds place and
      the absolute value is taken. The one exception to this rule
      are the error codes -100 through -162, which are changed to
      900 through 962.}
  var
    TT : Word;
    AbsCode : Word;
  begin
    AbsCode := Abs(Code);
    TT := (AbsCode div 1000);
    if (AbsCode >= 100) and (AbsCode <= 199) then
      MapCodeToStringID := ErrorMsgBase + 800 + AbsCode
    else
      MapCodeToStringID := ErrorMsgBase + (TT * 100) + (AbsCode mod 1000);
  end;

  function pErrorMsg(P : PChar; Code : Integer) : PChar;
    {-Fill P with a string from APW.RES stringtable}
  begin
    if LoadString(hInstance, MapCodeToStringID(Code),
                  P, MaxMessageLen) = 0 then
      {Message not found, return empty string}
      P[0] := #0;
    pErrorMsg := P;
  end;

  function ErrorMsg(const ErrorCode : SmallInt) : ShortString;
    {-Return an error message for ErrorCode}
  var
    Msg : array[0..80] of Char;
  begin
    ErrorMsg := StrPas(pErrorMsg(Msg, ErrorCode));
  end;

//TWriteThread
constructor TWriteThread.Create(ComPort: TCustomCommPort);
begin
  inherited Create(False);
  FComPort := ComPort;
  FComHandle := FComPort.ComHandle;
  FillChar(WOL, SizeOf(WOL), 0);
  WOL.hEvent := CreateEvent(nil, True, False, nil);
end;

destructor TWriteThread.Destroy;
begin
  CloseHandle(WOL.hEvent);
  Inherited;
end;

procedure TWriteThread.Execute;
var
  BytesToWrite, BytesWritten: Cardinal;
  lpErrors: DWORD;
  lpStat: TComStat;

  procedure WriteSuccess;
  begin
    if BytesWritten < FComPort.OBuffUsed then
      Move(FComPort.OBuffer[BytesWritten], FComPort.OBuffer[0],
           FComPort.OBuffUsed - BytesWritten);
    Dec(FComPort.OBuffUsed, BytesWritten);
//    if FComPort.OBuffUsed < 0 then FComPort.OBuffUsed := 0;
  end;

begin
  SetEvent(FComPort.wEvent);
  Repeat
    if (FComPort.OBuffUsed > 0) then begin
      EnterCriticalSection(FComPort.WriteSection);
      ClearCommError(FComPort.ComHandle, lpErrors,@lpStat);
      BytesToWrite := FComPort.OBuffUsed;
      if BytesToWrite > (FComPort.OBuffSize - lpStat.cbOutQue) then
        BytesToWrite := (FComPort.OBuffSize - lpStat.cbOutQue);
      if FComPort.Enabled and(BytesToWrite > 0)and
         (not FComPort.FKillThreads) and (FComPort.CTS or (FComPort.FFlowControl = fcNone)) then begin
        if WriteFile(FComHandle, FComPort.OBuffer^[0], BytesToWrite, BytesWritten, @WOL) then begin
          WriteSuccess;
        end
        else if GetLastError = ERROR_IO_PENDING then begin
          if GetOverlappedResult(FComHandle, WOL, BytesWritten, True) then begin
            ResetEvent(WOL.hEvent);
            WriteSuccess;
          end;
        end;
      end;
      LeaveCriticalSection(FComPort.WriteSection);
      if FComPort.FKillThreads then Break;
      Sleep(10);
    end
    else Sleep(10);
  until FComPort.FKillThreads or Terminated;
  SetEvent(FComPort.wEvent);
end;

constructor TReadThread.Create(ComPort: TCustomCommPort);
begin
  inherited Create(False);
  FComPort := ComPort;
  FComHandle := FComPort.ComHandle;
  FillChar(ROL, SizeOf(ROL), 0);
  ROL.hEvent := CreateEvent(nil, True, False, nil);
end;

destructor TReadThread.Destroy;
begin
  CloseHandle(ROL.hEvent);
  Inherited;
end;

procedure TReadThread.Execute;
var
  BytesToRead, BytesReaded: Cardinal;
  lpErrors: DWORD;
  lpStat: TComStat;

  procedure ReadSuccess;
  begin
    //if BytesReaded < 0 then BytesReaded := 0;
    Inc(FComPort.IBuffUsed, BytesReaded);
    if FComPort.IBuffUsed > FComPort.IBuffSize then
      FComPort.IBuffUsed := FComPort.IBuffSize;
  end;

begin
  SetEvent(FComPort.rEvent);
  Repeat
    if WaitForSingleObject(FcomPort.ReadReadyEvent, 500) = WAIT_OBJECT_0 then begin
      if FComPort.FKillThreads then Break;
      if FComPort.Enabled then
        ClearCommError(FComHandle, lpErrors, @lpStat)
      else begin lpErrors := 0; FillChar(lpStat, SizeOf(LpStat), 0); end;
      EnterCriticalSection(FComPort.ReadSection);
//      BytesToRead := FComPort.IBuffSize-FComPort.IBuffUsed;
      BytesToRead := lpStat.cbInQue;
      if BytesToRead > FComPort.IBuffSize - FComPort.IBuffUsed then
        BytesToRead := FComPort.IBuffSize - FComPort.IBuffUsed;
      BytesReaded := 0;
      if (BytesToRead > 0) and FComPort.Enabled then begin
        if ReadFile(FComHandle, FComPort.IBuffer^[FComPort.IBuffUsed], BytesToRead, BytesReaded, @ROL) then begin
          ReadSuccess;
        end
        else if GetLastError = ERROR_IO_PENDING then begin
          if GetOverlappedResult(FComHandle, ROL, BytesReaded, True) then begin
            ResetEvent(ROL.hEvent);
            ReadSuccess;
          end;
        end;
      end;
      LeaveCriticalSection(FComPort.ReadSection);
      if (FComPort.IBuffUsed > 0) and
         Assigned(FComPort.FOnTriggerAvail) then begin
        Synchronize(DoOnDataAvail);
//        DoOnDataAvail;
      end;
    end
    else SetEvent(FComPort.ReadReadyEvent);
  until FComPort.FKillThreads or Terminated;
  SetEvent(FComPort.rEvent);
end;

procedure TReadThread.DoOnDataAvail;
begin
  if Assigned(FComPort.OnTriggerAvail) then
    FComPort.OnTriggerAvail(FComPort, FComPort.IBuffUsed);
end;

//TStatusThread

constructor TStatusThread.Create(ComPort: TCustomCommPort; Events: TC32EventType);
const
  EvList: array[TC32EventState] of dword = (EV_BREAK, EV_CTS, EV_DSR, EV_ERR,
    EV_RING, EV_RLSD, EV_RXCHAR, EV_RXFLAG, EV_TXEMPTY);
//var
//  EvIndex: TC32EventState;
//  AttrWord: dword;
begin
  Inherited Create(false);
  FComPort := ComPort;
  FComHandle := ComPort.ComHandle;
//  AttrWord := 0;
//  for EvIndex := evBREAK to evTXEMPTY do
//    if EvIndex in Events then
//      AttrWord := AttrWord or EvList[EvIndex];
  OnSignal := FComPort.HandleTC32Event;
  FillChar(SOL, Sizeof(SOL), 0);
  SOL.hEvent := CreateEvent(nil, True, False, nil);
end;

destructor TStatusThread.Destroy;
begin
  CloseHandle(SOL.hEvent);
  Inherited Destroy;
end;

procedure TStatusThread.Execute;
var
  BytesTransferred: DWORD; //Dummy, not valid for WaitCommEvent
  OK: Boolean;
  lpModemStatus: DWORD;
begin
  if FComPort.Enabled and GetCommModemStatus(FComHandle, lpModemStatus) then begin
    FComPort.FCTS  := lpModemStatus and MS_CTS_ON <> 0;
    FComPort.FDSR  := lpModemStatus and MS_DSR_ON <> 0;
    FComPort.FRing := lpModemStatus and MS_RING_ON <> 0;
    FComPort.FDCD  := lpModemStatus and MS_RLSD_ON <> 0;
    FStatus := EV_CTS or EV_DSR or EV_RING or EV_RLSD;
    Synchronize(DoOnSignal);
  end;
  SetEvent(FComPort.sEvent); // Status thread started
  repeat
    if FcomPort.FKillThreads then Break;
    OK := WaitCommEvent(FComHandle, FStatus, @SOL);
    if Not OK then Sleep(10);
    if FcomPort.FKillThreads then Break;
    if (not OK) and (GetLastError = ERROR_IO_PENDING) then begin
      OK := GetOverlappedResult(FComHandle, SOL,
         BytesTransferred, True);
      if OK then ResetEvent(SOL.hEvent);
    end;
    if FComPort.FKillThreads then Break;
    if OK and ((FStatus or EV_RXCHAR) > 0) then begin
      if FComPort.Enabled then SetEvent(FComPort.ReadReadyEvent);
      if (FComPort.IBuffUsed > 0) and
         Assigned(FComPort.FOnTriggerAvail) then begin
        Synchronize(DoOnDataAvail);
      end;
    end;
    if OK and (Fstatus and (EV_CTS or EV_DSR or EV_RING or EV_RLSD or EV_ERR) <> 0) and
       GetCommModemStatus(FComHandle, lpModemStatus) then begin
      FComPort.FCTS  := lpModemStatus and MS_CTS_ON <> 0;
      FComPort.FDSR  := lpModemStatus and MS_DSR_ON <> 0;
      FComPort.FDCD  := lpModemStatus and MS_RLSD_ON <> 0;
    end;
    if OK then Synchronize(DoOnSignal);
//    if OK then DoOnSignal;
  until FComPort.FKillThreads or Terminated;
  SetEvent(FComPort.sEvent);
end;

procedure TStatusThread.DoOnDataAvail;
begin
  if Assigned(FComPort.OnTriggerAvail) then
    FComPort.OnTriggerAvail(FComPort, FComPort.IBuffUsed);
end;

procedure TStatusThread.DoOnSignal;
begin
  if Assigned(FOnSignal) then FOnSignal(Self, FStatus);
end;

procedure TStatusThread.DoOnDTRSignal;
begin
  if Assigned(FComPort.OnDTRSignal) then
    FComPort.OnDTRSignal(FComPort);
end;

procedure TStatusThread.DoOnRingSignal;
begin
  if Assigned(FComPort.OnRingSignal) then
    FComPort.OnRingSignal(FComPort);
end;

//TCustomComm32

constructor TCustomCommPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FComHandle := INVALID_HANDLE_VALUE;
  FComNumber := 2;
  FBaudRate := cbr19200;
  FParity := paNone;
  FStopbits := sb1_0;
  FDatabits := 8;
  FMonitorEvents := [evBreak, evCTS, evDSR, evError, evRing,
                     evRlsd, evRxChar, evRxFlag, evTxEmpty];
  IBuffSize := 2048;
  OBuffSize := 4096;
  InitializeCriticalSection(WriteSection);
  InitializeCriticalSection(ReadSection);
  ReadReadyEvent := CreateEvent(nil, False, False, nil);
  sEvent := CreateEvent(nil, False, False, nil);
  wEvent := CreateEvent(nil, False, False, nil);
  rEvent := CreateEvent(nil, False, False, nil);
end;

destructor TCustomCommPort.Destroy;
begin
  ClosePort;
  CloseHandle(sEvent);
  CloseHandle(wEvent);
  CloseHandle(rEvent);
  CloseHandle(ReadReadyEvent);
  DeleteCriticalSection(ReadSection);
  DeleteCriticalSection(WriteSection);
  Inherited Destroy;
end;

function TCustomCommPort.Enabled: Boolean;
begin
  Result := FComHandle <> INVALID_HANDLE_VALUE;
end;

function TCustomCommPort.GetOutQueCount: integer;
var
  lpErrors: DWORD;
  lpStat: TComStat;
begin
  Result := 0;
  if Enabled then begin
    ClearCommError(FComHandle, lpErrors, @lpStat);
    Result := lpStat.cbOutQue;
  end;
end;

procedure TCustomCommPort.OpenPort;
var
  CommTimeouts: TCommTimeouts;
  DeviceName: String;
begin
  if FOpen then Exit;
  SetLastError(0); //remove any pending errors
  DeviceName := '\\.\COM' + IntToStr(FComNumber);
  FComHandle := CreateFile(PCHAR(DeviceName), GENERIC_READ or GENERIC_WRITE,
    0, nil, OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED, 0);
  if Enabled then begin
    GetMem(OBuffer, OBuffSize);
    GetMem(IBuffer, IBuffSize);
    SetupComm(FComHandle, IBuffSize, OBuffSize);

    FillChar(CommTimeOuts, SizeOf(TCommTimeouts), 0);
//   CommTimeouts.ReadIntervalTimeout := MAXDWORD;

    SetCommTimeOuts(FComHandle, CommTimeOuts);

    SetBaudrate(FBaudrate);
    SetParity(FParity);
    SetStopbits(FStopbits);
    SetDatabits(FDatabits);
    SetFlowControl(FFlowControl);
    
    FKillThreads := False;
    StatusThread := TStatusThread.Create(Self, FMonitorEvents);
    WaitForSingleObject(sEvent, 10);
    SetCommMask(FComHandle, EV_BREAK or EV_CTS or EV_DSR or EV_ERR or
                       EV_RING or EV_RLSD or EV_RXCHAR or EV_TXEMPTY);
    ReadThread := TReadThread.Create(Self);
    WaitForSingleObject(rEvent, 10);
    WriteThread := TWriteThread.Create(Self);
    WaitForSingleObject(wEvent, 10);
    FOpen := True;
    if Assigned(FOnOpen) then
      FOnOpen(Self, GetLastError);
  end;
end;

procedure TCustomCommPort.ClosePort;
//var
//  res: integer;
begin
  FKillThreads := True;
  if FOpen then begin
    FTxD  := False;
    FRxD  := False;
    FRing := False;
    FDCD  := False;
//    FlushOutBuffer;
//    FlushInBuffer;
    WriteThread.Terminate;
    WriteThread.WaitFor;
    WriteThread.Free;
    WriteThread := Nil;
    SetEvent(ReadReadyEvent);
    ReadThread.Terminate;
    ReadThread.WaitFor;
    ReadThread.Free;
    ReadThread := Nil;
    SetCommMask(FComHandle, 0);
    StatusThread.Terminate;
    StatusThread.WaitFor;
    StatusThread.Free;
    StatusThread := Nil;
    CloseHandle(FComHandle);
    FComHandle := INVALID_HANDLE_VALUE;
    FDTR := False;
    FreeMem(IBuffer, IBuffSize);
    FreeMem(OBuffer, OBuffSize);
    IBuffer := nil;
    OBuffer := nil;
    IBuffUsed := 0;
    OBuffUsed := 0;
    FOpen := False;
  end;
end;

function TCustomCommPort.GetInBuffUsed: integer;
begin
  Result := IBuffUsed;
end;

function TCustomCommPort.GetInBuffFree: integer;
begin
  EnterCriticalSection(ReadSection);
  Result := IBuffSize - IBuffUsed;
  LeaveCriticalSection(ReadSection);
end;

function TCustomCommPort.GetOutBuffUsed: integer;
begin
  Result := OBuffUsed;
end;

function TCustomCommPort.GetOutBuffFree: integer;
begin
  Result := OBuffSize - OBuffUsed;
end;

procedure TCustomCommPort.SetOpen(o: boolean);
begin
  if o then OpenPort else ClosePort;
end;

function TCustomCommPort.GetOpen: Boolean;
begin
  Result := FOpen;
end;

function TCustomCommPort.CharReady: Boolean;
begin
  Result := IBuffUsed > 0;
end;

function TCustomCommPort.GetChar: Char;
begin
  Result := #0;
  if IBuffUsed > 0 then GetBlock(Result, 1)
end;

procedure TCustomCommPort.PutChar(ch: char);
begin
  PutBlock(ch, 1);
end;

function TCustomCommPort.PutStream(Stream: TStream): Integer;
begin
  if Assigned(Stream) then begin
    Stream.Position := 0;
    Result := 0;
    if not Enabled then Exit;
    EnterCriticalSection(WriteSection);
    //  Result := Count;
    if (OBuffSize - OBuffUsed) >= Stream.Size then
      Result := Stream.Size
    else Result := OBuffSize - OBuffUsed;
    Stream.Read(OBuffer[OBuffUsed], Result);
    Inc(OBuffUsed, Result);
    LeaveCriticalSection(WriteSection);
  end else Result := 0;
end;

procedure TCustomCommPort.PutString(const s: String);
begin
  if Length(s) > 0 then PutBlock(s[1], length(s));
end;

procedure TCustomCommPort.SendBreak(Ticks : Word; Yield : Boolean);
begin
  if Enabled then Set_Break(True);
  Sleep(Ticks * 55);
  if Enabled then Set_Break(False);
end;

function TCustomCommPort.PutBlock(const Buf; Count: Cardinal): Integer;
begin
  Result := 0;
  if not Enabled then exit;
  EnterCriticalSection(WriteSection);
//  Result := Count;
  if (OBuffSize - OBuffUsed) >= Count then
    Result := Count
  else Result := OBuffSize - OBuffUsed;
  Move(Buf, OBuffer[OBuffUsed], Result);
  Inc(OBuffUsed, Result);
  LeaveCriticalSection(WriteSection);
end;

function TCustomCommPort.GetBlock(var Buf; Count: Cardinal): Integer;
begin
  Result := 0;
  if not Enabled then Exit;
  EnterCriticalSection(ReadSection);
  Result := Count;
  if Count > IBuffUsed then Result := IBuffUsed;
  Move(IBuffer[0], Buf, Result);
  Move(IBuffer[Result], IBuffer[0], IBuffUsed - Cardinal(Result));
  Dec(IBuffUsed, Result);
  LeaveCriticalSection(ReadSection);
end;

{Errorflags for OnErrorSignal
 CE_BREAK       The hardware detected a break condition.
 CE_DNS	        Windows 95 only: A parallel device is not selected.
 CE_FRAME       The hardware detected a framing error.
 CE_IOE	        An I/O error occurred during communications with the device.
 CE_MODE        The requested mode is not supported, or the hFile parameter
                is invalid. If this value is specified, it is the only valid error.
 CE_OOP	        Windows 95 only: A parallel device signaled that it is out of paper.
 CE_OVERRUN     A character-buffer overrun has occurred. The next character is lost.
 CE_PTO	        Windows 95 only: A time-out occurred on a parallel device.
 CE_RXOVER      An input buffer overflow has occurred. There is either no
                room in the input buffer, or a character was received after
                the end-of-file (EOF) character.
 CE_RXPARITY    The hardware detected a parity error.
 CE_TXFULL      The application tried to transmit a character, but the output
                buffer was full.}

procedure TCustomCommPort.HandleTC32Event(Sender: TObject; Status: dword);
var
  Errors: dword;
begin
  Errors := 0;
//  FillChar(FCT, SizeOf(FCT), 0);
//  if Enabled then
//    ClearCommError(FComHandle, Errors, @FCT);
  if Status and EV_RLSD > 0 then
    if Assigned(FOnRLSDSignal) then FOnRLSDSignal(Self);
  if Status and EV_BREAK > 0 then
    if Assigned(FOnBreakSignal) then FOnBreakSignal(Self);
  if Status and EV_CTS > 0 then
    if Assigned(FOnCTSSignal) then FOnCTSSignal(Self);
  if Status and EV_DSR > 0 then
    if Assigned(FOnDSRSignal) then FOnDSRSignal(Self);
  if Status and EV_ERR > 0 then
    if Assigned(FOnErrorSignal) then FOnErrorSignal(Self, Errors);
  if Status and EV_RING > 0 then begin
    if Assigned(FOnRingSignal) then FOnRingSignal(Self);
  end;
  if Status and EV_TXEMPTY > 0 then
    if Assigned(FOnTxEmptySignal) then FOnTxEmptySignal(Self);
end;

procedure TCustomCommPort.SetBaudRate(Value: TBaudRate);
const
  CBR: array[TBaudRate] of Integer = (CBR_110, CBR_300, CBR_600, CBR_1200, CBR_2400,
                       CBR_4800, CBR_9600, CBR_14400, CBR_19200, CBR_38400,
                       CBR_56000, CBR_57600, CBR_115200, CBR_128000, CBR_256000);
begin
  FBaudRate := Value;
  if Enabled then
  begin
    GetCommState(FComHandle, FDCB);
    FDCB.BaudRate := CBR[FBaudRate];
    SetCommState(FComHandle, FDCB);
  end;
end;

procedure TCustomCommPort.SetParity(Value: TParity);
const
  PAR: array[TParity] of byte = (NOPARITY, ODDPARITY, EVENPARITY,
                                 MARKPARITY, SPACEPARITY);
begin
  FParity := Value;
  if Enabled then
  begin
    GetCommState(FComHandle, FDCB);
    FDCB.Parity := PAR[FParity];
    SetCommState(FComHandle, FDCB);
  end;
end;

procedure TCustomCommPort.SetStopbits(Value: TStopbits);
const
  STB: array[TStopbits] of Byte = (ONESTOPBIT, ONE5STOPBITS, TWOSTOPBITS);
begin
  FStopbits := Value;
  if Enabled then begin
    GetCommState(FComHandle, FDCB);
    FDCB.Stopbits := STB[FStopbits];
    SetCommState(FComHandle, FDCB);
  end;
end;

procedure TCustomCommPort.SetDataBits(Value: TDatabits);
begin
  FDataBits := Value;
  if Enabled then begin
    GetCommState(FComHandle, FDCB);
    FDCB.Bytesize := Value;
    SetCommState(FComHandle, FDCB);
  end;
end;

procedure TCustomCommPort.SetBaud(B: integer);
begin
  Case B of
    110     : BaudRate := cbr110;
    300     : BaudRate := cbr300;
    600     : BaudRate := cbr600;
    1200    : BaudRate := cbr1200;
    2400    : BaudRate := cbr2400;
    4800    : BaudRate := cbr4800;
    9600    : BaudRate := cbr9600;
    14400   : BaudRate := cbr14400;
    19200   : BaudRate := cbr19200;
    38400   : BaudRate := cbr38400;
    56000   : BaudRate := cbr56000;
    57600   : BaudRate := cbr57600;
    115200  : BaudRate := cbr115200;
    128000  : BaudRate := cbr128000;
    256000  : BaudRate := cbr256000;
  end;
end;

function TCustomCommPort.GetBaud: integer;
begin
  Result := 0;
  Case BaudRate of
    cbr110     : result := 110;
    cbr300     : result := 300;
    cbr600     : result := 600;
    cbr1200    : result := 1200;
    cbr2400    : result := 2400;
    cbr4800    : result := 4800;
    cbr9600    : result := 9600;
    cbr14400   : result := 14400;
    cbr19200   : result := 19200;
    cbr38400   : result := 38400;
    cbr56000   : result := 56000;
    cbr57600   : result := 57600;
    cbr115200  : result := 115200;
    cbr128000  : result := 128000;
    cbr256000  : result := 256000;
  end;
end;

procedure TCustomCommPort.FlushInBuffer;
begin
  if Enabled then begin
    PurgeComm(FComHandle, PURGE_RXABORT or PURGE_RXCLEAR);
  end;
  EnterCriticalSection(ReadSection);
  IBuffUsed := 0;
  LeaveCriticalSection(ReadSection);
end;

procedure TCustomCommPort.FlushOutBuffer;
begin
  if Enabled then begin
    PurgeComm(FComHandle, PURGE_TXABORT or PURGE_TXCLEAR);
  end;
  EnterCriticalSection(WriteSection);
  OBuffUsed := 0;
  LeaveCriticalSection(WriteSection);
end;

(*
function TCustomCommPort.GetModemState(Index: Integer): boolean;
var
  Flag, State: dword;
begin
  case Index of
    1: State := MS_CTS_ON;
    2: State := MS_DSR_ON;
    3: State := MS_RING_ON;
    4: State := MS_RLSD_ON;
    else
      State := 0;
  end;
  Result := false;
  if Enabled then
    if GetCommModemStatus(FComHandle, Flag) then
      Result := (Flag and State > 0);
end;
*)

procedure TCustomCommPort.SetDTR(State: boolean);
const
  DTR: array[Boolean] of Integer = (Windows.CLRDTR, Windows.SETDTR);
//var
//  res: Boolean;
//  ErrCode: Integer;
begin
  FDTR := State;
  if FOpen then begin
    GetLastError;
    if FDTR then
      EscapeCommFunction(FComHandle, Windows.SETDTR)
    else EscapeCommFunction(FComHandle, Windows.CLRDTR);
    {$IFDEF DEBUG}
    if Not Res then begin
      ErrCode := GetLastError;
      WriteDebugLog('DTR set: Error! - Code: ' + IntToStr(ErrCode));
    end;
    {$ENDIF}
  end;
  {$IFDEF DEBUG}
  if not FOpen then WriteDebugLog('Set DTR: Port Closed!');
  {$ENDIF}
  if Assigned(FOnDTRSignal) and Assigned(StatusThread) then
    StatusThread.Synchronize(StatusThread.DoOnDTRSignal);
end;

procedure TCustomCommPort.SetRTS(State: boolean);
const
  RTS: array[boolean] of Integer = (Windows.CLRRTS, Windows.SETRTS);
begin
  FRTS := State;
  if FOpen then
    EscapeCommFunction(FComHandle, RTS[State]);
end;

function TCustomCommPort.Set_BREAK(State: Boolean): boolean;
const
  BREAK: array[boolean] of Integer = (CLRBREAK, SETBREAK);
begin
  Result := False;
  if FOpen then
    Result := EscapeCommFunction(FComHandle, BREAK[State]);
end;

procedure TCustomCommPort.SetFlowControl(const Value: TFlowControl);
const
  dcb_OutxCTSFlow  = $0004;
  dcb_OutxDSRFlow  = $0008;
  dcb_DTRBit1      = $0010;
  dcb_DTRBit2      = $0020;
  dcb_OutX         = $0100;
  dcb_InX          = $0200;
  dcb_RTSBit1      = $1000;
  dcb_RTSBit2      = $2000;
  InHdwFlow  = dcb_DTRBit2 + dcb_RTSBit2 + dcb_DTRBit1 + dcb_DTRBit2;
  OutHdwFlow = dcb_OutxDSRFlow + dcb_OutxCTSFlow;
  AllHdwFlow = InHdwFlow + OutHdwFlow;
  AllSfwFlow = dcb_InX + dcb_OutX;

begin
  FFlowControl := Value;
  if Enabled then
  begin
    GetCommState(FComHandle, FDCB);
    case Value of
      fcNone: FDCB.Flags := 0;
      fcSoftware: FDCB.Flags := AllSfwFlow;
      fcHardware: FDCB.Flags := AllHdwFlow;
    end;
    SetCommState(FComHandle, FDCB);
  end;
end;

end.
