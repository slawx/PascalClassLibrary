// Object implementation of presto library
// Documentation: http://tools.asix.net/prg_presto_download_dll.htm
//
// Features:
// * Switching SPI data MSB or LSB first bit
// * Block read/write/writeread operations
// * Library runtime or compile time bindings
//
// Date: 2011-08-19

unit UPrestoDLL;

{$mode delphi}

{$DEFINE LIBRARY_RUNTIME}
//{$DEFINE LIBRARY_COMPILETIME}

interface

{$IFDEF Windows}

uses
  Windows, Classes, SysUtils;

const
  PrestoLibName = 'presto.dll';

  // QSetPins()
  PINS_HI = 3;
  PINS_LO = 2;
  PINS_HIZ = 1;
  PINS_D_BIT = 0;
  PINS_C_BIT = 2;
  PINS_P_BIT = 4;
  PINS_L_BIT = 6;

  // QShiftByte()/QShiftByte_OutIn()
  SHIFT_OUTIN_PIND = 0; // InputPin value
  SHIFT_OUTIN_PINL = 1;
  SHIFT_OUTIN_PINP = 2;
  SHIFT_OUTIN_PINI = 3;
  SHIFT_MODE1 = 1; //mode value
  SHIFT_MODE3 = 3;

  // QSetPrestoSpeed()
  PRESTO_CLK1 = 0; // 3MHz - default value
  PRESTO_CLK2 = 1; // 1.5MHz
  PRESTO_CLK4 = 2; // 750kHz
  PRESTO_CLK32 = 3; // 93.75kHz

  OPEN_OK = $100;
  OPEN_NOTFOUND = $101;
  OPEN_CANNOTOPEN = $102;
  OPEN_ALREADYOPEN = $103;
  CLOSE_OK = $200;
  CLOSE_CANNOTCLOSE = $201;
  POWERON_OK = $300;
  POWERON_OCURR = $301;
  GETPINS_CODE = $400;
  GETPINS_PIND = $01;
  GETPINS_PINL = $02;
  GETPINS_PINP = $04;
  GETPINS_PINI = $08;
  NOT_OPENED = $501;
  SHIFT_BYTE_OUTIN_CODE = $600;
  SUPPLY_VOLTAGE_CODE = $700;
  SUPPLY_VOLTAGE_0V = $00;
  SUPPLY_VOLTAGE_2V = $01;
  SUPPLY_VOLTAGE_5V = $03; {1 or 2}
  VPP_OK = $800;
  VPP_OCURR = $801;
  GO_BUTTON_NOT_PRESSED = $900;
  GO_BUTTON_PRESSED = $901;

  FATAL_OVERCURRENTVDD = $01;
  FATAL_OVERCURRENTVPP = $02;
  FATAL_OVERVOLTAGEVDD = $04;

type
  ENoAnswer = class(Exception);
  EWrongAnswer = class(Exception);

  TPrestoSpeed = (psClk1, psClk2, psClk4, psClk32);
  TPrestoSPIMode = (smMode1, smMode3);
  TPrestoPin = (spD, spL, spP, spI);
  TPinState = (psNoChange, psHighZ, psLow, psHigh);
  TSPIDataOrder = (doLSBFirst, doMSBFirst);

  { TPresto }

  TPresto = class
  private
    FOpenned: Boolean;
    FPrestoSpeed: TPrestoSpeed;
    FSerialNumber: Integer;
    FPowerOnVddDelay: Integer; // us
    FSPIDataOrder: TSPIDataOrder;
    FSPIMode: TPrestoSPIMode;
    FSPIInputPin: TPrestoPin;
    function ReadAnswer: Integer;
    function ReadAnswerBlocking: Integer;
    function GetGoButtonState: Boolean;
    procedure SetActiveLED(AValue: Boolean);
    procedure SetDPullUp(AValue: Boolean);
    procedure SetOpenned(AValue: Boolean);
    procedure SetPowerVdd(AValue: Boolean);
    procedure SetPowerVpp13V(AValue: Boolean);
    procedure SetPrestoSpeed(AValue: TPrestoSpeed);
    procedure SetSerialNumber(AValue: Integer);
    function ReverseByte(Value: Byte): Byte;
  public
    constructor Create;
    procedure WriteByte(Data: Byte);
    function ReadWriteByte(Data: Byte): Byte;
    function ReadByte: Byte;
    procedure ReadWriteBlock(WriteData, ReadData: TStream);
    procedure WriteBlock(WriteData: TStream);
    procedure ReadBlock(ReadData: TStream);
    procedure Delay(Duration: Integer);
    procedure SetPin(Pin: TPrestoPin; Value: TPinState);
    function GetPin(Pin: TPrestoPin): Boolean;
    procedure Open;
    procedure Close;
    property Openned: Boolean read FOpenned write SetOpenned;
    property ActiveLED: Boolean write SetActiveLED;
    property PowerVdd: Boolean write SetPowerVdd;
    property PowerVpp13V: Boolean write SetPowerVpp13V;
    property SerialNumber: Integer read FSerialNumber write SetSerialNumber;
    property PowerOnVddDelay: Integer read FPowerOnVddDelay
      write FPowerOnVddDelay;
    property GoButtonState: Boolean read GetGoButtonState;
    property Speed: TPrestoSpeed read FPrestoSpeed write SetPrestoSpeed;
    property DPullUp: Boolean write SetDPullUp;
    property SPIMode: TPrestoSPIMode read FSPIMode write FSPIMode;
    property SPIInputPin: TPrestoPin read FSPIInputPin write FSPIInputPin;
    property SPIDataOrder: TSPIDataOrder read FSPIDataOrder write FSPIDataOrder;
  end;

const
  SPIModeValue: array[TPrestoSPIMode] of Integer = (SHIFT_MODE1, SHIFT_MODE3);

{$IFDEF LIBRARY_COMPILETIME}
function AGet(var answer: integer): LongBool; stdcall; external PrestoLibName;
procedure QSetActiveLED(led: LongBool); stdcall; external PrestoLibName;
procedure QOpenPresto(sn: integer); stdcall; external PrestoLibName;
procedure QClosePresto; stdcall; external PrestoLibName;
procedure QPoweronVdd(delayus: integer); stdcall; external PrestoLibName;
procedure QPoweroffVdd; stdcall; external PrestoLibName;
function AGetBlocking: integer; stdcall; external PrestoLibName;
procedure QSetPins(pins: cardinal); stdcall; external PrestoLibName;
procedure QGetPins; stdcall; external PrestoLibName;
procedure QShiftByte(databyte:integer; mode:integer); stdcall; external PrestoLibName;
procedure QShiftByte_OutIn(databyte:integer; mode:integer; InputPin:integer); stdcall; external PrestoLibName;
procedure QCheckSupplyVoltage; stdcall; external PrestoLibName;
procedure QPoweronVpp13V; stdcall; external PrestoLibName;
procedure QPoweroffVpp13V; stdcall; external PrestoLibName;
procedure QSetDPullup(dpullup_on: LongBool); stdcall; external PrestoLibName;
procedure QCheckGoButton; stdcall; external PrestoLibName;
procedure QSetPrestoSpeed(speed:cardinal); stdcall; external PrestoLibName;
procedure QDelay(delayus: integer); stdcall; external PrestoLibName;
{$ENDIF}

{$IFDEF LIBRARY_RUNTIME}
var
  LibraryLoaded: Boolean;
  AGet: function (out answer: integer): LongBool; stdcall;
  QSetActiveLED: procedure (led: LongBool); stdcall;
  QOpenPresto: procedure (sn: integer); stdcall;
  QClosePresto: procedure ; stdcall;
  QPoweronVdd: procedure (delayus: integer); stdcall;
  QPoweroffVdd: procedure ; stdcall;
  AGetBlocking: function : integer; stdcall;
  QSetPins: procedure (pins: cardinal); stdcall;
  QGetPins: procedure ; stdcall;
  QShiftByte: procedure (databyte:integer; mode:integer); stdcall;
  QShiftByte_OutIn: procedure (databyte:integer; mode:integer; InputPin:integer); stdcall;
  QCheckSupplyVoltage: procedure ; stdcall;
  QPoweronVpp13V: procedure ; stdcall;
  QPoweroffVpp13V: procedure ; stdcall;
  QSetDPullup: procedure (dpullup_on: LongBool); stdcall;
  QCheckGoButton: procedure ; stdcall;
  QSetPrestoSpeed: procedure (speed:cardinal); stdcall;
  QDelay: procedure (delayus: integer); stdcall;
{$ENDIF}

{$ENDIF}

implementation

{$IFDEF Windows}

{$IFDEF LIBRARY_RUNTIME}
var
  DLLHandle: HModule;
{$ENDIF}

resourcestring
  SNoAnswer = 'Presto no answer';
  SWrongAnswer = 'Presto wrong answer';
  SPowerVddError = 'Presto power Vdd error';
  SPowerVpp13VError = 'Presto power Vpp 13V error';
  SOpenError = 'Presto open error: %s';
  SCloseError = 'Presto close error';

{$IFDEF LIBRARY_RUNTIME}
procedure LoadLibraries;
begin
  if not LibraryLoaded then begin
    DLLHandle := LoadLibrary(PrestoLibName);
    if DLLHandle <> 0 then begin
      @AGet := GetProcAddress(DLLHandle, 'AGet');
      @QSetActiveLED := GetProcAddress(DLLHandle, 'QSetActiveLED');
      @QOpenPresto := GetProcAddress(DLLHandle, 'QOpenPresto');
      @QClosePresto := GetProcAddress(DLLHandle, 'QClosePresto');
      @QPoweronVdd := GetProcAddress(DLLHandle, 'QPoweronVdd');
      @QPoweroffVdd := GetProcAddress(DLLHandle, 'QPoweroffVdd');
      @AGetBlocking := GetProcAddress(DLLHandle, 'AGetBlocking');
      @QSetPins := GetProcAddress(DLLHandle, 'QSetPins');
      @QGetPins := GetProcAddress(DLLHandle, 'QGetPins');
      @QShiftByte := GetProcAddress(DLLHandle, 'QShiftByte');
      @QShiftByte_OutIn := GetProcAddress(DLLHandle, 'QShiftByte_OutIn');
      @QCheckSupplyVoltage := GetProcAddress(DLLHandle, 'QCheckSupplyVoltage');
      @QPoweronVpp13V := GetProcAddress(DLLHandle, 'QPoweronVpp13V');
      @QPoweroffVpp13V := GetProcAddress(DLLHandle, 'QPoweroffVpp13V');
      @QSetDPullup := GetProcAddress(DLLHandle, 'QSetDPullup');
      @QCheckGoButton := GetProcAddress(DLLHandle, 'QCheckGoButton');
      @QSetPrestoSpeed := GetProcAddress(DLLHandle, 'QSetPrestoSpeed');
      @QDelay := GetProcAddress(DLLHandle, 'QDelay');
    end else raise Exception.Create('Missing library presto.dll');
    LibraryLoaded := True;
  end;
end;

procedure FreeLibraries;
begin
  if DLLHandle <> 0 then FreeLibrary(DLLHandle);
  LibraryLoaded := False;
end;
{$ENDIF}

{ TPresto }

procedure TPresto.SetActiveLED(AValue: Boolean);
begin
  QSetActiveLED(AValue);
end;

procedure TPresto.SetDPullUp(AValue: Boolean);
begin
  QSetDPullup(AValue);
end;

function TPresto.ReadAnswer: Integer;
begin
  if not AGet(Result) then
    raise ENoAnswer.Create(SNoAnswer);
end;

function TPresto.ReadAnswerBlocking: Integer;
begin
  while not AGet(Result) do ;
end;

function TPresto.ReverseByte(Value: Byte): Byte;
begin
  Value := ((Value shr 1) and $55) or ((Value and $55) shl 1);
  Value := ((Value shr 2) and $33) or ((Value and $33) shl 2);
  Value := ((Value shr 4) and $f) or ((Value and $f) shl 4);
  Result := Value;
end;

function TPresto.GetGoButtonState: Boolean;
begin
  QCheckGoButton;
  Result := ReadAnswer = GO_BUTTON_PRESSED;
end;

procedure TPresto.SetOpenned(AValue: Boolean);
begin
  if AValue then begin
    Open;
  end else begin
    Close;
  end;
end;

procedure TPresto.SetPowerVdd(AValue: Boolean);
begin
  if AValue then begin
    QPoweronVdd(FPowerOnVddDelay);
    if ReadAnswer <> POWERON_OK then
      raise Exception.Create(SPowerVddError);
  end else QPoweroffVdd;
end;

procedure TPresto.SetPowerVpp13V(AValue: Boolean);
begin
  if AValue then begin
    QPoweronVpp13V;
    if ReadAnswer <> VPP_OK then
      raise Exception.Create(SPowerVpp13VError);
  end else QPoweroffVpp13V;
end;

procedure TPresto.SetPrestoSpeed(AValue: TPrestoSpeed);
begin
  if FPrestoSpeed = AValue then Exit;
  FPrestoSpeed := AValue;
  if FOpenned then QSetPrestoSpeed(Integer(AValue));
end;

procedure TPresto.SetSerialNumber(AValue: Integer);
begin
  if FSerialNumber = AValue then Exit;
  FSerialNumber := AValue;
  Openned := False;
  Openned := True;
end;

constructor TPresto.Create;
begin
  FSerialNumber := -1;
  FPowerOnVddDelay := 10000;
  FSPIMode := smMode1;
  FSPIDataOrder := doLSBFirst;
  FSPIInputPin := spI;
  FPrestoSpeed := psClk1;
end;

procedure TPresto.WriteByte(Data: Byte);
begin
  if FSPIDataOrder = doMSBFirst then Data := ReverseByte(Data);
  QShiftByte(Data, SPIModeValue[FSPIMode]);
end;

function TPresto.ReadWriteByte(Data: Byte): Byte;
var
  Answer: Integer;
begin
  if FSPIDataOrder = doMSBFirst then Data := ReverseByte(Data);
  QShiftByte_OutIn(Data, SPIModeValue[FSPIMode], Integer(FSPIInputPin));
  Answer := ReadAnswerBlocking;// AGetBlocking;
  if (Answer and $ff00) <> SHIFT_BYTE_OUTIN_CODE then
    raise EWrongAnswer.Create(SWrongAnswer);
  Result := Answer and $ff;
  if FSPIDataOrder = doMSBFirst then Result := ReverseByte(Result);
end;

function TPresto.ReadByte: Byte;
begin
  Result := ReadWriteByte(0);
end;

procedure TPresto.ReadWriteBlock(WriteData, ReadData: TStream);
var
  Answer: Integer;
  I: Integer;
begin
  WriteData.Position := 0;
  for I := 0 to WriteData.Size - 1 do
    QShiftByte_OutIn(WriteData.ReadByte, SPIModeValue[FSPIMode], Integer(FSPIInputPin));

  ReadData.Position := 0;
  while ReadData.Position < ReadData.Size do begin
    Answer := ReadAnswerBlocking; // AGetBlocking;
    if (Answer and $ff00) <> SHIFT_BYTE_OUTIN_CODE then
      raise EWrongAnswer.Create(SWrongAnswer);
    ReadData.WriteByte(ReverseByte(Answer and $ff));
  end;
end;

procedure TPresto.WriteBlock(WriteData: TStream);
begin
  WriteData.Position := 0;
  while WriteData.Position < WriteData.Size do begin
    WriteByte(WriteData.ReadByte);
  end;
end;

procedure TPresto.ReadBlock(ReadData: TStream);
var
  Answer: Integer;
  I: Integer;
begin
  ReadData.Position := 0;
  for I := 0 to ReadData.Size - 1 do begin
    QShiftByte_OutIn(0, SPIModeValue[FSPIMode], Integer(FSPIInputPin));
  end;

  ReadData.Position := 0;
  while ReadData.Position < ReadData.Size do begin
    Answer := ReadAnswerBlocking; // AGetBlocking;
    if (Answer and $ff00) <> SHIFT_BYTE_OUTIN_CODE then
      raise EWrongAnswer.Create(SWrongAnswer);
    ReadData.WriteByte(ReverseByte(Answer and $ff));
  end;
end;

procedure TPresto.Delay(Duration: Integer);
begin
  QDelay(Duration);
end;

procedure TPresto.SetPin(Pin: TPrestoPin; Value: TPinState);
begin
  QSetPins(Integer(Value) shl (Integer(Pin) * 2));
end;

function TPresto.GetPin(Pin: TPrestoPin): Boolean;
var
  Answer: Integer;
begin
  QGetPins;
  Answer := ReadAnswerBlocking;
  if (Answer and $ff00) <> GETPINS_CODE then
    raise EWrongAnswer.Create(SWrongAnswer);
  Result := ((Answer shr Integer(Pin)) and 1) = 1;
end;

procedure TPresto.Open;
var
  Answer: Integer;
begin
  if not FOpenned then begin
    {$IFDEF LIBRARY_RUNTIME}
    if not LibraryLoaded then LoadLibraries;
    {$ENDIF}
    while AGet(Answer) do ; // Flush input buffer
    QOpenPresto(SerialNumber);
    Answer := AGetBlocking;
    if Answer <> OPEN_OK then
      raise Exception.Create(Format(SOpenError, [IntToHex(Answer, 4)]));
    if FPrestoSpeed <> psClk1 then QSetPrestoSpeed(Integer(FPrestoSpeed));
    FOpenned := True;
  end;
end;

procedure TPresto.Close;
var
  Answer: Integer;
begin
  if FOpenned then begin
    while AGet(Answer) do ; // Flush receive buffer
    QClosePresto;
    //if ReadAnswer <> CLOSE_OK then
    //  raise Exception.Create(SCloseError);
    FOpenned := False;
  end;
end;

initialization

{$IFDEF LIBRARY_RUNTIME}
LibraryLoaded := False;
//LoadLibraries;
{$ENDIF}

finalization

{$IFDEF LIBRARY_RUNTIME}
if LibraryLoaded then FreeLibraries;
{$ENDIF}

{$ENDIF}

end.
