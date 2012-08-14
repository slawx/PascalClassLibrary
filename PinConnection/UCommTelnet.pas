unit UCommTelnet;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, UCommPin, SpecializedList, DateUtils, UStreamHelper,
  SpecializedStream, UBinarySerializer;

type
  TCommTelnet = class;

  TTelnetCode = (tcEOR = 239, tcSE = 240, tcNOP = 241, tcDATA_MARK = 242,
    tcBREAK = 243, tcIP = 244, tcAO = 245, tcAYT = 246, tcEC = 247,
    tcEL = 248, tcGA = 249, tcSB = 250, tcWILL = 251, tcWONT = 252,
    tcDO = 253, tcDONT = 254, tcIAC = 255);

  TTelnetCommand = (tmTransmitBinary = 0, tmEcho = 1, tmReconnection = 2,
    tmSupressGoAhead = 3, tmApproxMessageSizeNegotiation = 4,
    tmStatus = 5, tmTimingMark = 6,
    tmRemoteControlledTransAndEcho = 7, tmOutputLineWidth = 8, tmOutputPageSize = 9,
    tmOutputCarriageReturnDisposition = 10, tmOutputHorizontalTabStops = 11,
    tmOutputHorizontalTabDisposition = 12, tmOutputFormfeedDisposition = 13,
    tmOutputVerticalTabstops = 14, tmOutputVerticalTabDisposition = 15,
    tmOutputLinefeedDisposition = 16, tmExtendedASCII = 17, tmLogout = 18,
    tmByteMacro = 19, tmDataEntryTerminal = 20,
    tmSUPDUP = 21, tmSUPDUPOutput = 22, tmSendLocation = 23, tmTerminalType = 24,
    tmEndOfRecord = 25, tmTACACSUserIdentification = 26, tmOutputMarking = 27,
    tmTerminalLocationNumber = 28, tmTelnet3270Regime = 29, tmX_3PAD = 30,
    tmNegotiateAboutWindowSize = 31, tmTerminalSpeed = 32,
    tmRemoteFlowControl = 33, tmLineMode = 34,
    tmXDisplayLocation = 35, tmEnvironmentOption = 36,
    tmAuthentication = 37, tmEncryptionOption = 38, tmNewEnvironmentOption = 39,
    tmTN3270E = 40, tmXauth = 41,
    tmCharset = 42, tmRSP = 43, tmComPortControlOption = 44,
    tmTelnetSuppressLocalEcho = 45, tmTelnetStartTLS = 46,
    rmKERMIT = 47, tmSendURL = 48, tmForwardX = 49);

  TTelnetState = (tsNormal, tsIAC, tsSB, tsSB_IAC, tsOption);

  TTelnetOptionEvent = procedure (Sender: TCommTelnet; Data: TStream) of object;

  TTelnetType = (ttClient, ttServer);

  { TTelnetOption }

  TTelnetOption = class
  private
    FOnRequest: TTelnetOptionEvent;
  public
    Telnet: TCommTelnet;
    Code: TTelnetOption;
    ServerChecked: Boolean;
    ServerSupport: Boolean;
    property OnRequest: TTelnetOptionEvent read FOnRequest write FOnRequest;
  end;

  { TTelnetOptionList }

  { TCommTelnet }

  TCommTelnet = class
  private
    FState: TTelnetState;
    FCommandData: TStreamHelper;
    procedure TelnetDataReceive(Sender: TCommPin; Stream: TStream);
    procedure RawDataReceive(Sender: TCommPin; Stream: TStream);
  public
    Options: TListObject;
    TelnetPin: TCommPin;
    RawPin: TCommPin;
    Timeout: TDateTime;
    procedure Register(Option: TTelnetOption);
    procedure Unregister(Option: TTelnetOption);
    function CheckOption(Option: TTelnetCommand): Boolean;
    procedure SendSubCommand(Option: TTelnetCommand; Request, Response: TListByte);
    procedure SendCommand(Code: TTelnetCode; Request, Response: TListByte);
    constructor Create;
    destructor Destroy; override;
  end;


implementation

resourcestring
  SUnknownState = 'Unknown state';
  SWrongResponseOption = 'Wrong response option';

{ TCommTelnet }

procedure TCommTelnet.TelnetDataReceive(Sender: TCommPin; Stream: TStream);
var
  Data: Byte;
  RawData: TBinarySerializer;
begin
  try
    RawData := TBinarySerializer.Create;
    RawData.List := TListByte.Create;
    RawData.OwnsList := True;
    Stream.Position := 0;
    while Stream.Position < Stream.Size do begin
      Data := Stream.ReadByte;
      if Data = Byte(tcIAC) then begin
        RawData.WriteByte(Byte(tcIAC));
        RawData.WriteByte(Byte(tcIAC));
      end else RawData.WriteByte(Data);
    end;
    RawPin.Send(RawData.List);
  finally
    RawData.Free;
  end;
end;

procedure TCommTelnet.RawDataReceive(Sender: TCommPin; Stream: TStream);
var
  Data: Byte;
  RawData: TBinarySerializer;
begin
  try
    RawData := TBinarySerializer.Create;
    RawData.List := TListByte.Create;
    RawData.OwnsList := True;

    Stream.Position := 0;
    while Stream.Position < Stream.Size do begin
      Data := Stream.ReadByte;
      if FState = tsNormal then begin
        if Data = Byte(tcIAC) then FState := tsIAC
          else RawData.WriteByte(Data);
      end else
      if FState = tsIAC then begin
        if Data = Byte(tcSB) then begin
          FState := tsSB;
          FCommandData.Size := 0;
        end
        else if Data = Byte(tcDO) then begin
          FCommandData.WriteByte(Data);
          FState := tsOption;
        end else
        if Data = Byte(tcDONT) then begin
          FCommandData.WriteByte(Data);
          FState := tsOption;
        end else
        if Data = Byte(tcWONT) then begin
          FCommandData.WriteByte(Data);
          FState := tsOption;
        end else FState := tsNormal;
      end else
      if FState = tsSB then begin
        if Data = Byte(tcIAC) then FState := tsSB_IAC
        else FCommandData.WriteByte(Data);
      end else
      if FState = tsSB_IAC then begin
        if Data = Byte(tcSE) then begin
          FState := tsNormal;
        end else

      end else
      if FState = tsOption then begin
        FCommandData.WriteByte(Data);
        FState := tsNormal;
      end else raise Exception.Create(SUnknownState);
    end;
    TelnetPin.Send(RawData.List);
  finally
    RawData.Free;
  end;
end;

procedure TCommTelnet.Register(Option: TTelnetOption);
begin
  Option.Telnet := Self;
  Options.Add(Option);
end;

procedure TCommTelnet.Unregister(Option: TTelnetOption);
begin
  Options.Remove(Option);
end;

function TCommTelnet.CheckOption(Option: TTelnetCommand): Boolean;
var
  Data: TBinarySerializer;
begin
  try
    Data := TBinarySerializer.Create;
    Data.List := TListByte.Create;
    Data.OwnsList := True;
    Data.WriteByte(Byte(tcWILL));
    Data.WriteByte(Byte(Option));
    RawPin.Send(Data.List);
  finally
    Data.Free;
  end;
end;

procedure TCommTelnet.SendSubCommand(Option: TTelnetCommand; Request,
  Response: TListByte);
var
  RequestData: TBinarySerializer;
  ResponseData: TBinarySerializer;
begin
  try
    RequestData := TBinarySerializer.Create;
    RequestData.List := TListByte.Create;
    RequestData.OwnsList := True;
    ResponseData := TBinarySerializer.Create;
    ResponseData.List := TListByte.Create;
    ResponseData.OwnsList := True;

    RequestData.WriteByte(Byte(Option));
    RequestData.WriteList(Request, 0, Request.Count);
    RequestData.WriteByte(Byte(tcIAC));
    RequestData.WriteByte(Byte(tcSE));
    SendCommand(tcSB, RequestData.List, ResponseData.List);
    ResponseData.Position := 0;
    if ResponseData.ReadByte <> Byte(Option) then
      raise Exception.Create(SWrongResponseOption);
  finally
    RequestData.Free;
    RequestData.Free;
  end;
end;

procedure TCommTelnet.SendCommand(Code: TTelnetCode; Request,
  Response: TListByte);
var
  Data: TBinarySerializer;
begin
  try
    Data := TBinarySerializer.Create;
    Data.List := TListByte.Create;
    Data.OwnsList := True;
    Data.WriteByte(Byte(tcIAC));
    Data.WriteByte(Byte(Code));
    RawPin.Send(Data.List);
//    repeat

//    until ;
  finally
    Data.Free;
  end;
end;

constructor TCommTelnet.Create;
begin
  FCommandData := TStreamHelper.Create;
  TelnetPin := TCommPin.Create;
  RawPin := TCommPin.Create;
  Options := TListObject.Create;
  Timeout := 2 * OneSecond;
end;

destructor TCommTelnet.Destroy;
begin
  FCommandData.Free;
  Options.Free;
  TelnetPin.Free;
  RawPin.Free;
  inherited Destroy;
end;

end.

