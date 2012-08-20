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
    FActive: Boolean;
    FOnRequest: TTelnetOptionEvent;
  protected
    procedure SetActive(AValue: Boolean); virtual;
  public
    Telnet: TCommTelnet;
    Code: TTelnetCommand;
    ServerChecked: Boolean;
    SupportedByServer: Boolean;
    procedure Assign(Source: TTelnetOption); virtual;
    function CheckOption: Boolean;
    procedure SendCommand(Request, Response: TListByte);
    property OnRequest: TTelnetOptionEvent read FOnRequest write FOnRequest;
    property Active: Boolean read FActive write SetActive;
  end;

  TTelnetPortType = (ptClient, ptServer);

  { TCommTelnet }

  TCommTelnet = class(TCommNode)
  private
    FResponses: TListObject; // TListObject<TListByte>
    FActive: Boolean;
    FState: TTelnetState;
    FCommandData: TBinarySerializer;
    procedure SetActive(AValue: Boolean);
    procedure TelnetDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure RawDataReceive(Sender: TCommPin; Stream: TListByte);
    procedure ReadResponse(Response: TListByte);
    function ResponseCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    Options: TListObject;
    TelnetPin: TCommPin;
    RawPin: TCommPin;
    Timeout: TDateTime;
    PortType: TTelnetPortType;
    ErrorCount: Integer;
    OptionsNegotationEnable: Boolean;
    procedure Register(Option: TTelnetOption);
    procedure Unregister(Option: TTelnetOption);
    function CheckOption(OptionCode: TTelnetCommand): Boolean;
    function SearchOption(OptionCode: TTelnetCommand): TTelnetOption;
    procedure SendSubCommand(OptionCode: TTelnetCommand; Request, Response: TListByte);
    procedure SendCommand(Code: TTelnetCode; Request, Response: TListByte);
    procedure Purge;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
  end;


implementation

resourcestring
  SUnknownState = 'Unknown state';
  SWrongResponseOption = 'Wrong response option';
  SWrongResponseCode = 'Wrong response code';
  SWrongResponse = 'Wrong response';
  SOptionNotFound = 'Option not found';
  STimeout = 'Telnet command timout';

{ TTelnetOption }

procedure TTelnetOption.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
end;

procedure TTelnetOption.Assign(Source: TTelnetOption);
begin
  SupportedByServer := Source.SupportedByServer;
  ServerChecked := Source.ServerChecked;
  Code := Source.Code;
  FOnRequest := Source.FOnRequest;
  Active := Source.FActive;
end;

function TTelnetOption.CheckOption: Boolean;
var
  RequestData: TBinarySerializer;
  ResponseData: TBinarySerializer;
begin
  if not ServerChecked then
  try
    RequestData := TBinarySerializer.Create;
    RequestData.List := TListByte.Create;
    RequestData.OwnsList := True;
    ResponseData := TBinarySerializer.Create;
    ResponseData.List := TListByte.Create;
    ResponseData.OwnsList := True;

    RequestData.WriteByte(Byte(Code));
    Telnet.SendCommand(tcDo, RequestData.List, ResponseData.List);
    if ResponseData.List[0] = Byte(tcWILL) then SupportedByServer := True
      else if ResponseData.List[0] = Byte(tcWONT) then SupportedByServer := False
      else raise Exception.Create(SWrongResponse);
    ServerChecked := True;
  finally
    RequestData.Free;
    RequestData.Free;
  end;
end;

procedure TTelnetOption.SendCommand(Request, Response: TListByte);
var
  RequestData: TBinarySerializer;
  ResponseData: TBinarySerializer;
  I: Integer;
begin
  if Telnet.OptionsNegotationEnable then CheckOption;
  try
    RequestData := TBinarySerializer.Create;
    RequestData.List := TListByte.Create;
    RequestData.OwnsList := True;
    ResponseData := TBinarySerializer.Create;
    ResponseData.List := TListByte.Create;
    ResponseData.OwnsList := True;

    RequestData.WriteByte(Byte(Code));
    RequestData.WriteList(Request, 0, Request.Count);
    if Assigned(Response) then begin
      Telnet.SendCommand(tcSB, RequestData.List, ResponseData.List);
      if ResponseData.List[0] <> Byte(Code) then
        raise Exception.Create(SWrongResponseOption);
      ResponseData.List.Delete(0);
      Response.Assign(ResponseData.List);
    end else Telnet.SendCommand(tcSB, RequestData.List, nil);
  finally
    RequestData.Free;
    ResponseData.Free;
  end;
end;

{ TCommTelnet }

procedure TCommTelnet.TelnetDataReceive(Sender: TCommPin; Stream: TListByte);
var
  Data: Byte;
  RawData: TBinarySerializer;
  I: Integer;
begin
  try
    RawData := TBinarySerializer.Create;
    RawData.List := TListByte.Create;
    RawData.OwnsList := True;
    for I := 0 to Stream.Count - 1 do begin
      Data := Stream[I];
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

procedure TCommTelnet.SetActive(AValue: Boolean);
var
  I: Integer;
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  for I := 0 to Options.Count - 1 do
  with TTelnetOption(Options[I]) do begin
    if (not ServerChecked) and OptionsNegotationEnable then CheckOption;
    Active := AValue;
  end;
end;

procedure TCommTelnet.RawDataReceive(Sender: TCommPin; Stream: TListByte);
var
  Data: Byte;
  RawData: TBinarySerializer;
  I: Integer;
begin
  try
    RawData := TBinarySerializer.Create;
    RawData.List := TListByte.Create;
    RawData.OwnsList := True;

    for I := 0 to Stream.Count - 1 do begin
      Data := Stream[I];
      if FState = tsNormal then begin
        if Data = Byte(tcIAC) then begin
          FCommandData.Clear;
          FState := tsIAC;
        end else RawData.WriteByte(Data);
      end else
      if FState = tsIAC then begin
        if Data = Byte(tcIAC) then begin
          RawData.WriteByte(Data);
          FState := tsNormal;
        end else
        if Data = Byte(tcSB) then begin
          // Subnegotation
          FCommandData.WriteByte(Data);
          FState := tsSB;
        end else
        if (Data = Byte(tcWILL)) or (Data = Byte(tcDONT)) or (Data = Byte(tcWONT)) or (Data = Byte(tcDO))
        then begin
          // Three byte negotation commands
          FCommandData.WriteByte(Data);
          FState := tsOption;
        end else
        if (Data = Byte(tcAYT)) or (Data = Byte(tcNOP)) or (Data = Byte(tcGA)) or
        (Data = Byte(tcEL)) or (Data = Byte(tcEC)) or (Data = Byte(tcAO)) or
        (Data = Byte(tcIP)) or (Data = Byte(tcBREAK)) or (Data = Byte(tcDATA_MARK)) or
        (Data = Byte(tcEOR)) then begin
          // Two byte commands
          FCommandData.WriteByte(Data);
          FResponses.AddNew(TListByte.Create);
          TListByte(FResponses.Last).Assign(FCommandData.List);         FState := tsNormal;
        end else
          FState := tsNormal;
      end else
      if FState = tsSB then begin
        // Data inside subnegotation
        if Data = Byte(tcIAC) then FState := tsSB_IAC
          else FCommandData.WriteByte(Data);
      end else
      if FState = tsSB_IAC then begin
        // End of subnegotation data
        if Data = Byte(tcSE) then begin
          FResponses.AddNew(TListByte.Create);
          TListByte(FResponses.Last).Assign(FCommandData.List);
          FState := tsNormal;
        end else begin
          Inc(ErrorCount);
          FState := tsNormal;
        end;
      end else
      if FState = tsOption then begin
        // Third byte of negotation
        FCommandData.WriteByte(Data);
        FResponses.AddNew(TListByte.Create);
        TListByte(FResponses.Last).Assign(FCommandData.List);
        FState := tsNormal;
      end else raise Exception.Create(SUnknownState);
    end;
    TelnetPin.Send(RawData.List);
  finally
    RawData.Free;
  end;
end;

procedure TCommTelnet.ReadResponse(Response: TListByte);
var
  TimeStart: TDateTime;
  ElapsedTime: TDateTime;
begin
  TimeStart := Now;
  repeat
    ElapsedTime := Now - TimeStart;
  until (ElapsedTime > Timeout) or (ResponseCount > 0);
  if ElapsedTime > Timeout then
    raise Exception.Create(STimeout);
  Response.Assign(TListByte(FResponses.First));
  FResponses.Delete(0);
end;

function TCommTelnet.ResponseCount: Integer;
begin
  Result := FResponses.Count;
end;

procedure TCommTelnet.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TCommTelnet then begin
    TCommTelnet(Dest).Timeout := Timeout;
    TCommTelnet(Dest).PortType := PortType;
    TCommTelnet(Dest).ErrorCount := ErrorCount;
    TCommTelnet(Dest).OptionsNegotationEnable := OptionsNegotationEnable;
    for I := 0 to Options.Count - 1 do begin
      TTelnetOption(TCommTelnet(Dest).Options[I]).Assign(TTelnetOption(Options[I]));
      TTelnetOption(TCommTelnet(Dest).Options[I]).Telnet := TCommTelnet(Dest);
    end;
  end else inherited;
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

function TCommTelnet.CheckOption(OptionCode: TTelnetCommand): Boolean;
var
  Option: TTelnetOption;
begin
  Option := SearchOption(OptionCode);
  if Assigned(Option) then Result := Option.CheckOption
    else raise Exception.Create(SOptionNotFound);
end;

function TCommTelnet.SearchOption(OptionCode: TTelnetCommand): TTelnetOption;
var
  I: Integer;
begin
  I := 0;
  while (I < Options.Count) and (TTelnetOption(Options[I]).Code <> OptionCode) do
    Inc(I);
  if I < Options.Count then Result := TTelnetOption(Options[I])
    else Result := nil;
end;

procedure TCommTelnet.SendSubCommand(OptionCode: TTelnetCommand; Request,
  Response: TListByte);
var
  Option: TTelnetOption;
begin
  Option := SearchOption(OptionCode);
  if Assigned(Option) then Option.SendCommand(Request, Response)
    else raise Exception.Create(SOptionNotFound);
end;

procedure TCommTelnet.SendCommand(Code: TTelnetCode; Request,
  Response: TListByte);
var
  Data: TBinarySerializer;
  LastIAC: Boolean;
  I: Integer;
begin
  try
    Data := TBinarySerializer.Create;
    Data.List := TListByte.Create;
    Data.OwnsList := True;
    Data.WriteByte(Byte(tcIAC));
    Data.WriteByte(Byte(Code));
    for I := 0 to Request.Count - 1 do begin
      if Request[I] = Byte(tcIAC) then Data.WriteByte(Byte(tcIAC));
      Data.WriteByte(Request[I]);
    end;
    if Code = tcSB then begin
      Data.WriteByte(Byte(tcIAC));
      Data.WriteByte(Byte(tcSE));
    end;
    RawPin.Send(Data.List);
    if Assigned(Response) then begin
      ReadResponse(Response);
      if Response[0] <> Byte(Code) then
        raise Exception.Create(SWrongResponseCode);
      Response.Delete(0);
      if Code = tcSB then begin
        if (Response[Response.Count - 2] <> Byte(tcIAC)) or
        (Response[Response.Count - 1] <> Byte(tcSE)) then
          raise Exception.Create(SWrongResponse);
        Response.DeleteItems(Response.Count - 2, 2);
      end;
      // Remove IAC escape character from data
      I := 0;
      while (I < Response.Count) do begin
        if Response[I] = Byte(tcIAC) then begin
          if not LastIAC then LastIAC := True
          else begin
            LastIAC := False;
            Response.Delete(I);
            Dec(I);
          end;
        end;
        Inc(I);
      end;
    end;
  finally
    Data.Free;
  end;
end;

procedure TCommTelnet.Purge;
begin
  FState := tsNormal;
  FResponses.Clear;
end;

constructor TCommTelnet.Create(AOwner: TComponent);
begin
  inherited;
  FResponses := TListObject.Create;
  FCommandData := TBinarySerializer.Create;
  FCommandData.List := TListByte.Create;
  FCommandData.OwnsList := True;
  TelnetPin := TCommPin.Create;
  TelnetPin.OnReceive := TelnetDataReceive;
  TelnetPin.Node := Self;
  RawPin := TCommPin.Create;
  RawPin.OnReceive := RawDataReceive;
  RawPin.Node := Self;
  Options := TListObject.Create;
  Options.OwnsObjects := False;
  Timeout := 2 * OneSecond;
end;

destructor TCommTelnet.Destroy;
begin
  FreeAndNil(FCommandData);
  FreeAndNil(Options);
  FreeAndNil(TelnetPin);
  FreeAndNil(RawPin);
  FreeAndNil(FResponses);
  inherited Destroy;
end;

end.

