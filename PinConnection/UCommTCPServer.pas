unit UCommTCPServer;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, UCommPin, UCommon, UThreading,
  DateUtils, SpecializedList;

type
  TCommTCPServer = class;

  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TCommSocketReceiveThread }

  TCommSocketReceiveThread = class(TListedThread)
  public
    Parent: TCommTCPServer;
    Stream: TListByte;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  TSocketConnectEvent = procedure(Sender: TCommTCPServer; Pin: TCommPin) of object;

  { TCommTCPServerSession }

  TCommTCPServerSession = class
  private
    procedure ReceiveData(Sender: TCommPin; Stream: TListByte);
  public
    Server: TCommTCPServer;
    Socket: TTCPBlockSocket;
    Pin: TCommPin;
    constructor Create;
    destructor Destroy; override;
  end;

  { TCommTCPServer }

  TCommTCPServer = class(TCommNode)
  private
    FActive: Boolean;
    FOnConnect: TSocketConnectEvent;
    FOnDisconnect: TSocketConnectEvent;
    //FOnReceiveData: TReceiveDataEvent;
    FReceiveThread: TCommSocketReceiveThread;
  protected
    procedure SetActive(const AValue: Boolean); override;
  public
    Sessions: TListObject; // TListObject<TCommTCPServerSession>
    Socket: TTCPBlockSocket;
    Address: string;
    Port: Word;
    property Active: Boolean read FActive write SetActive;
    property OnConnect: TSocketConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TSocketConnectEvent read FOnDisconnect write FOnDisconnect;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TCommTCPServerSession }

procedure TCommTCPServerSession.ReceiveData(Sender: TCommPin; Stream: TListByte);
var
  Mem: TMemoryStream;
begin
  try
    Mem := TMemoryStream.Create;
    Stream.WriteToStream(Mem);
    Mem.Position := 0;
    Socket.SendStreamRaw(Mem);
  finally
    Mem.Free;
  end;
end;

constructor TCommTCPServerSession.Create;
begin
  Socket := TTCPBlockSocket.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
  Pin.Node := Server;
end;

destructor TCommTCPServerSession.Destroy;
begin
  Pin.Free;
  Socket.Free;
  inherited Destroy;
end;

{ TCommSocketReceiveThread }

procedure TCommSocketReceiveThread.Execute;
var
  InBufferUsed: Integer;
  Buffer: array of Byte;
  NewSocket: TSocket;
  NewSession: TCommTCPServerSession;
  I: Integer;
  DoSleep: Boolean;
begin
  with Parent do begin
    repeat
      DoSleep := True;

      // Check if new connection is available
      if Assigned(Socket) then
      with Socket do
      if CanRead(0) then begin
        NewSocket := Accept;
        if Assigned(FOnConnect) then begin
          NewSession := TCommTCPServerSession.Create;
          NewSession.Server := Parent;
          NewSession.Socket.Socket := NewSocket;
          if Assigned(FOnConnect) then FOnConnect(Parent, NewSession.Pin);
          Parent.Sessions.Add(NewSession);
          DoSleep := False;
        end;
      end;

      // Check available data on all opened sockets
      for I := 0 to Sessions.Count - 1 do
      with TCommTCPServerSession(Sessions[I]) do begin
        InBufferUsed := Socket.WaitingData;
        if (not Pin.Connected) or (Socket.Socket = INVALID_SOCKET) or
        (Socket.LastError <> 0) then begin
          if Assigned(FOnDisconnect) then FOnDisconnect(Self.Parent, Pin);
        end;

        if InBufferUsed > 0 then begin
          SetLength(Buffer, InBufferUsed);
          Socket.RecvBuffer(Buffer, Length(Buffer));
          //if Socket.Socket = INVALID_SOCKET then Break;

          Stream.Count := Length(Buffer);
          Stream.ReplaceBuffer(0, PByte(Buffer)^, Length(Buffer));
          Pin.Send(Stream);
          DoSleep := False;
        end;
      end;
      if DoSleep and not Terminated then Sleep(1);
    until Terminated;
  end;
end;

constructor TCommSocketReceiveThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Stream := TListByte.Create;
end;

destructor TCommSocketReceiveThread.Destroy;
begin
  Stream.Free;
  inherited Destroy;
end;

{ TCommTCPServer }

procedure TCommTCPServer.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  if AValue then begin
    Socket.Bind(Address, IntToStr(Port));
    if Socket.LastError <> 0 then raise Exception.Create('Bind error' + Socket.GetErrorDesc(Socket.LastError));
    Socket.Listen;
    if Socket.LastError <> 0 then raise Exception.Create('Listen error');
    FReceiveThread := TCommSocketReceiveThread.Create(True);

    FReceiveThread.FreeOnTerminate := False;
    FReceiveThread.Parent := Self;
    FReceiveThread.Start;
  end else begin
    Socket.CloseSocket;
    FReceiveThread.Terminate;
    FReceiveThread.WaitFor;
    FreeAndNil(FReceiveThread);
  end;
end;

constructor TCommTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  Sessions := TListObject.Create;
  Socket := TTCPBlockSocket.Create;
end;

destructor TCommTCPServer.Destroy;
begin
  Active := False;
  Socket.Free;
  Sessions.Free;
  inherited Destroy;
end;

end.

