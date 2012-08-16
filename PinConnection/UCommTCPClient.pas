unit UCommTCPClient;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, blcksock, synsock, UCommPin, UCommon, UThreading,
  DateUtils, SpecializedList;

type
  TCommTCPClient = class;

  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TCommSocketReceiveThread }

  TCommSocketReceiveThread = class(TListedThread)
  public
    Parent: TCommTCPClient;
    Stream: TListByte;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TCommTCPClient }

  TCommTCPClient = class
  private
    FActive: Boolean;
    FOnReceiveData: TReceiveDataEvent;
    FReceiveThread: TCommSocketReceiveThread;
    procedure ReceiveData(Sender: TCommPin; Stream: TListByte);
    procedure SetActive(const AValue: Boolean);
  public
    Socket: TTCPBlockSocket;
    Pin: TCommPin;
    Address: string;
    Port: Word;
    property Active: Boolean read FActive write SetActive;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

resourcestring
  SCantConnectToServer = 'Can''t connect to remote server';

{ TCommTCPClient }

procedure TCommTCPClient.ReceiveData(Sender: TCommPin; Stream: TListByte);
var
  Mem: TMemoryStream;
begin
  if FActive then begin
    try
      Mem := TMemoryStream.Create;
      Stream.WriteToStream(Mem);
      Mem.Position := 0;
      Socket.SendStreamRaw(Mem);
    finally
      Mem.Free;
    end;
  end;
end;

procedure TCommTCPClient.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  if AValue then begin
    Socket.Connect(Address, IntToStr(Port));
    if Socket.LastError <> 0 then begin
      FActive := False;
      raise Exception.Create(SCantConnectToServer);
    end;
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

constructor TCommTCPClient.Create;
begin
  inherited Create;
  Socket := TTCPBlockSocket.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
end;

destructor TCommTCPClient.Destroy;
begin
  Active := False;
  Socket.Free;
  Pin.Free;
  inherited Destroy;
end;

{ TCommSocketReceiveThread }

procedure TCommSocketReceiveThread.Execute;
var
  InBufferUsed: Integer;
  Buffer: array of Byte;
begin
  InBufferUsed := 0;
  with Parent do begin
    repeat
      if InBufferUsed = 0 then Sleep(1);
        //else Yield;
      if Assigned(Socket) then
      with Socket do
      if CanRead(0) then begin
        InBufferUsed := WaitingData;
        if InBufferUsed > 0 then begin
          SetLength(Buffer, InBufferUsed);
          RecvBuffer(Buffer, Length(Buffer));

          Stream.Count := Length(Buffer);
          Stream.ReplaceBuffer(0, Pointer(Buffer)^, Length(Buffer));
          Pin.Send(Stream);
        end else InBufferUsed := 0;
      end else InBufferUsed := 0;
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
  inherited;
end;

end.

