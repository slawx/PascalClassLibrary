unit UCommSocket;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, blcksock, UCommPin, UCommon;

type
  TCommSocket = class;

  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TCommSocketReceiveThread }

  TCommSocketReceiveThread = class(TThread)
  public
    Parent: TCommSocket;
    Stream: TMemoryStream;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TCommSocket }

  TCommSocket = class
  private
    FActive: Boolean;
    FOnReceiveData: TReceiveDataEvent;
    FReceiveThread: TCommSocketReceiveThread;
    procedure ReceiveData(Sender: TCommPin; Stream: TStream);
    procedure SetActive(const AValue: Boolean);
  public
    Socket: TBlockSocket;
    Pin: TCommPin;
    property Active: Boolean read FActive write SetActive;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCommSocket }

procedure TCommSocket.ReceiveData(Sender: TCommPin; Stream:TStream);
begin
  Socket.SendStreamRaw(Stream);
end;

procedure TCommSocket.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  if AValue then begin
    FReceiveThread := TCommSocketReceiveThread.Create(True);
    FReceiveThread.FreeOnTerminate := False;
    FReceiveThread.Parent := Self;
    FReceiveThread.Start;
  end else begin
    FReceiveThread.Terminate;
    FReceiveThread.WaitFor;
    FreeAndNil(FReceiveThread);
  end;
end;

constructor TCommSocket.Create;
begin
  inherited Create;
  Socket := TTCPBlockSocket.Create;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
end;

destructor TCommSocket.Destroy;
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
  with Parent do repeat
    try
      if InBufferUsed = 0 then Sleep(1);
      if Assigned(Socket) then
      with Socket do
      if CanRead(100) then begin
        InBufferUsed := WaitingData;
        if InBufferUsed > 0 then begin
          SetLength(Buffer, InBufferUsed);
          RecvBuffer(Buffer, Length(Buffer));

          Stream.Size := Length(Buffer);
          Stream.Position := 0;
          Stream.Write(Buffer[0], Length(Buffer));
          Pin.Send(Stream);
        end else InBufferUsed := 0;
      end else InBufferUsed := 0;
    except
      on E: Exception do
        if Assigned(ExceptionHandler) then ExceptionHandler(Self, E);
    end;
  until Terminated;
end;

constructor TCommSocketReceiveThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Stream := TMemoryStream.Create;
end;

destructor TCommSocketReceiveThread.Destroy;
begin
  Stream.Free;
  inherited;
end;

end.

