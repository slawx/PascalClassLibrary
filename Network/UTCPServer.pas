unit UTCPServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF WINDOWS}
  WinSock,
  {$ELSE}
  baseunix, sockets,
  //LibC,
  {$ENDIF}
  BlckSock, UPool, UResetableThread;

type
  TTCPServer = class;

  { TTCPClientThread }

  TTCPClientThread = class(TResetableThread)
    Parent: TTCPServer;
    Socket: TTCPBlockSocket;
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TClientThreadedPool }

  TClientThreadedPool = class(TThreadedPool)
  private
    FActive: Boolean;
    procedure SetActive(const AValue: Boolean);
  public
    property Active: Boolean read FActive write SetActive;
  end;

  { TAcceptThread }

  TAcceptThread = class(TThread)
    Parent: TTCPServer;
    procedure Execute; override;
  end;

  { TTCPServer }

  TTCPServer = class
  private
    FOnClientConnect: TNotifyEvent;
    Socket: TTCPBlockSocket;
    FActive: Boolean;
    AcceptThread: TAcceptThread;
    procedure SetActive(const AValue: Boolean);
  public
    ThreadPool: TClientThreadedPool;
    Address: string;
    Port: Word;
    constructor Create;
    destructor Destroy; override;
    property Active: Boolean read FActive write SetActive;
    property OnClientConnect: TNotifyEvent read FOnClientConnect
      write FOnClientConnect;
  end;

implementation

{ TTCPServer }

procedure TTCPServer.SetActive(const AValue: Boolean);
begin
  if AValue and not FActive then begin
    with Socket do begin
      ThreadPool.Active := True;
      CreateSocket;
      SetLinger(True, 10);
      WriteLn(Address + ':' + IntToStr(Port));
      Bind(Address, IntToStr(Port));
      WriteLn(LastError);
      if LastError <> 0 then raise Exception.Create('Socket bind error');
      Listen;
      if LastError <> 0 then raise Exception.Create('Socket listen error');
      AcceptThread := TAcceptThread.Create(True);
      AcceptThread.Parent := Self;
      AcceptThread.FreeOnTerminate := False;
      AcceptThread.Resume;
    end;
  end else
  if not AValue and FActive then begin
    with Socket do begin
      AcceptThread.Terminate;
      AcceptThread.WaitFor;
      AcceptThread.Destroy;
      ThreadPool.Active := False;
      CloseSocket;
    end;
  end;
  FActive := AValue;
end;

constructor TTCPServer.Create;
begin
  ThreadPool := TClientThreadedPool.Create;
  ThreadPool.TotalCount := 10;
  ThreadPool.Active := True;

  Socket := TTCPBlockSocket.Create;
  Address := '0.0.0.0';
  Port := 80;
end;

destructor TTCPServer.Destroy;
begin
  ThreadPool.Destroy;
  Active := False;
  Socket.Destroy;
  inherited Destroy;
end;

{ TAcceptThread }

procedure TAcceptThread.Execute;
var
  NewSocket: TSocket;
  NewObject: TTCPClientThread;
begin
  repeat
    if Parent.Socket.CanRead(1000) then begin
      NewSocket := Parent.Socket.Accept;
      if Parent.Socket.LastError = 0 then begin
        NewObject := TTCPClientThread(Parent.ThreadPool.Acquire);
        NewObject.Parent := Parent;
        NewObject.Socket.Socket := NewSocket;
        NewObject.Start;
      end;
    end;
  until Terminated;
end;

{ TTCPClientThread }

procedure TTCPClientThread.Execute;
begin
  if Assigned(Parent.FOnClientConnect) then
    Parent.FOnClientConnect(Self);

  Parent.ThreadPool.Release(Self);
end;

constructor TTCPClientThread.Create;
begin
  inherited;
  Socket := TTCPBlockSocket.Create;
end;

destructor TTCPClientThread.Destroy;
begin
  Socket.Destroy;
  inherited;
end;

{ TClientThreadedPool }

procedure TClientThreadedPool.SetActive(const AValue: Boolean);
var
  I: Integer;
begin
  if not FActive and AValue then begin
    for I := 0 to TotalCount - 1 do begin
      TThreadedPoolItem(Items[I]).Item := TTCPClientThread.Create;
    end;
  end else
  if FActive and not AValue then begin

  end;
  FActive := AValue;
end;

end.

