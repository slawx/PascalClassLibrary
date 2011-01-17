unit UCommThread;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, blcksock, UCommPin, SyncObjs, UStreamHelper, UCommon;

type
  TCommThread = class;

  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TCommThreadReceiveThread }

  TCommThreadReceiveThread = class(TThread)
  public
    Parent: TCommThread;
    Stream: TMemoryStream;
    procedure Execute; override;
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
  end;

  { TCommThread }

  TCommThread = class
  private
    FActive: Boolean;
    FOnReceiveData: TReceiveDataEvent;
    FReceiveThread: TCommThreadReceiveThread;
    FInputBuffer: TMemoryStream;
    FInputBufferLock: TCriticalSection;
    FDataAvailable: TEvent;
    procedure ReceiveData(Sender: TCommPin; Stream: TStream);
    procedure ExtReceiveData(Sender: TCommPin; Stream: TStream);
    procedure SetActive(const AValue: Boolean);
  public
    Ext: TCommPin;
    Pin: TCommPin;
    property Active: Boolean read FActive write SetActive;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCommThread }

procedure TCommThread.ReceiveData(Sender: TCommPin; Stream:TStream);
begin
  Ext.Send(Stream);
end;

procedure TCommThread.ExtReceiveData(Sender: TCommPin; Stream: TStream);
var
  StreamHelper: TStreamHelper;
begin
  try
    StreamHelper := TStreamHelper.Create(FInputBuffer);
    FInputBufferLock.Acquire;
    StreamHelper.WriteStream(Stream, Stream.Size);
    FDataAvailable.SetEvent;
  finally
    FInputBufferLock.Release;
    StreamHelper.Free;
  end;
end;

procedure TCommThread.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  if AValue then begin
    FReceiveThread := TCommThreadReceiveThread.Create(True);
    FReceiveThread.FreeOnTerminate := False;
    FReceiveThread.Parent := Self;
    FReceiveThread.Start;
  end else begin
    FReceiveThread.Terminate;
    FReceiveThread.WaitFor;
    FreeAndNil(FReceiveThread);
  end;
end;

constructor TCommThread.Create;
begin
  inherited Create;
  FInputBuffer := TMemoryStream.Create;
  FInputBufferLock := TCriticalSection.Create;
  Ext := TCommPin.Create;
  Ext.OnReceive := ExtReceiveData;
  Pin := TCommPin.Create;
  Pin.OnReceive := ReceiveData;
  FDataAvailable := TEvent.Create(nil, False, False, '');
end;

destructor TCommThread.Destroy;
begin
  Active := False;
  FInputBuffer.Free;
  FInputBufferLock.Free;
  Ext.Free;
  Pin.Free;
  FDataAvailable.Free;
  inherited Destroy;
end;

{ TCommThreadReceiveThread }

procedure TCommThreadReceiveThread.Execute;
var
  StreamHelper: TStreamHelper;
begin
  try
    StreamHelper := TStreamHelper.Create(Stream);
  with Parent do repeat
    try
      if FDataAvailable.WaitFor(1) = wrSignaled then try
        FInputBufferLock.Acquire;
        Stream.Size := 0;
        StreamHelper.WriteStream(FInputBuffer, FInputBuffer.Size);
        Pin.Send(Stream);
        FInputBuffer.Clear;
      finally
        FInputBufferLock.Release;
      end;
    except
      on E: Exception do
        if Assigned(ExceptionHandler) then ExceptionHandler(Self, E);
    end;
  until Terminated;

  finally
    StreamHelper.Free;
  end;
end;

constructor TCommThreadReceiveThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Stream := TMemoryStream.Create;
end;

destructor TCommThreadReceiveThread.Destroy;
begin
  Stream.Free;
  inherited;
end;

end.
