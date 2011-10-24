unit UCommThread;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, blcksock, UCommPin, SyncObjs, UStreamHelper, UCommon,
  DateUtils, UThreading;

type
  TCommThread = class;

  TReceiveDataEvent = procedure(Stream: TMemoryStream) of object;

  { TCommThreadReceiveThread }

  TCommThreadReceiveThread = class(TListedThread)
  public
    Parent: TCommThread;
    Stream: TStreamHelper;
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
    FStatusEvent: TEvent;
    FStatusValue: Integer;
    procedure PinReceiveData(Sender: TCommPin; Stream: TStream);
    procedure PinSetStatus(Sender: TCommPin; Status: Integer);
    procedure ExtReceiveData(Sender: TCommPin; Stream: TStream);
    procedure ExtSetStatus(Sender: TCommPin; AStatus: Integer);
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

procedure TCommThread.PinReceiveData(Sender: TCommPin; Stream:TStream);
begin
  if FActive then Ext.Send(Stream);
end;

procedure TCommThread.PinSetStatus(Sender: TCommPin; Status: Integer);
begin
  if FActive then Ext.Status := Status;
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

procedure TCommThread.ExtSetStatus(Sender: TCommPin; AStatus: Integer);
begin
  try
    FInputBufferLock.Acquire;
    FStatusValue := AStatus;
    FStatusEvent.SetEvent;
  finally
    FInputBufferLock.Release;
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
    FReceiveThread.Name := 'CommThread';
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
  Ext.OnSetSatus := ExtSetStatus;
  Pin := TCommPin.Create;
  Pin.OnReceive := PinReceiveData;
  Pin.OnSetSatus := PinSetStatus;
  FDataAvailable := TSimpleEvent.Create;
  FStatusEvent := TSimpleEvent.Create;
end;

destructor TCommThread.Destroy;
begin
  Active := False;
  FInputBuffer.Free;
  FInputBufferLock.Free;
  Ext.Free;
  Pin.Free;
  FStatusEvent.Free;
  FDataAvailable.Free;
  inherited Destroy;
end;

{ TCommThreadReceiveThread }

procedure TCommThreadReceiveThread.Execute;
begin
  try
    with Parent do
    repeat
      // Check if new data arrived
      if FDataAvailable.WaitFor(1) = wrSignaled then
      try
        FInputBufferLock.Acquire;
        Stream.Size := 0;
        Stream.WriteStream(FInputBuffer, FInputBuffer.Size);
        Pin.Send(Stream);
      finally
        FDataAvailable.ResetEvent;
        FInputBuffer.Clear;
        FInputBufferLock.Release;
      end; // else Yield;

      // Check if state changed
      if FStatusEvent.WaitFor(1) = wrSignaled then
      try
        FInputBufferLock.Acquire;
        Pin.Status := FStatusValue;
      finally
        FStatusEvent.ResetEvent;
        FInputBufferLock.Release;
      end;
    until Terminated;
  finally
  end;
end;

constructor TCommThreadReceiveThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt);
begin
  inherited;
  Stream := TStreamHelper.Create;
end;

destructor TCommThreadReceiveThread.Destroy;
begin
  Stream.Free;
  inherited;
end;

end.

