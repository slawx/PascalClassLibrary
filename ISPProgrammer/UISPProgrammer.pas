unit UISPProgrammer;

{$mode delphi}

interface

uses
  Classes, SysUtils, Registry, UIntelHexFile, UCommSerialPort,
  UCPUType, UJobProgressView;

type
  TLogEvent = procedure (Text: string) of object;

  TISPProgCapability = (ipcRead, ipcWrite, ipcReset, ipcErase);
  TISPProgCapabilities = set of TISPProgCapability;

  { TISPProgrammer }

  TISPProgrammer = class
  private
    FActive: Boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnLog: TLogEvent;
    FCPUType: TCPUType;
  protected
    function GetCPUType: TCPUType; virtual;
    procedure SetCPUType(AValue: TCPUType); virtual;
    procedure SetActive(AValue: Boolean); virtual; // private
  public
    HexFile: TIntelHexFile;
    FileName: string;
    SerialPort: TCommSerialPort;
    Capabilities: TISPProgCapabilities;
    procedure Log(Text: string);
    procedure LoadFromRegistry(Root: HKEY; Key: string); virtual;
    procedure SaveToRegistry(Root: HKEY; Key: string); virtual;
    procedure Write(Job: TJob); virtual;
    procedure Read(Job: TJob); virtual;
    procedure Verify(Job: TJob); virtual;
    procedure Erase; virtual;
    procedure Reset; virtual;
    function ReadIdentification: string; virtual;
    constructor Create; virtual;
    destructor Destroy; override;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property Active: Boolean read FActive write SetActive;
    property CPUType: TCPUType read GetCPUType write SetCPUType;
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
  end;

implementation

resourcestring
  SNotImplemented = 'Not implemented';

{ TISPProgrammer }

function TISPProgrammer.GetCPUType: TCPUType;
begin
  Result := FCPUType;
end;

procedure TISPProgrammer.SetCPUType(AValue: TCPUType);
begin
  FCPUType := AValue;
end;

procedure TISPProgrammer.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if AValue then begin
    if Assigned(FOnActivate) then
      FOnActivate(Self);
  end else begin
    if Assigned(FOnDeactivate) then
      FOnDeactivate(Self);
  end;
end;

procedure TISPProgrammer.Log(Text: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Text);
end;

procedure TISPProgrammer.LoadFromRegistry(Root: HKEY; Key: string);
begin

end;

procedure TISPProgrammer.SaveToRegistry(Root: HKEY; Key: string);
begin

end;

procedure TISPProgrammer.Write(Job: TJob);
begin
  raise Exception.Create(SNotImplemented);
end;

procedure TISPProgrammer.Read(Job: TJob);
begin
  raise Exception.Create(SNotImplemented);
end;

procedure TISPProgrammer.Verify(Job: TJob);
begin
  raise Exception.Create(SNotImplemented);
end;

procedure TISPProgrammer.Erase;
begin
  raise Exception.Create(SNotImplemented);
end;

procedure TISPProgrammer.Reset;
begin
  raise Exception.Create(SNotImplemented);
end;

function TISPProgrammer.ReadIdentification: string;
begin
  Result := '';
  raise Exception.Create(SNotImplemented);
end;

constructor TISPProgrammer.Create;
begin
  HexFile := TIntelHexFile.Create;
  HexFile.BytePerLine := 20;
  SerialPort := nil;
end;

destructor TISPProgrammer.Destroy;
begin
  Active := False;
  HexFile.Free;
  inherited Destroy;
end;

end.

