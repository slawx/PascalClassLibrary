unit UAudioSystemWindows;

{$mode objfpc}{$H+}

interface

{$IFDEF Windows}
uses
  Windows, Classes, SysUtils, UAudioSystem, MMSystem, DateUtils;

type
  TAudioSystemWindows = class(TAudioSystem)
  public
    PlayerIndex: Integer;
  end;

  TMPDeviceTypes = (dtAutoSelect, dtAVIVideo, dtCDAudio, dtDAT, dtDigitalVideo, dtMMMovie,
    dtOther, dtOverlay, dtScanner, dtSequencer, dtVCR, dtVideodisc, dtWaveAudio);

  { TPlayerWindows }

  TPlayerWindows = class(TPlayer)
  private
    FHandle: HWND;
    FDeviceId: MCIDEVICEID;
    FDeviceType: TMPDeviceTypes;
    FFlags: Longint;
    FUseNotify: Boolean;
    FNotify: Boolean;
    FUseWait: Boolean;
    FWait: Boolean;
    FAliasName: string;
    procedure DoClose;
    procedure DoOpen;
    procedure SetDeviceType(AValue: TMPDeviceTypes);
    procedure CheckError(AValue: Integer);
    function GetErrorMessage(Code: Integer): string;
    procedure SetActive(AValue: Boolean); override;
    procedure SetNotify(AValue: Boolean);
    procedure SetWait(AValue: Boolean);
    function GetPosition: TDateTime; override;
    procedure SetPosition(AValue: TDateTime); override;
    function GetLength: TDateTime; override;
  public
    procedure Play; override;
    procedure Pause; override;
    procedure Stop; override;
    constructor Create; override;
    destructor Destroy; override;
    property DeviceType: TMPDeviceTypes read FDeviceType write SetDeviceType;
    property Handle: HWND read FHandle;
    property Wait: Boolean read FWait write SetWait;
    property Notify: Boolean read FNotify write SetNotify;
  end;
{$ENDIF}

resourcestring
  SMCIUnknownError = 'Unknown error code';

implementation

{$IFDEF Windows}

{ TPlayerWindows }

procedure TPlayerWindows.SetDeviceType(AValue: TMPDeviceTypes);
begin
  if FDeviceType = AValue then Exit;
  FDeviceType := AValue;
end;

procedure TPlayerWindows.CheckError(AValue: Integer);
begin
  if AValue <> 0 then raise Exception.Create('Error ' + IntToStr(AValue) + ': ' + GetErrorMessage(AValue));
end;

function TPlayerWindows.GetErrorMessage(Code: Integer): string;
var
  ErrMsg: array[0..4095] of Char;
begin
  if not mciGetErrorString(Code, ErrMsg, SizeOf(ErrMsg)) then
    Result := SMCIUnknownError
  else SetString(Result, ErrMsg, StrLen(ErrMsg));
end;

procedure TPlayerWindows.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  inherited SetActive(AValue);
  if AValue then DoOpen else DoClose;
end;

procedure TPlayerWindows.SetNotify(AValue: Boolean);
begin
  if FNotify = AValue then Exit;
  FNotify := AValue;
  FUseNotify := True;
end;

procedure TPlayerWindows.SetWait(AValue: Boolean);
begin
  if FWait = AValue then Exit;
  FWait := AValue;
  FUseWait := True;
end;

function TPlayerWindows.GetPosition: TDateTime;
var
  Parm: TMCI_Status_Parms;
begin
  FFlags := mci_Wait or mci_Status_Item;
  Parm.dwItem := mci_Status_Position;
  CheckError(mciSendCommand(FDeviceID, mci_Status, FFlags, Longint(@Parm)));
  Result := Parm.dwReturn * OneMillisecond;
end;

procedure TPlayerWindows.SetPosition(AValue: TDateTime);
var
  Parm: TMCI_Seek_Parms;
begin
  if FDeviceID <> 0 then begin
    FFlags := 0;
    if FUseWait then
    begin
      if FWait then FFlags := mci_Wait;
      FUseWait := False;
    end
    else FFlags := mci_Wait;
    if FUseNotify then
    begin
      if FNotify then FFlags := FFlags or mci_Notify;
      FUseNotify := False;
    end;
    FFlags := FFlags or mci_To;
    Parm.dwTo := Round(AValue / OneMillisecond);
    CheckError(mciSendCommand(FDeviceID, mci_Seek, FFlags, Longint(@Parm)));
    if FPlaying then Play;
  end;
end;

function TPlayerWindows.GetLength: TDateTime;
var
  Parm: TMCI_Status_Parms;
begin
  FFlags := mci_Wait or mci_Status_Item;
  Parm.dwItem := mci_Status_Length;
  mciSendCommand(FDeviceID, mci_Status, FFlags, Longint(@Parm));
  Result := Parm.dwReturn * OneMillisecond;
end;

procedure TPlayerWindows.Play;
var
  Parm: TMCI_Play_Parms;
begin
  if FDeviceID = 0 then DoOpen;

  FFlags := 0;
  if FUseNotify then
  begin
    if FNotify then FFlags := mci_Notify;
    FUseNotify := False;
  end else FFlags := mci_Notify;
  if FUseWait then
  begin
    if FWait then FFlags := FFlags or mci_Wait;
    FUseWait := False;
  end;
  CheckError(mciSendCommand(FDeviceID, mci_Play, FFlags, Longint(@Parm)));
  FPlaying := True;
end;

procedure TPlayerWindows.Pause;
var
  Parm: TMCI_Generic_Parms;
begin
  if FPlaying then begin
    CheckError(mciSendCommand(FDeviceID, mci_Pause, FFlags, Longint(@Parm)));
    FPlaying := False;
  end else begin
    CheckError(mciSendCommand(FDeviceID, mci_Resume, FFlags, Longint(@Parm)));
    FPlaying := True;
  end;
end;

procedure TPlayerWindows.Stop;
var
  Parm: TMCI_Generic_Parms;
begin
  FFlags := 0;
  if FUseNotify then
  begin
    if FNotify then FFlags := mci_Notify;
    FUseNotify := False;
  end else FFlags := mci_Notify;
  if FUseWait then
  begin
    if FWait then FFlags := FFlags or mci_Wait;
    FUseWait := False;
  end;
  CheckError(mciSendCommand(FDeviceID, mci_Stop, FFlags, Longint(@Parm)));
  FPlaying := False;
end;

constructor TPlayerWindows.Create;
begin
  inherited Create;
end;

destructor TPlayerWindows.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TPlayerWindows.DoOpen;
const
  DeviceName: array[TMPDeviceTypes] of PChar = ('', 'AVIVideo', 'CDAudio', 'DAT',
    'DigitalVideo', 'MMMovie', 'Other', 'Overlay', 'Scanner', 'Sequencer',
    'VCR', 'Videodisc', 'WaveAudio');
var
  Parm: TMCI_Open_Parms;
begin
  if FDeviceId <> 0 then DoClose;

  FillChar(Parm, SizeOf(TMCI_Open_Parms), 0);
  Parm.dwCallback := 0;
  Parm.lpstrDeviceType := DeviceName[FDeviceType];
  Parm.lpstrElementName := PChar(FFileName);

  FFlags := 0;

  if FUseWait then
  begin
    if FWait then FFlags := mci_Wait;
    FUseWait := False;
  end
  else
    FFlags := mci_Wait;

  if FUseNotify then
  begin
    if FNotify then FFlags := FFlags or mci_Notify;
    FUseNotify := False;
  end;

  if FDeviceType <> dtAutoSelect then
    FFlags := FFlags or mci_Open_Type;

  if FDeviceType <> dtAutoSelect then
    FFlags := FFlags or mci_Open_Type
  else
    FFlags := FFlags or MCI_OPEN_ELEMENT;

  //Parm.dwCallback := Handle;
  CheckError(mciSendCommand(0, mci_Open, FFlags, Longint(@Parm)));
  FDeviceID := Parm.wDeviceID;
  FActive := True;
end;

procedure TPlayerWindows.DoClose;
var
  Parm: TMCI_Generic_Parms;
begin
  if FDeviceId <> 0 then begin
    FFlags := 0;
    if FUseWait then
    begin
      if FWait then FFlags := mci_Wait;
      FUseWait := False;
    end
    else FFlags := mci_Wait;
    if FUseNotify then
    begin
      if FNotify then FFlags := FFlags or mci_Notify;
      FUseNotify := False;
    end;
    CheckError(mciSendCommand(FDeviceId, mci_Close, FFlags, Longint(@Parm)));
    FDeviceId := 0;
    FActive := False;
  end;
end;

{$ENDIF}

end.

