unit UAudioSystem;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TMediaPlayerDriverClass = class of TMediaPlayerDriver;
  TOutputDriver = (omAlsa, omOSS, omDirectX, omWin32);

  EOpenOutputFailed = class(Exception);

  { TAudioSystem }

  TAudioSystem = class(TComponent)
  protected
    FOutputDriver: TOutputDriver;
    procedure SetOutputMode(AValue: TOutputDriver); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMediaPlayerDriverClass: TMediaPlayerDriverClass; virtual;
    property OutputMode: TOutputDriver read FOutputDriver write SetOutputMode;
  end;

  TAudioSystemClass = class of TAudioSystem;

  { TMediaPlayerDriver }

  TMediaPlayerDriver = class
  private
  protected
    FActive: Boolean;
    FFileName: string;
    FAudioSystem: TAudioSystem;
    FPlaying: Boolean;
    procedure SetActive(AValue: Boolean); virtual;
    procedure SetPlaying(AValue: Boolean); virtual;
    function GetMuted: Boolean; virtual;
    procedure SetMuted(AValue: Boolean); virtual;
    function GetLength: TDateTime; virtual;
    function GetPosition: TDateTime; virtual;
    function GetVolume: Real; virtual;
    procedure SetPosition(AValue: TDateTime); virtual;
    procedure SetVolume(AValue: Real); virtual;
    procedure SetFileName(AValue: string); virtual;
  public
    procedure Play; virtual;
    procedure Pause; virtual;
    procedure Stop; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    property Position: TDateTime read GetPosition write SetPosition;
    property Length: TDateTime read GetLength;
    property Volume: Real read GetVolume write SetVolume; // 0..1
    property Muted: Boolean read GetMuted write SetMuted;
    property AudioSystem: TAudioSystem read FAudioSystem write FAudioSystem;
    property FileName: string read FFileName write SetFileName;
    property Playing: Boolean read FPlaying write SetPlaying;
    property Active: Boolean read FActive write SetActive;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TMediaPlayer = class(TComponent)
  private
    procedure CheckDriver;
    function GetActive: Boolean;
    function GetAudioSystem: TAudioSystem;
    function GetFileName: string;
    function GetLength: TDateTime;
    function GetMuted: Boolean;
    function GetPlaying: Boolean;
    function GetPosition: TDateTime;
    function GetVolume: Real;
    procedure SetActive(AValue: Boolean);
    procedure SetAudioSystem(AValue: TAudioSystem);
    procedure SetFileName(AValue: string);
    procedure SetMuted(AValue: Boolean);
    procedure SetPlaying(AValue: Boolean);
    procedure SetPosition(AValue: TDateTime);
    procedure SetVolume(AValue: Real);
  public
    Driver: TMediaPlayerDriver;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Open;
    procedure Close;
    property Position: TDateTime read GetPosition write SetPosition;
    property Length: TDateTime read GetLength;
    property Playing: Boolean read GetPlaying write SetPlaying;
  published
    property Volume: Real read GetVolume write SetVolume; // 0..1
    property Muted: Boolean read GetMuted write SetMuted;
    property AudioSystem: TAudioSystem read GetAudioSystem write SetAudioSystem;
    property FileName: string read GetFileName write SetFileName;
    property Active: Boolean read GetActive write SetActive;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPlayerClass = class of TMediaPlayerDriver;

  { TAudioSystemManagerItem }

  TAudioSystemManagerItem = class
    Name: string;
    SystemClass: TAudioSystemClass;
    Supported: Boolean;
  end;

  { TAudioSystemManager }

  TAudioSystemManager = class(TComponent)
    Systems: TObjectList; // TListObject<TAudioSystem>
    procedure Register(Name: string; SystemClass: TAudioSystemClass);
    procedure FillStringList(StringList: TStrings);
    function SearchByName(Name: string; SupportedOnly: Boolean = True): TAudioSystemManagerItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  WavFileExt = '.wav';
  Mp3FileExt = '.mp3';

var
  DefaultAudioSystem: TAudioSystem;

resourcestring
  SOpenOutputFailed = 'Failed opening audio output';
  SDefaultAudioSystemNotSet = 'Default audio system not set';
  SMediaPlayerDriverNotAssigned = 'Media player driver not assigned';


implementation

{ TMediaPlayer }

function TMediaPlayer.GetLength: TDateTime;
begin
  CheckDriver;
  Result := Driver.Length;
end;

procedure TMediaPlayer.CheckDriver;
begin
  if not Assigned(Driver) then
    raise Exception.Create(SMediaPlayerDriverNotAssigned);
end;

function TMediaPlayer.GetActive: Boolean;
begin
  CheckDriver;
  Result := Driver.Active;
end;

function TMediaPlayer.GetAudioSystem: TAudioSystem;
begin
  CheckDriver;
  Result := Driver.AudioSystem;
end;

function TMediaPlayer.GetFileName: string;
begin
  CheckDriver;
  Result := Driver.FileName;
end;

function TMediaPlayer.GetMuted: Boolean;
begin
  CheckDriver;
  Result := Driver.Muted;
end;

function TMediaPlayer.GetPlaying: Boolean;
begin
  CheckDriver;
  Result := Driver.Playing;
end;

function TMediaPlayer.GetPosition: TDateTime;
begin
  CheckDriver;
  Result := Driver.Position;
end;

function TMediaPlayer.GetVolume: Real;
begin
  CheckDriver;
  Result := Driver.Volume;
end;

procedure TMediaPlayer.SetActive(AValue: Boolean);
begin
  CheckDriver;
  Driver.Active := True;
end;

procedure TMediaPlayer.SetAudioSystem(AValue: TAudioSystem);
var
  DriverClass: TMediaPlayerDriverClass;
begin
  FreeAndNil(Driver);
  DriverClass := AValue.GetMediaPlayerDriverClass;
  Driver := DriverClass.Create;
  Driver.AudioSystem := DefaultAudioSystem;
end;

procedure TMediaPlayer.SetFileName(AValue: string);
begin
  CheckDriver;
  Driver.FileName := AValue;
end;

procedure TMediaPlayer.SetMuted(AValue: Boolean);
begin
  CheckDriver;
  Driver.Muted := AValue;
end;

procedure TMediaPlayer.SetPlaying(AValue: Boolean);
begin
  CheckDriver;
  Driver.Playing := AValue;
end;

procedure TMediaPlayer.SetPosition(AValue: TDateTime);
begin
  CheckDriver;
  Driver.Position := AValue;
end;

procedure TMediaPlayer.SetVolume(AValue: Real);
begin
  CheckDriver;
  Driver.Volume := AValue;
end;

procedure TMediaPlayer.Play;
begin
  CheckDriver;
  Driver.Play;
end;

procedure TMediaPlayer.Pause;
begin
  CheckDriver;
  Driver.Pause;
end;

procedure TMediaPlayer.Stop;
begin
  CheckDriver;
  Driver.Stop;
end;

procedure TMediaPlayer.Open;
begin
  CheckDriver;
  Driver.Open;
end;

procedure TMediaPlayer.Close;
begin
  CheckDriver;
  Driver.Close;
end;

constructor TMediaPlayer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if not Assigned(DefaultAudioSystem) then
    raise Exception.Create(SDefaultAudioSystemNotSet);
  AudioSystem := DefaultAudioSystem;
end;

destructor TMediaPlayer.Destroy;
begin
  FreeAndNil(Driver);
  inherited Destroy;
end;

{ TAudioSystemManagerItem }


{ TAudioSystemManager }

procedure TAudioSystemManager.FillStringList(StringList: TStrings);
var
  I: Integer;
begin
  StringList.Clear;
  for I := 0 to Systems.Count - 1 do
  with TAudioSystemManagerItem(Systems[I]) do
    StringList.AddObject(Name, Systems[I]);
end;

function TAudioSystemManager.SearchByName(Name: string; SupportedOnly: Boolean = True):
  TAudioSystemManagerItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Systems.Count) and
    ((TAudioSystemManagerItem(Systems[I]).Name <> Name) or
    (not TAudioSystemManagerItem(Systems[I]).Supported and SupportedOnly)) do Inc(I);
  if I < Systems.Count then Result := TAudioSystemManagerItem(Systems[I])
    else Result := nil;
end;

procedure TAudioSystemManager.Register(Name: string;
  SystemClass: TAudioSystemClass);
var
  NewItem: TAudioSystemManagerItem;
begin
  NewItem := TAudioSystemManagerItem.Create;
  NewItem.Name := Name;
  NewItem.SystemClass := SystemClass;
  NewItem.Supported := True;
  Systems.Add(NewItem);
end;

constructor TAudioSystemManager.Create(AOwner: TComponent);
begin
  inherited;
  Systems := TObjectList.Create;
end;

destructor TAudioSystemManager.Destroy;
begin
  Systems.Free;
  inherited Destroy;
end;

{ TMediaPlayerDriver }

procedure TMediaPlayerDriver.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
end;

procedure TMediaPlayerDriver.SetPlaying(AValue: Boolean);
begin
  if FPlaying = AValue then Exit;
  if AValue then Play else Stop;
end;

function TMediaPlayerDriver.GetMuted: Boolean;
begin
  Result := False;
end;

procedure TMediaPlayerDriver.SetMuted(AValue: Boolean);
begin
end;

function TMediaPlayerDriver.GetLength: TDateTime;
begin
  Result := 0;
end;

function TMediaPlayerDriver.GetPosition: TDateTime;
begin
  Result := 0;
end;

function TMediaPlayerDriver.GetVolume: Real;
begin
  Result := 0;
end;

procedure TMediaPlayerDriver.SetPosition(AValue: TDateTime);
begin
end;

procedure TMediaPlayerDriver.SetVolume(AValue: Real);
begin
end;

procedure TMediaPlayerDriver.SetFileName(AValue: string);
begin
  if AValue = FFileName then Exit;
  FFileName := AValue;
  Close;
  Open;
end;

procedure TMediaPlayerDriver.Play;
begin
end;

procedure TMediaPlayerDriver.Pause;
begin
end;

procedure TMediaPlayerDriver.Stop;
begin
end;

procedure TMediaPlayerDriver.Open;
begin
  Active := True;
end;

procedure TMediaPlayerDriver.Close;
begin
  Active := False;
end;

constructor TMediaPlayerDriver.Create;
begin
  inherited;
end;

destructor TMediaPlayerDriver.Destroy;
begin
  Stop;
  Active := False;
  inherited Destroy;
end;

{ TAudioSystem }

procedure TAudioSystem.SetOutputMode(AValue: TOutputDriver);
begin
  if FOutputDriver = AValue then Exit;
  FOutputDriver := AValue;
end;

constructor TAudioSystem.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF Windows}
  FOutputDriver := omWin32;
  {$ENDIF}
  {$IFDEF Linux}
  FOutputDriver := omAlsa;
  {$ENDIF}
end;

destructor TAudioSystem.Destroy;
begin
  inherited Destroy;
end;

function TAudioSystem.GetMediaPlayerDriverClass: TMediaPlayerDriverClass;
begin
  Result := TMediaPlayerDriver;
end;

initialization

DefaultAudioSystem := TAudioSystem.Create(nil);

finalization

FreeAndNil(DefaultAudioSystem);

end.

