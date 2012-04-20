unit UAudioSystem;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
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
    property OutputMode: TOutputDriver read FOutputDriver write SetOutputMode;
  end;

  TAudioSystemClass = class of TAudioSystem;

  { TPlayer }

  TPlayer = class(TComponent)
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TPlayerClass = class of TPlayer;

  { TAudioSystemManagerItem }

  TAudioSystemManagerItem = class
    Name: string;
    SystemClass: TAudioSystemClass;
    PlayerClass: TPlayerClass;
  end;

  { TAudioSystemManager }

  TAudioSystemManager = class(TComponent)
    Systems: TObjectList; // TListObject<TAudioSystem>
    procedure Register(Name: string; SystemClass: TAudioSystemClass;
      PlayerClass: TPlayerClass);
    procedure FillStringList(StringList: TStrings);
    function SearchByName(Name: string): TAudioSystemManagerItem;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

resourcestring
  SOpenOutputFailed = 'Failed opening audio output';


implementation

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

function TAudioSystemManager.SearchByName(Name: string): TAudioSystemManagerItem;
var
  I: Integer;
begin
  I := 0;
  while (I < Systems.Count) and (TAudioSystemManagerItem(Systems[I]).Name <> Name) do Inc(I);
  if I < Systems.Count then Result := TAudioSystemManagerItem(Systems[I])
    else Result := nil;
end;

procedure TAudioSystemManager.Register(Name: string;
  SystemClass: TAudioSystemClass; PlayerClass: TPlayerClass);
var
  NewItem: TAudioSystemManagerItem;
begin
  NewItem := TAudioSystemManagerItem.Create;
  NewItem.Name := Name;
  NewItem.SystemClass := SystemClass;
  NewItem.PlayerClass := PlayerClass;
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

{ TPlayer }

procedure TPlayer.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
end;

procedure TPlayer.SetPlaying(AValue: Boolean);
begin
  if FPlaying = AValue then Exit;
  if AValue then Play else Stop;
end;

function TPlayer.GetMuted: Boolean;
begin
  Result := False;
end;

procedure TPlayer.SetMuted(AValue: Boolean);
begin
end;

function TPlayer.GetLength: TDateTime;
begin
  Result := 0;
end;

function TPlayer.GetPosition: TDateTime;
begin
  Result := 0;
end;

function TPlayer.GetVolume: Real;
begin
  Result := 0;
end;

procedure TPlayer.SetPosition(AValue: TDateTime);
begin
end;

procedure TPlayer.SetVolume(AValue: Real);
begin
end;

procedure TPlayer.SetFileName(AValue: string);
begin
  if AValue = FFileName then Exit;
  FFileName := AValue;
  Close;
  Open;
end;

procedure TPlayer.Play;
begin
end;

procedure TPlayer.Pause;
begin
end;

procedure TPlayer.Stop;
begin
end;

procedure TPlayer.Open;
begin
  Active := True;
end;

procedure TPlayer.Close;
begin
  Active := False;
end;

constructor TPlayer.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TPlayer.Destroy;
begin
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

end.

