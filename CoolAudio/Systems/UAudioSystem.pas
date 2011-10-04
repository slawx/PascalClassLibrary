unit UAudioSystem;

{$mode objfpc}{$H+}

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
    constructor Create; virtual;
    destructor Destroy; override;
    property OutputMode: TOutputDriver read FOutputDriver write SetOutputMode;
  end;

  TAudioSystemClass = class of TAudioSystem;

  { TPlayer }

  TPlayer = class(TComponent)
  private
    procedure SetPlaying(AValue: Boolean);
  protected
    FFileName: string;
    FAudioSystem: TAudioSystem;
    FPlaying: Boolean;
    function GetMuted: Boolean; virtual; abstract;
    procedure SetMuted(AValue: Boolean); virtual; abstract;
    function GetLength: TDateTime; virtual; abstract;
    function GetPosition: TDateTime; virtual; abstract;
    function GetVolume: Real; virtual; abstract;
    procedure SetPosition(AValue: TDateTime); virtual; abstract;
    procedure SetVolume(AValue: Real); virtual; abstract;
    procedure SetFileName(AValue: string); virtual;
  public
    procedure Play; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Position: TDateTime read GetPosition write SetPosition;
    property Length: TDateTime read GetLength;
    property Volume: Real read GetVolume write SetVolume; // 0..1
    property Muted: Boolean read GetMuted write SetMuted;
    property AudioSystem: TAudioSystem read FAudioSystem write FAudioSystem;
    property FileName: string read FFileName write SetFileName;
    property Playing: Boolean read FPlaying write SetPlaying;
    constructor Create; virtual;
  end;

  TPlayerClass = class of TPlayer;

  { TAudioSystemManagerItem }

  TAudioSystemManagerItem = class
    Name: string;
    SystemClass: TAudioSystemClass;
    PlayerClass: TPlayerClass;
  end;

  { TAudioSystemManager }

  TAudioSystemManager = class
    Systems: TObjectList; // TListObject<TAudioSystem>
    procedure Register(Name: string; SystemClass: TAudioSystemClass;
      PlayerClass: TPlayerClass);
    procedure FillStringList(StringList: TStrings);
    constructor Create;
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

constructor TAudioSystemManager.Create;
begin
  Systems := TObjectList.Create;
end;

destructor TAudioSystemManager.Destroy;
begin
  Systems.Free;
  inherited Destroy;
end;

{ TPlayer }

procedure TPlayer.SetPlaying(AValue: Boolean);
begin
  if FPlaying = AValue then Exit;
  if AValue then Play else Stop;
end;

procedure TPlayer.SetFileName(AValue: string);
begin
  if AValue = FFileName then Exit;
  FFileName := AValue;
end;

constructor TPlayer.Create;
begin

end;

{ TAudioSystem }

procedure TAudioSystem.SetOutputMode(AValue: TOutputDriver);
begin
  if FOutputDriver = AValue then Exit;
  FOutputDriver := AValue;
end;

constructor TAudioSystem.Create;
begin
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

