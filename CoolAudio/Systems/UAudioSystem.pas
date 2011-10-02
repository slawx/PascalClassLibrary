unit UAudioSystem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  TOutputDriver = (omAlsa, omOSS, omDirectX, omWin32);

  EOpenOutputFailed = class(Exception);

  { TAudioSystem }

  TAudioSystem = class
  protected
    FOutputDriver: TOutputDriver;
    procedure SetOutputMode(AValue: TOutputDriver); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property OutputMode: TOutputDriver read FOutputDriver write SetOutputMode;
  end;

  { TPlayer }

  TPlayer = class
  private
  protected
    FPlaying: Boolean;
    function GetMuted: Boolean; virtual; abstract;
    procedure SetMuted(AValue: Boolean); virtual; abstract;
    function GetLength: TDateTime; virtual; abstract;
    function GetPosition: TDateTime; virtual; abstract;
    function GetVolume: Real; virtual; abstract;
    procedure SetPosition(AValue: TDateTime); virtual; abstract;
    procedure SetVolume(AValue: Real); virtual; abstract;
  public
    procedure Play; virtual; abstract;
    procedure Pause; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Position: TDateTime read GetPosition write SetPosition;
    property Length: TDateTime read GetLength;
    property Volume: Real read GetVolume write SetVolume; // 0..1
    property Muted: Boolean read GetMuted write SetMuted;
    constructor Create; virtual;
  end;

resourcestring
  SOpenOutputFailed = 'Failed opening audio output';


implementation

{ TPlayer }

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

end;

destructor TAudioSystem.Destroy;
begin
  inherited Destroy;
end;

end.

