unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, UAudioSystem, UAudioSystemFMOD, UAudioSystemMPlayer;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ButtonStop: TButton;
    ButtonPlay: TButton;
    ButtonPause: TButton;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    TimerPlayback: TTimer;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerPlaybackTimer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    Player: TPlayer;
    AudioSystem: TAudioSystem;
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AudioSystem := TAudioSystemMPlayer.Create;
  TAudioSystemMPlayer(AudioSystem).Path := 'c:\Program Files\SMPlayer\mplayer\mplayer.exe';
  Player := TPlayerMPlayer.Create;
  Player.AudioSystem := AudioSystem;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Player.Free;
  AudioSystem.Free;
end;

procedure TMainForm.TimerPlaybackTimer(Sender: TObject);
begin
  if Player.Playing then begin
    TrackBar1.OnChange := nil;
    TrackBar1.Position := Trunc(Player.Position / Player.Length * TrackBar1.Max);
    TrackBar1.OnChange := TrackBar1Change;
  end;
end;

procedure TMainForm.TrackBar1Change(Sender: TObject);
begin
  Player.Position := TrackBar1.Position / TrackBar1.Max * Player.Length;
end;

procedure TMainForm.ButtonPlayClick(Sender: TObject);
begin
  Player.FileName := Edit1.Text;
  Player.Play;
end;

procedure TMainForm.ButtonStopClick(Sender: TObject);
begin
  Player.Stop;
end;

procedure TMainForm.ButtonPauseClick(Sender: TObject);
begin
  Player.Pause;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then Edit1.Text := OpenDialog1.FileName;
end;

end.

