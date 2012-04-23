unit UFormMain;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, UAudioSystem, UAudioSystemFMOD,
  UAudioSystemMPlayer, UCoolAudio, UPlaylist;

type

  { TFormMain }

  TFormMain = class(TForm)
    AOpen: TAction;
    AShowPlaylist: TAction;
    APause: TAction;
    AStop: TAction;
    APlay: TAction;
    APlayNext: TAction;
    APlayPrevious: TAction;
    ActionList1: TActionList;
    ButtonNext: TButton;
    ButtonPrevious: TButton;
    ButtonOpen: TButton;
    ButtonPlaylist: TButton;
    ButtonStop: TButton;
    ButtonPlay: TButton;
    ButtonPause: TButton;
    ComboBoxBackend: TComboBox;
    Label1: TLabel;
    LabelPosition: TLabel;
    OpenDialog1: TOpenDialog;
    TimerPlayback: TTimer;
    TrackBar1: TTrackBar;
    procedure AOpenExecute(Sender: TObject);
    procedure APauseExecute(Sender: TObject);
    procedure APlayExecute(Sender: TObject);
    procedure APlayNextExecute(Sender: TObject);
    procedure APlayPreviousExecute(Sender: TObject);
    procedure AShowPlaylistExecute(Sender: TObject);
    procedure AStopExecute(Sender: TObject);
    procedure ComboBoxBackendChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerPlaybackTimer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    AudioSystem: TAudioSystem;
    Player: TPlayer;
    Playlist: TPlaylist;
    procedure UpdateInterface;
  end;

var
  FormMain: TFormMain;

const
  ApplicationTitle = 'CoolAudio demo';

implementation

{$R *.lfm}

uses
  UFormPlaylist;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Playlist := TPlaylist.Create(nil);
  AudioSystemManager.FillStringList(ComboBoxBackend.Items);
  if ComboBoxBackend.Items.Count > 0 then
    ComboBoxBackend.ItemIndex := 0;
  ComboBoxBackendChange(Self);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Player);
  FreeAndNil(AudioSystem);
  FreeAndNil(Playlist);
end;

procedure TFormMain.TimerPlaybackTimer(Sender: TObject);
begin
  if Assigned(Player) and Player.Playing then begin
    TrackBar1.OnChange := nil;
    TrackBar1.Position := Trunc(Player.Position / Player.Length * TrackBar1.Max);
    Application.ProcessMessages;
    TrackBar1.OnChange := TrackBar1Change;
    LabelPosition.Caption := 'Position: ' + TimeToStr(Player.Position) + ' / ' + TimeToStr(Player.Length);
  end;
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  Player.Position := TrackBar1.Position / TrackBar1.Max * Player.Length;
end;

procedure TFormMain.UpdateInterface;
begin
  Caption := ApplicationTitle;
  if Assigned(Player) then Caption := Player.FileName + ' - ' + Caption;
end;

procedure TFormMain.ComboBoxBackendChange(Sender: TObject);
begin
  FreeAndNil(Player);
  FreeAndNil(AudioSystem);
  if ComboBoxBackend.ItemIndex <> - 1 then begin
    with TAudioSystemManagerItem(ComboBoxBackend.Items.Objects[ComboBoxBackend.ItemIndex]) do begin
      AudioSystem := SystemClass.Create(nil);
      Player := PlayerClass.Create(nil);
      Player.AudioSystem := AudioSystem;
      Playlist.Player := Player;
      //Player.Active := True;
    end;
  end;
end;

procedure TFormMain.APlayExecute(Sender: TObject);
begin
  Player.Play;
end;

procedure TFormMain.APlayNextExecute(Sender: TObject);
begin
  Playlist.PlayNext;
  FormPlaylist.ReloadList;
end;

procedure TFormMain.APlayPreviousExecute(Sender: TObject);
begin
  Playlist.PlayPrevious;
end;

procedure TFormMain.AShowPlaylistExecute(Sender: TObject);
begin
  FormPlaylist.Show;
end;

procedure TFormMain.AStopExecute(Sender: TObject);
begin
  Player.Stop;
end;

procedure TFormMain.APauseExecute(Sender: TObject);
begin
  Player.Pause;
end;

procedure TFormMain.AOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Player.FileName := OpenDialog1.FileName;
    Playlist.AddFile(OpenDialog1.FileName);
    Player.Play;
  end;
end;

end.

