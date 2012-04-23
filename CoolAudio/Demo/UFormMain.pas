unit UFormMain;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, ActnList, UAudioSystem,
  UCoolAudio, UPlaylist;

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
    Label2: TLabel;
    LabelPosition: TLabel;
    MediaPlayer: TMediaPlayer;
    OpenDialog1: TOpenDialog;
    PlayList: TPlayList;
    TimerPlayback: TTimer;
    TrackBarPosition: TTrackBar;
    TrackBarVolume: TTrackBar;
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
    procedure FormShow(Sender: TObject);
    procedure TimerPlaybackTimer(Sender: TObject);
    procedure TrackBarPositionChange(Sender: TObject);
    procedure TrackBarVolumeChange(Sender: TObject);
  private
  public
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
  PlayList := TPlayList.Create(nil);
  AudioSystemManager.FillStringList(ComboBoxBackend.Items);
  if ComboBoxBackend.Items.Count > 0 then
    ComboBoxBackend.ItemIndex := 0;
  ComboBoxBackendChange(Self);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PlayList);
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormMain.TimerPlaybackTimer(Sender: TObject);
begin
  if Assigned(MediaPlayer) and MediaPlayer.Playing then begin
    TrackBarPosition.OnChange := nil;
    TrackBarPosition.Position := Trunc(MediaPlayer.Position / MediaPlayer.Length * TrackBarPosition.Max);
    TrackBarVolume.OnChange := nil;
    TrackBarVolume.Position := Trunc(MediaPlayer.Volume * TrackBarVolume.Max);
    Application.ProcessMessages;
    TrackBarPosition.OnChange := TrackBarPositionChange();
    TrackBarVolume.OnChange := TrackBarVolumeChange();
    LabelPosition.Caption := 'Position: ' + TimeToStr(MediaPlayer.Position) + ' / ' + TimeToStr(MediaPlayer.Length);
  end;
end;

procedure TFormMain.TrackBarPositionChange(Sender: TObject);
begin
  MediaPlayer.Position := TrackBarPosition.Position / TrackBarPosition.Max * MediaPlayer.Length;
end;

procedure TFormMain.TrackBarVolumeChange(Sender: TObject);
begin
  MediaPlayer.Volume := TrackBarVolume.Position / TrackBarVolume.Max;
end;

procedure TFormMain.UpdateInterface;
begin
  APlay.Enabled := not MediaPlayer.Playing;
  APause.Enabled := MediaPlayer.Playing;
  AStop.Enabled := MediaPlayer.Playing;
  Caption := ApplicationTitle;
  if Assigned(MediaPlayer) then Caption := MediaPlayer.FileName + ' - ' + Caption;
end;

procedure TFormMain.ComboBoxBackendChange(Sender: TObject);
begin
  if ComboBoxBackend.ItemIndex <> - 1 then begin
    with TAudioSystemManagerItem(ComboBoxBackend.Items.Objects[ComboBoxBackend.ItemIndex]) do begin
      DefaultAudioSystem := SystemClass.Create(nil);
      MediaPlayer.AudioSystem := DefaultAudioSystem;
      PlayList.Player := MediaPlayer;
      //MediaPlayer.Active := True;
    end;
  end;
end;

procedure TFormMain.APlayExecute(Sender: TObject);
begin
  MediaPlayer.Play;
  UpdateInterface;
end;

procedure TFormMain.APlayNextExecute(Sender: TObject);
begin
  PlayList.PlayNext;
  FormPlaylist.ReloadList;
end;

procedure TFormMain.APlayPreviousExecute(Sender: TObject);
begin
  PlayList.PlayPrevious;
end;

procedure TFormMain.AShowPlaylistExecute(Sender: TObject);
begin
  FormPlaylist.Show;
end;

procedure TFormMain.AStopExecute(Sender: TObject);
begin
  MediaPlayer.Stop;
  UpdateInterface;
end;

procedure TFormMain.APauseExecute(Sender: TObject);
begin
  MediaPlayer.Pause;
  UpdateInterface;
end;

procedure TFormMain.AOpenExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    MediaPlayer.FileName := OpenDialog1.FileName;
    PlayList.AddFile(OpenDialog1.FileName);
    MediaPlayer.Play;
    UpdateInterface;
  end;
end;

end.

