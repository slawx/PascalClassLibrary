unit UMainForm;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, UAudioSystem, UAudioSystemFMOD, UAudioSystemMPlayer,
  UCoolAudio;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    ButtonStop: TButton;
    ButtonPlay: TButton;
    ButtonPause: TButton;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    LabelPosition: TLabel;
    OpenDialog1: TOpenDialog;
    TimerPlayback: TTimer;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure ButtonPauseClick(Sender: TObject);
    procedure ButtonPlayClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
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
  AudioSystemManager.FillStringList(ComboBox1.Items);
  if ComboBox1.Items.Count > 0 then
    ComboBox1.ItemIndex := 0;
  ComboBox1Change(Self);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Player);
  FreeAndNil(AudioSystem);
end;

procedure TMainForm.TimerPlaybackTimer(Sender: TObject);
begin
  if Assigned(Player) and Player.Playing then begin
    TrackBar1.OnChange := nil;
    TrackBar1.Position := Trunc(Player.Position / Player.Length * TrackBar1.Max);
    Application.ProcessMessages;
    TrackBar1.OnChange := TrackBar1Change;
    LabelPosition.Caption := 'Position: ' + TimeToStr(Player.Position) + ' / ' + TimeToStr(Player.Length);
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

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  FreeAndNil(Player);
  FreeAndNil(AudioSystem);
  if ComboBox1.ItemIndex <> - 1 then begin
    with TAudioSystemManagerItem(ComboBox1.Items.Objects[ComboBox1.ItemIndex]) do begin
      AudioSystem := SystemClass.Create;
      Player := PlayerClass.Create;
      Player.AudioSystem := AudioSystem;
      //Player.Active := True;
    end;
  end;
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

