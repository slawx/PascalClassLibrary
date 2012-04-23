unit UFormPlaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, UPlaylist;

type

  { TFormPlaylist }

  TFormPlaylist = class(TForm)
    AClearAll: TAction;
    AItemPlay: TAction;
    AItemDelete: TAction;
    AAddFile: TAction;
    AAddDirectory: TAction;
    ActionList1: TActionList;
    Button1: TButton;
    ButtonAddFile: TButton;
    ButtonAddFolder: TButton;
    ButtonClear: TButton;
    ButtonDelete: TButton;
    CheckBoxRandom: TCheckBox;
    CheckBoxRepeat: TCheckBox;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    procedure AAddDirectoryExecute(Sender: TObject);
    procedure AAddFileExecute(Sender: TObject);
    procedure AClearAllExecute(Sender: TObject);
    procedure AItemDeleteExecute(Sender: TObject);
    procedure AItemPlayExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure ListView1Data(Sender: TObject; Item: TListItem);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1KeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
  public
    procedure ReloadList;
    procedure UpdateInterface;
  end;

var
  FormPlaylist: TFormPlaylist;


implementation

uses
  UAudioSystem, UFormMain;

resourcestring
  SSelectDirectory = 'Select directory';

{$R *.lfm}

{ TFormPlaylist }

procedure TFormPlaylist.FormCreate(Sender: TObject);
begin
end;

procedure TFormPlaylist.AClearAllExecute(Sender: TObject);
begin
  FormMain.Playlist.Items.Clear;
  ReloadList;
end;

procedure TFormPlaylist.AItemDeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  with FormMain.Playlist, Items do begin
    for I := Count - 1 downto 0 do
      if ListView1.Items[I].Selected then Delete(I);
    if Count > 0 then ListView1.Items[0].Selected := True;
  end;
  ReloadList;
end;

procedure TFormPlaylist.AItemPlayExecute(Sender: TObject);
begin
  FormMain.Playlist.CurrentIndex := ListView1.Selected.Index;
  FormMain.Playlist.Play;
end;

procedure TFormPlaylist.AAddDirectoryExecute(Sender: TObject);
var
  Dir: string;
  sr: TSearchRec;
  Ext: string;
  NewItem: TPlaylistItem;
begin
  if SelectDirectory(SSelectDirectory, '', Dir) then begin
    if FindFirst(Dir + DirectorySeparator + '*.*', $2f, sr) = 0 then begin
      repeat
        Ext := ExtractFileExt(Dir + DirectorySeparator + sr.Name);
        if (Ext = WavFileExt) then begin
          NewItem := TPlaylistItem.Create;
          NewItem.FileName := Dir + DirectorySeparator + sr.Name;
          FormMain.Playlist.Items.Add(NewItem);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    ReloadList;
  end;
end;

procedure TFormPlaylist.AAddFileExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    FormMain.Playlist.AddFile(OpenDialog1.FileName);
    ReloadList;
  end;
end;

procedure TFormPlaylist.FormDestroy(Sender: TObject);
begin
end;

procedure TFormPlaylist.FormShow(Sender: TObject);
begin
  ReloadList;
end;

procedure TFormPlaylist.ListView1Click(Sender: TObject);
begin
  UpdateInterface;
end;

procedure TFormPlaylist.ListView1Data(Sender: TObject; Item: TListItem);
begin
  if (Item.Index >= 0) and (Item.Index < FormMain.Playlist.Items.Count) then
  with TPlaylistItem(FormMain.Playlist.Items[Item.Index]) do begin
    Item.Caption := FileName;
    Item.Data := FormMain.Playlist.Items[Item.Index];
  end;
end;

procedure TFormPlaylist.ListView1DblClick(Sender: TObject);
begin
  AItemPlay.Execute;
end;

procedure TFormPlaylist.ListView1KeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then AItemPlay.Execute;
end;

procedure TFormPlaylist.ReloadList;
begin
  ListView1.Items.Count := FormMain.Playlist.Items.Count;
  ListView1.Refresh;
  ListView1.Selected := nil;
  if (FormMain.Playlist.CurrentIndex >= 0) and
    (FormMain.Playlist.CurrentIndex < ListView1.Items.Count) then
    ListView1.Items[FormMain.Playlist.CurrentIndex].Selected := True;
  UpdateInterface;
end;

procedure TFormPlaylist.UpdateInterface;
begin
  AItemDelete.Enabled := Assigned(ListView1.Selected);
  AItemPlay.Enabled := Assigned(ListView1.Selected);
  AClearAll.Enabled := ListView1.Items.Count > 0;
  CheckBoxRandom.Checked := FormMain.Playlist.RandomOrder;
  CheckBoxRepeat.Checked := FormMain.Playlist.RepeatInfinitely;
end;

end.

