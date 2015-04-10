unit UPlaylist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, UAudioSystem;

type
  TPlaylistItem = class
    FileName: string;
  end;

  { TPlayList }

  TPlayList = class(TComponent)
  private
    FRandomOrder: Boolean;
    FRepeatInfinitely: Boolean;
  public
    CurrentIndex: Integer;
    procedure AddFile(FileName: string);
    procedure Shuffle;
    procedure Play;
    procedure PlayNext;
    procedure PlayPrevious;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    Player: TMediaPlayer;
    Items: TObjectList; // TObjectList<TPlaylistItem>
    property RandomOrder: Boolean read FRandomOrder write FRandomOrder;
    property RepeatInfinitely: Boolean read FRepeatInfinitely
      write FRepeatInfinitely;
  end;


implementation

{ TPlayList }

procedure TPlayList.AddFile(FileName: string);
var
  NewItem: TPlaylistItem;
begin
  NewItem := TPlaylistItem.Create;
  NewItem.FileName := FileName;
  Items.Add(NewItem);
end;

procedure TPlayList.Shuffle;
begin

end;

procedure TPlayList.Play;
begin
  Player.FileName := TPlaylistItem(Items[CurrentIndex]).FileName;
  Player.Play;
end;

procedure TPlayList.PlayNext;
begin
  Inc(CurrentIndex);
  if CurrentIndex >= Items.Count then begin
    CurrentIndex := 0;
    if RandomOrder then Shuffle;
    if not RepeatInfinitely then Player.Stop;
  end;
  Play;
end;

procedure TPlayList.PlayPrevious;
begin
  Dec(CurrentIndex);
  if CurrentIndex < 0 then begin
    CurrentIndex := Items.Count - 1;
  end;
  Play;
end;

constructor TPlayList.Create(AOwner: TComponent);
begin
  inherited;
  Items := TObjectList.Create;
end;

destructor TPlayList.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

end.

