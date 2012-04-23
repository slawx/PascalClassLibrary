unit UPlaylist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, UAudioSystem;

type
  TPlaylistItem = class
    FileName: string;
  end;

  { TPlaylist }

  TPlaylist = class(TComponent)
  public
    Player: TPlayer;
    Items: TObjectList; // TObjectList<TPlaylistItem>
    RandomOrder: Boolean;
    RepeatInfinitely: Boolean;
    CurrentIndex: Integer;
    procedure AddFile(FileName: string);
    procedure Shuffle;
    procedure Play;
    procedure PlayNext;
    procedure PlayPrevious;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;


implementation

{ TPlaylist }

procedure TPlaylist.AddFile(FileName: string);
var
  NewItem: TPlaylistItem;
begin
  NewItem := TPlaylistItem.Create;
  NewItem.FileName := FileName;
  Items.Add(NewItem);
end;

procedure TPlaylist.Shuffle;
begin

end;

procedure TPlaylist.Play;
begin
  Player.FileName := TPlaylistItem(Items[CurrentIndex]).FileName;
  Player.Play;
end;

procedure TPlaylist.PlayNext;
begin
  Inc(CurrentIndex);
  if CurrentIndex >= Items.Count then begin
    CurrentIndex := 0;
    if RandomOrder then Shuffle;
    if not RepeatInfinitely then Player.Stop;
  end;
  Play;
end;

procedure TPlaylist.PlayPrevious;
begin
  Dec(CurrentIndex);
  if CurrentIndex < 0 then begin
    CurrentIndex := Items.Count - 1;
  end;
  Play;
end;

constructor TPlaylist.Create(AOwner: TComponent);
begin
  inherited;
  Items := TObjectList.Create;
end;

destructor TPlaylist.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

end.

