unit UPlaylist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TPlaylistItem = class

  end;

  { TPlaylist }

  TPlaylist = class
    Items: TObjectList; // TObjectList<TPlaylistItem>
    constructor Create;
    destructor Destroy; override;
  end;


implementation

{ TPlaylist }

constructor TPlaylist.Create;
begin
  Items := TObjectList.Create;
end;

destructor TPlaylist.Destroy;
begin
  Items.Free;
  inherited Destroy;
end;

end.

