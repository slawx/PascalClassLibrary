unit UPlaylist;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TPlaylistItem = class

  end;

  { TPlaylist }

  TPlaylist = class(TComponent)
    Items: TObjectList; // TObjectList<TPlaylistItem>
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;


implementation

{ TPlaylist }

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

