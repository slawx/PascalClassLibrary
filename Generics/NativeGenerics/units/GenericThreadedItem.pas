unit GenericThreadedItem;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

  { TGThreadedItem }

  generic TGThreadedItem<TItemType> = class
  private
    FItem: TItemType;
    FLock: TCriticalSection;
  public
    constructor Create;
    procedure Lock;
    procedure UnLock;
    destructor Destroy; override;
    property Item: TItemType read FItem write FItem;
  end;

implementation

{ TGThreadedItem }

constructor TGThreadedItem.Create;
begin
  FLock := TCriticalSection.Create;
end;

procedure TGThreadedItem.Lock;
begin
  FLock.Acquire;
end;

procedure TGThreadedItem.UnLock;
begin
  FLock.Release;
end;

destructor TGThreadedItem.Destroy;
begin
  FLock.Destroy;
  inherited Destroy;
end;

end.

