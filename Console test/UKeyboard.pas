unit UKeyboard;

interface

uses
  Contnrs, SysUtils, Forms, Classes, Controls;

type
  TShowKeyEvent = procedure(Key: Char) of object;

  TKeyboard = class
  private
    FOnKeyPressedEvent: TNotifyEvent;
    FOnShowKey: TShowKeyEvent;
  published
    Queue: TQueue;
    constructor Create;
    function KeyPressed: Boolean;
    function ReadKey: Char;
    function ReadLn: string;
    destructor Destroy; override;
    procedure PressKey(Key: Char);
    property OnKeyPressed: TNotifyEvent read FOnKeyPressedEvent write FOnKeyPressedEvent;
    property OnShowKey: TShowKeyEvent read FOnShowKey write FOnShowKey;
  end;

implementation

{ TKeyboard }

constructor TKeyboard.Create;
begin
  Queue := TQueue.Create;
end;

destructor TKeyboard.Destroy;
begin
  Queue.Free;
  inherited;
end;

function TKeyboard.KeyPressed: Boolean;
begin
  Result := Queue.Count > 0;
end;

procedure TKeyboard.PressKey(Key: Char);
begin
  Queue.Push(Pointer(Ord(Key)));
  if Assigned(FOnKeyPressedEvent) then
    FOnKeyPressedEvent(Self);
end;

function TKeyboard.ReadKey: Char;
begin
  while Queue.Count = 0 do begin
    Sleep(10);
    Application.ProcessMessages;
  end;
  Result := Chr(Integer(Queue.Pop));
end;

function TKeyboard.ReadLn: string;
var
  Key: Char;
begin
  Result := '';
  repeat
    Key := ReadKey;
    if Key = #8 then begin
      if Length(Result) > 0 then SetLength(Result, Length(Result) - 1)
        else Key := #0;
    end else
    if Key = #13 then 
    else Result := Result + Key;
    if Assigned(FOnShowKey) then
      FOnShowKey(Key);
  until Key = #13;
  if Assigned(FOnShowKey) then
    FOnShowKey(#10);
end;

end.
