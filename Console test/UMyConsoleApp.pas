unit UMyConsoleApp;

interface

uses
  SysUtils, Forms, UConsoleApp, UObjectType, UConsole, UFileSystem, UClasses,
  UTextScreen, Graphics;

type
  TMyConsoleApp = class(TConsoleApp)
    procedure Execute; override;
  end;

implementation

{ TMyConsoleApp }

procedure TMyConsoleApp.Execute;
var
  I: Integer;
  Text: string;
begin
  Randomize;
  with Screen, Keyboard do begin
    WriteLn('Zavádìní systému ChronOS...');
    TextColor := clSilver;
    ClearEol;
    Sleep(500);
    for I := 0 to 10 do begin
      WriteLn(' Služba ' + IntToStr(I) + ' naètena');
      Sleep(Random(300));
    end;
    repeat
      WriteLn;
      Write('[uživatel@server /]: ');
      Text := ReadLn;
      WriteLn;
      if Text = 'verze' then WriteLn(' Verze: 0.0.1')
      else if Text = 'nápovìda' then begin
        WriteLn('Seznam pøíkazù');
        WriteLn(' nápovìda - zobrazí tento výpis');
        WriteLn(' verze - zobrazí verzi systému');
        WriteLn(' konec - ukonèí systém');
      end else if Text = 'konec' then WriteLn('Systém ukonèen')
      else WriteLn('Neznámý pøíkaz');
    until Text = 'konec';
  end;
end;

end.
