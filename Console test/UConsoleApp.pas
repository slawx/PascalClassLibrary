unit UConsoleApp;

interface

uses
  UConsole, UFileSystem, UTextScreen, UKeyboard;

type
  TConsoleApp = class
  private
  public
    Console: TConsole;
    Screen: TTextScreen;
    Keyboard: TKeyboard;
    function GetCurrentDirectory: TDirectory;
    procedure Execute; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
    procedure KeyboardShowKey(Key: Char);
  end;

implementation

{ TConsoleApp }

constructor TConsoleApp.Create;
begin
  Console := TConsole.Create;
  Screen := TTextScreen.Create;
  Keyboard := TKeyboard.Create;
  Keyboard.OnShowKey := KeyboardShowKey;
end;

destructor TConsoleApp.Destroy;
begin
  Keyboard.Free;
  Screen.Free;
  Console.Destroy;
  inherited;
end;

function TConsoleApp.GetCurrentDirectory: TDirectory;
begin

end;

procedure TConsoleApp.KeyboardShowKey(Key: Char);
begin
  Screen.Write(Key);
end;

end.
