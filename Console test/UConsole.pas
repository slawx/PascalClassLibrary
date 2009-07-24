unit UConsole;

interface

uses
  Types, Graphics, SysUtils;

type
  TConsole = class
    procedure ClearScreen;
    procedure ClearEol;
    constructor Create;
    destructor Destroy; override;
    procedure GotoXY(X, Y: Integer);
    function Read: string;
    function ReadLn: string;
    function ReadKey: Char;
    function KeyPressed: Boolean;
    function WhereX: Integer;
    function WhereY: Integer;
    procedure Write(Text: string);
    procedure WriteLn(Text: string);
  end;

implementation

{ TConsole }

procedure TConsole.ClearEol;
begin

end;

procedure TConsole.ClearScreen;
begin

end;

constructor TConsole.Create;
begin
end;

destructor TConsole.Destroy;
begin
  inherited;
end;

procedure TConsole.GotoXY(X, Y: Integer);
begin
end;

function TConsole.KeyPressed: Boolean;
begin

end;

function TConsole.Read: string;
begin
  //Result := System.Read;
end;

function TConsole.ReadKey: Char;
begin

end;

function TConsole.ReadLn: string;
begin
  //Result := System.Read;
end;

function TConsole.WhereX: Integer;
begin
end;

function TConsole.WhereY: Integer;
begin
end;

procedure TConsole.Write(Text: string);
begin
  System.Write(Text);
end;

procedure TConsole.WriteLn(Text: string);
begin
  Write(Text + #13#10);
end;

end.
