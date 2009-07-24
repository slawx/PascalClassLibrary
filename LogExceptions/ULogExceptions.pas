// You have to turn on generating of detailed map file!
// Release date 24.2.2006

unit ULogExceptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Sockets, ExtCtrls, Registry, UMapFile, WinSock, MemCheck;

const
  ErrorLogHost = 'www.zdechov.net';
  ErrorLogScript = 'jirihajda/error.php';
  NazevSouboru = 'Error.txt';

type
  TStoreError = class(TThread)
     Bezi: Boolean;
     procedure Execute; override;
  end;

  TRadekInfoVyjimky = record
    Popis: string;
    Text: string;
  end;

  TLogExceptions = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Label2: TLabel;
    Button2: TButton;
    Memo1: TMemo;
    Image1: TImage;
    CheckBox1: TCheckBox;
    TcpClient1: TTcpClient;
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  private
    FMapFile: TMapFile;
//    Report: string;
    StoreError: TStoreError;
    Data: array of TRadekInfoVyjimky;
    Vyska: Integer;
    procedure Send;
  public
    Stav: string;            // Obsah lokálních promìnných
    NazevAplikace: string;   // Název aplikace
    AppReleaseDate: string;
    AppVersion: string;
    procedure Obsluha(Sender : TObject; E : Exception );
    procedure Pridat(NovyPopis, NovyText: string);
  end;

var
  LogExceptions: TLogExceptions;

implementation

{$R *.dfm}

{ TOsetreniVyjimek }

function ReturnAddr(Index: Integer): Pointer;
asm
  MOV     EAX,[EBP + Index * 4]
end;

procedure TLogExceptions.Obsluha(Sender: TObject; E: Exception);
var
//  ExtRec: PExceptionRecord;
//  Log: string;
  Soubor: TextFile;
  I: Integer;
  Stack: TCallStack;
begin
  FMapFile.LoadExceptionData;
//  ShowMessage();
  Pridat('Èas', DateTimeToStr(Now));
  Pridat('Aplikace', NazevAplikace);
  Pridat('Verze', AppVersion);
  Pridat('Datum vydání', AppReleaseDate);
  Pridat('Text chyby', E.Message);
  Pridat('Tøída vyjímky', E.ClassName);
  Pridat('Adresa', Format('%p', [ReturnAddr(6)]));
  Pridat('Jednotka', FMapFile.ExceptUnitName);
  Pridat('Metoda', FMapFile.ExceptMethodName);
  Pridat('Øádek', IntToStr(FMapFile.ExceptLineNumber));
  Pridat('Stavové informace', Stav);
  Pridat('Zásobník volání', TextualDebugInfoForAddress(Cardinal(ReturnAddr(6))));

  FillCallStack(Stack, 0);
  Pridat('Callstack', CallStackTextualRepresentation(Stack, ''));
  for I := 0 to Length(Stack) - 1 do begin
    FMapFile.LoadExceptionData(Stack[I]);
    Pridat('CallStackAddress', Format('%p', [Stack[I]]));
    Pridat('Adresa', Format('%p', [ReturnAddr(6)]));
    Pridat('Jednotka', FMapFile.ExceptUnitName);
    Pridat('Metoda', FMapFile.ExceptMethodName);
    Pridat('Øádek', IntToStr(FMapFile.ExceptLineNumber));
  end;

  ShowModal;    // Ukaž dialog

  if Edit1.Text <> '' then Pridat('Komentáø uživatele', Edit1.Text);

  // Uložit chybu do souboru
  AssignFile(Soubor, NazevSouboru);
  try
    if FileExists(NazevSouboru) then Append(Soubor) else Rewrite(Soubor);
    WriteLn(Soubor);
    for I := 0 to High(Data) do WriteLn(Soubor, Data[I].Popis + ': ' + Data[I].Text);
  finally
    CloseFile(Soubor);
  end;
  StoreError.Execute;
end;

procedure TLogExceptions.Pridat(NovyPopis, NovyText: string);
begin
  SetLength(Data, Length(Data) + 1);
  with Data[High(Data)] do begin
    Popis := NovyPopis;
    Text := NovyText;
  end;
end;

procedure TLogExceptions.Send;
var
  I: Integer;
  //II: Integer;
  Hlaseni: string;
  //Buf: array[0..10000] of Char;
  //Pocet: Integer;
//  Radek: string;
  UseProxy: Boolean;
  ProxyServer: string;
  ProxyPort: string;
  HostHTTP: string;
begin
  // Check if proxy server enabled
  with TRegistry.Create do try
    RootKey := HKEY_CURRENT_USER;
    OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Internet Settings', False);
    UseProxy := ReadBool('ProxyEnable');
    ProxyServer := ReadString('ProxyServer');
  finally
    Free;
  end;

  // Generate report
  Hlaseni := '';
  for I := 0 to High(Data) do with Data[I] do
    Hlaseni := Hlaseni + '&' + Popis + '=' + Text;
  Delete(Hlaseni, 1, 1);

  // Replace blank spaces by special char %20
  while Pos(' ', Hlaseni) > 0 do
    Hlaseni := Copy(Hlaseni, 1, Pos(' ', Hlaseni)-1) + '%20' + Copy(Hlaseni, Pos(' ', Hlaseni) + 1, Length(Hlaseni));

  if UseProxy then begin
    ProxyPort := Copy(ProxyServer,Pos(':', ProxyServer) + 1, 255);
    Delete(ProxyServer,Pos(':', ProxyServer), 255);
    TcpClient1.RemoteHost := ProxyServer;
    TcpClient1.RemotePort := ProxyPort;
  end else begin
    TcpClient1.RemoteHost := ErrorLogHost;
    TcpClient1.RemotePort := '80';
  end;

  HostHTTP := 'http://' + ErrorLogHost + '/' + ErrorLogScript;
  with TcpClient1 do begin
    Connect;
    if Connected then begin
      SendLn('GET ' + HostHTTP + '?' + Hlaseni + ' HTTP/1.0');
      SendLn('Accept: text/html');
      SendLn('Content-Type: text/plain; charset="iso-8859-2"');
      Hlaseni := '';
      SendLn('Content-length: '+IntToStr(Length(Hlaseni)));
      if UseProxy then SendLn('Host: ' + ErrorLogHost);
      SendLn('');
      SendLn(Hlaseni);
//      Memo1.Lines.Add(Hlaseni);

      //for I := 0 to 100 do
      //  Memo1.Lines.Add(Receiveln);
      Close;
    end; // else ShowMessage('Nepodaøilo se pøipojit k serveru');
  end;
  SetLength(Data,0);
end;

procedure TLogExceptions.Button2Click(Sender: TObject);
begin
  if Height = Vyska then Height := Vyska + Memo1.Height + 8 else Height := Vyska;
end;

procedure TLogExceptions.FormShow(Sender: TObject);
var
  I: Integer;
begin
  Vyska := Height;
  Memo1.Clear;
  for I := 0 to High(Data) do with Data[I] do
    Memo1.Lines.Add(Popis + ': ' + Text);
  Edit1.Text := '';
  //Send;
end;

procedure TLogExceptions.Button1Click(Sender: TObject);
begin
  if CheckBox1.Checked then Application.Terminate;
  Close;
end;

{ TOdeslani }

procedure TStoreError.Execute;
begin
  Bezi := True;
  LogExceptions.Send;
  Bezi := False;
end;

procedure TLogExceptions.FormCreate(Sender: TObject);
begin
  (*
  StoreError := TStoreError.Create(True);
  NazevAplikace := Application.Title;
  Application.OnException := Obsluha;
  FMapFile := TMapFile.Create;
  FMapFile.MapFileName := ChangeFileExt(Application.ExeName, '.map');
  *)
end;

procedure TLogExceptions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Height := Vyska;
end;

procedure TLogExceptions.FormDestroy(Sender: TObject);
begin
  FreeAndNil(StoreError);
  FreeAndNil(FMapFile);
end;

end.
