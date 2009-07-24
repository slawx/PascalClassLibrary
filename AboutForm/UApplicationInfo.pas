unit UApplicationInfo;

interface

type
  TApplicationInfo = record
    Name: string;
    Identification: Byte;
    MajorVersion: Byte;
    MinorVersion: Byte;
    CompanyName: string;
    CompanyHomepage: string;
    Homepage: string;
    AuthorName: string;
    EmailContact: string;
    ReleaseDate: string;
  end;

var
  ApplicationInfo: TApplicationInfo;

implementation
                        
initialization

with ApplicationInfo do begin
  Name := 'Jméno programu';
  Identification := 1;
  MajorVersion := 1;
  MinorVersion := 0;
  CompanyName := 'Mezservis spol. s r.o.';
  CompanyHomePage := 'http://www.mezservis.cz/';
  HomePage := 'http://www.mezservis.cz/cz/vytahova-technika/software.html';
  AuthorName := 'Ing. Jiøí Hajda';
  EmailContact := 'jiri.hajda@mezservis.cz';
  ReleaseDate := '7.4.2009';
end;

finalization

end.
