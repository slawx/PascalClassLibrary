unit UApplicationInfo;

// Date: 2010-05-14

{$MODE Delphi}

interface

type
  TApplicationInfo = record
    Name: string;
    Identification: Byte;
    MajorVersion: Byte;
    MinorVersion: Byte;
    CompanyName: string;
    CompanyHomePage: string;
    HomePage: string;
    AuthorName: string;
    EmailContact: string;
    ReleaseDate: string;
  end;

var
  ApplicationInfo: TApplicationInfo;

implementation
                        
initialization

with ApplicationInfo do begin
  Name := 'Application';
  Identification := 1;
  MajorVersion := 1;
  MinorVersion := 0;
  CompanyName := 'Company';
  CompanyHomepage := 'http://www.company.cz/';
  HomePage := 'http://www.company.cz/index.html';
  AuthorName := 'Author';
  EmailContact := 'author@company.cz';
  ReleaseDate := '14.5.2010';
end;

finalization

end.
