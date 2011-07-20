unit UWebApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp, SpecializedList, UWebPage;

type
  TRegistredPage = class
    Path: string;
    Page: TWebPage;
  end;

  { TWebApp }

  TWebApp = class(TCustomApplication)
  private
    procedure DoRun; override;
  public
    Pages: TListObject;
    procedure RegisterPage(PageClass: TWebPageClass; out Reference; Path: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


procedure Register;


implementation

procedure Register;
begin
  RegisterClass(TWebApp);
end;

{ TWebApp }

procedure TWebApp.DoRun;
begin
  Terminate;
end;

procedure TWebApp.RegisterPage(PageClass: TWebPageClass; out Reference;
  Path: string);
var
  NewPage: TRegistredPage;
  Instance: TWebPage;
begin
  NewPage := TRegistredPage(Pages.AddNew(TRegistredPage.Create));
//  NewPage.Page := PageClass.Create(Self);
  NewPage.Page := PageClass.Create(nil);
  NewPage.Path := Path;
  TWebPage(Reference) := NewPage.Page;
end;

constructor TWebApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Pages := TListObject.Create;
end;

destructor TWebApp.Destroy;
begin
  Pages.Free;
  inherited Destroy;
end;

end.

