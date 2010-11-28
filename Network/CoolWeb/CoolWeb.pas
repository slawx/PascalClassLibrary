{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit CoolWeb; 

interface

uses
    UHTTPServer, UHTTPServerCGI, UHTTPServerTCP, UHTTPSessionFile, 
  UHTTPSessionMySQL, USqlDatabase, UTCPServer, UPageList, UUser, UCommon, 
  UHtmlClasses, UMemoryStreamEx, UMIMEType, UPool, UResetableThread, 
  UXmlClasses, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('CoolWeb', @Register); 
end.
