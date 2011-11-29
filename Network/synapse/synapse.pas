{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit synapse; 

interface

uses
    tlntsend, asn1util, blcksock, clamsend, dnssend, ftpsend, ftptsend, 
  httpsend, imapsend, ldapsend, mimeinln, mimemess, mimepart, nntpsend, 
  pingsend, pop3send, slogsend, smtpsend, snmpsend, sntpsend, synacode, 
  synacrypt, synadbg, synafpc, synachar, synaicnv, synaip, synaser, synautil, 
  synsock, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('synapse', @Register); 
end.
