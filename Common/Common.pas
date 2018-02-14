{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Common;

interface

uses
  StopWatch, UCommon, UDebugLog, UDelay, UPrefixMultiplier, UURI, UThreading,
  UMemory, UResetableThread, UPool, ULastOpenedList, URegistry,
  UJobProgressView, UXMLUtils, UApplicationInfo, USyncCounter, UListViewSort,
  UPersistentForm, UFindFile, UScaleDPI, UTheme, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UDebugLog', @UDebugLog.Register);
  RegisterUnit('ULastOpenedList', @ULastOpenedList.Register);
  RegisterUnit('UJobProgressView', @UJobProgressView.Register);
  RegisterUnit('UApplicationInfo', @UApplicationInfo.Register);
  RegisterUnit('UListViewSort', @UListViewSort.Register);
  RegisterUnit('UPersistentForm', @UPersistentForm.Register);
  RegisterUnit('UFindFile', @UFindFile.Register);
  RegisterUnit('UScaleDPI', @UScaleDPI.Register);
  RegisterUnit('UTheme', @UTheme.Register);
end;

initialization
  RegisterPackage('Common', @Register);
end.
