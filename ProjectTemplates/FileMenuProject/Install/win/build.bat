if not defined LAZDIR (
  set LAZDIR=C:\lazarus
)
SET APPNAME=FileMenuProject
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=i386 --operating-system=Win32 ..\..\%APPNAME%.lpi
copy ..\..\%APPNAME%.exe ..\..\lib\i386-win32-Release
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=x86_64 --operating-system=Win64 ..\..\%APPNAME%.lpi
copy ..\..\%APPNAME%.exe ..\..\lib\x86_64-win64-Release

"c:\Program Files (x86)\Inno Setup 5\ISCC.exe" "%APPNAME%.iss"

