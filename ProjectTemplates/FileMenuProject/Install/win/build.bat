if not defined LAZDIR (
  set LAZDIR=C:\lazarus
)
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=i386 --operating-system=Win32 ..\..\Application.lpi
%LAZDIR%\lazbuild.exe --lazarusdir=%LAZDIR% --build-mode="Release" --cpu=x86_64 --operating-system=Win64 ..\..\Application.lpi

"c:\Program Files (x86)\Inno Setup 5\ISCC.exe" "Application.iss"
