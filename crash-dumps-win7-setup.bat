if not exist %windir%\sysnative\cmd.exe goto ready
%windir%\sysnative\cmd.exe /c %0
exit /b 0

:ready
reg query "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps"

reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps" /f /v DumpFolder /t REG_EXPAND_SZ /d %userprofile%

reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps" /f /v DumpCount /t REG_DWORD /d 10

reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows\Windows Error Reporting\LocalDumps" /f /v DumpType /t REG_DWORD /d 1

