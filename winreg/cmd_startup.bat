REM It's not recommended to use this script, weird things may happend.
REM e.g. If you do some printing in init.cmd, commands like SDCC compiler, who depends on CMD output, may fail.

reg add "HKCU\Software\Microsoft\Command Processor" /v AutoRun /t REG_EXPAND_SZ /d %USERPROFILE%\init.cmd /f

REM reg delete "HKCU\Software\Microsoft\Command Processor" /v AutoRun

