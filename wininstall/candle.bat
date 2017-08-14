@echo off
if exist "%WIX%bin\candle.exe" goto found

echo WiX must be installed.
exit 1

:found
"%WIX%bin\candle.exe" %*
