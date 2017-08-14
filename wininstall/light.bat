@echo off
if exist "%WIX%bin\light.exe" goto found

echo WiX must be installed.
exit 1

:found
"%WIX%bin\light.exe" %*
