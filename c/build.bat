@echo off
setlocal

set M=%1
set WORKAREA=%M%

if "%WORKAREA%"=="" goto needargument

xcopy /s /i /y c %WORKAREA%\c
xcopy /s /i /y s %WORKAREA%\s
xcopy /s /i /y boot %WORKAREA%\boot
xcopy /s /i /y zlib %WORKAREA%\zlib
xcopy /s /i /y lz4 %WORKAREA%\lz4

mkdir %WORKAREA%\bin\%M%

echo #define SCHEME_SCRIPT "scheme-script" > %WORKAREA%\c\config.h

cd %WORKAREA%\c
nmake Makefile.%M%
cd ..\..

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
