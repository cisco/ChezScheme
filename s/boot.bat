@echo off
setlocal

set M=%1
set H=%2

set WORKAREA=%M%

if "%WORKAREA%"=="" goto needargument

if "%H%"=="" H=pb

xcopy /s /i /d /y s xc-%WORKAREA%\s
xcopy /s /i /d /y nanopass xc-%WORKAREA%\nanopass
xcopy /s /i /d /y unicode xc-%WORKAREA%\unicode

cd xc-%WORKAREA%\s
..\..\%H%\bin\%H%\scheme.exe --script make-xpatch.ss %M% macro
..\..\%H%\bin\%H%\scheme.exe --script make-xpatch.ss %M% build
cd ..\..

xcopy /s /i /d /y xc-%WORKAREA%\boot\%M% boot\%M%

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
