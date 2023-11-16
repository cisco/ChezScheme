@echo off
setlocal

set M=%1
set WORKAREA=%M%
set LINKAS=dll
set RUNTIMEAS=dll
set SRCDIR=%~dp0
set MAKETARGET=all-dlls
set SKIPVS=no
set SKIPSUBMODULE=no
set USEPB=yes

if "%WORKAREA%"=="" goto needargument

:argloop
shift
set ARG=%1
if defined ARG (
  if "%ARG%"=="/dll" set LINKAS=dll && goto argloop
  if "%ARG%"=="/exe" set LINKAS=exe && goto argloop
  if "%ARG%"=="/MD" set RUNTIMEAS=dll && goto argloop
  if "%ARG%"=="/MT" set RUNTIMEAS=static && goto argloop
  if "%ARG%"=="/only" set MAKETARGET=build && goto argloop
  if "%ARG%"=="/kernel" set MAKETARGET=kernel && goto argloop
  if "%ARG%"=="/none" set MAKETARGET=none && goto argloop
  if "%ARG%"=="/config" set MAKETARGET=none && goto argloop
  if "%ARG%"=="/force" set USEPB=no && goto argloop
  if "%ARG%"=="/test-one" set MAKETARGET=test-one && goto argloop
  if "%ARG%"=="/test-some-fast" set MAKETARGET=test-some-fast && goto argloop
  if "%ARG%"=="/test-some" set MAKETARGET=test-some && goto argloop
  if "%ARG%"=="/test" set MAKETARGET=test && goto argloop
  if "%ARG%"=="/test-more" set MAKETARGET=test-more && goto argloop
  if "%ARG%"=="/keepvs" set SKIPVS=yes && goto argloop
  if "%ARG%"=="/as-is" set SKIPSUBMODULE=yes && goto argloop
  echo Unrecognized argument %ARG%
  exit /B 1
)

if "%M%"=="pb" set VSCONFIG=x86
if "%M%"=="i3nt" set VSCONFIG=x86
if "%M%"=="ti3nt" set VSCONFIG=x86
if "%M%"=="a6nt" set VSCONFIG=x86_amd64
if "%M%"=="ta6nt" set VSCONFIG=x86_amd64
if "%M%"=="arm64nt" set VSCONFIG=x64_arm64
if "%M%"=="tarm64nt" set VSCONFIG=x64_arm64
if "%VSCONFIG%"=="" (
  echo Unrecognized machine type %M%
  exit /B 1
)

if %SKIPSUBMODULE%==yes goto postsubmodule
if not exist .git goto postsubmodule
where git > NUL 2>&1
if %ERRORLEVEL% neq 0 goto postsubmodule
git submodule update --init --depth 1
:postsubmodule

if not exist %WORKAREA% mkdir %WORKAREA%

echo srcdir=%SRCDIR% > %WORKAREA%\Mf-config
echo m=%M% >> %WORKAREA%\Mf-config
echo linkAs=%LINKAS% >> %WORKAREA%\Mf-config
echo runtimeAs=%RUNTIMEAS% >> %WORKAREA%\Mf-config
echo enableFrompb=%USEPB% >> %WORKAREA%\Mf-config

echo workarea=%WORKAREA% > Makefile
echo !include %WORKAREA%\Mf-config >> Makefile
type "%SRCDIR%\makefiles\Makefile.nt" >> Makefile

copy /y "%SRCDIR%\makefiles\buildmain.zuo" main.zuo > NUL
copy /y "%SRCDIR%\makefiles\workmain.zuo" %WORKAREA%\main.zuo > NUL

echo Configured for %M%

if %MAKETARGET%==none goto donebuilding

if %SKIPVS%==yes goto donevs
echo Configuring VS for %VSCONFIG%
call "%SRCDIR%/c/vs.bat" %VSCONFIG%
:donevs

nmake /nologo %MAKETARGET%

goto donebuilding

:needargument

echo Please supply the machine name as an argument
exit /B 1

:donebuilding
