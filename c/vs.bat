@echo off
set Applications=%ProgramFiles(x86)%
if not "%Applications%" == "" goto win64
set Applications=%ProgramFiles%
:win64

:: Set up Visual Studio command line environment variables given a
:: machine type, e.g., amd64 or x86.

:: Visual Studio 2022 Enterprise
set BATDIR=%ProgramW6432%\Microsoft Visual Studio\2022\Enterprise\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2022 Professional
set BATDIR=%ProgramW6432%\Microsoft Visual Studio\2022\Professional\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2022 Community
set BATDIR=%ProgramW6432%\Microsoft Visual Studio\2022\Community\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2019 Enterprise
set BATDIR=%Applications%\Microsoft Visual Studio\2019\Enterprise\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2019 Professional
set BATDIR=%Applications%\Microsoft Visual Studio\2019\Professional\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2019 Community
set BATDIR=%Applications%\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2019 BuildTools
set BATDIR=%Applications%\Microsoft Visual Studio\2019\BuildTools\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2017 Enterprise
set BATDIR=%Applications%\Microsoft Visual Studio\2017\Enterprise\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2017 Professional
set BATDIR=%Applications%\Microsoft Visual Studio\2017\Professional\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2017 Community
set BATDIR=%Applications%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2017 BuildTools
set BATDIR=%Applications%\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build
if exist "%BATDIR%\vcvarsall.bat" goto found

:: Visual Studio 2015
set BATDIR=%VS140COMNTOOLS%..\..\VC
if exist "%BATDIR%\vcvarsall.bat" goto found

echo Visual Studio 2022, 2019, 2017, or 2015 must be installed.
exit 1

:found

:: Clear environment variables that we might otherwise inherit
set INCLUDE=
set LIB=
set LIBPATH=

:: Visual Studio 2017's vcvarsall.bat changes the directory to %USERPROFILE%\Source if the directory exists. See https://developercommunity.visualstudio.com/content/problem/26780/vsdevcmdbat-changes-the-current-working-directory.html
set VSCMD_START_DIR=%CD%
"%BATDIR%\vcvarsall.bat" %1
