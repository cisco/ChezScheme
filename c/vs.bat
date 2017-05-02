@echo off

:: Set up Visual Studio command line environment variables given a
:: sequence of machine types to try ("x86", "x86_amd64", and "amd64").

:: Note: VS 11.0 (2012) and earlier won't work, because they
:: don't support C99 mid-block declarations. Also, there's no
:: such thing as VS 13.0.

:: Clear environment variables that we might otherwise inherit
set INCLUDE=
set LIB=
set LIBPATH=

:: Find visual studio
set VCDIR=%VS140COMNTOOLS%\..\..\vc
if not exist "%VCDIR%\vcvarsall.bat" set VCDIR=%VS120COMNTOOLS%\..\..\vc

:: Loop to find a requested machine type
if exist "%VCDIR%\vcvarsall.bat" goto :VCDIR
echo Could not find Visual Studio installed.
exit 1

:VCDIR

set MACHINETYPE=%1
if "%MACHINETYPE%" neq "" goto :MACHINE
echo Could not find working machine type.
exit 1

:MACHINE
if "%MACHINETYPE%" == "x86" goto :VS
if exist "%VCDIR%\bin\%MACHINETYPE%\vcvars%MACHINETYPE%.bat" goto :VS
shift
goto :VCDIR

:: Set environment variables
:VS
"%VCDIR%\vcvarsall.bat" %MACHINETYPE%
