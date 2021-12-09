@call "../c/vs.bat" x86

@IF "%VisualStudioVersion%"=="" ( GOTO error-undefined-vs )

@IF "%VisualStudioVersion%"=="14.0" (
  SET "Path32=%CommonProgramFiles(x86)%\Merge Modules\Microsoft_VC140_CRT_x86.msm"
  SET "Path64=%CommonProgramFiles(x86)%\Merge Modules\Microsoft_VC140_CRT_x64.msm"
)

@IF "%VisualStudioVersion%"=="15.0" or "%VisualStudioVersion%"=="16.0" or "%VisualStudioVersion%"=="17.0" (
  @PUSHD "%VCINSTALLDIR%Redist\MSVC"
  @FOR /D %%D IN (*) DO (
    @PUSHD %%D
    @FOR %%F IN (MergeModules\Microsoft_VC*_CRT_x86.msm) DO (
      SET "Path32=%VCINSTALLDIR%Redist\MSVC\%%D\%%F"
    )
    @FOR %%F IN (MergeModules\Microsoft_VC*_CRT_x64.msm) DO (
      SET "Path64=%VCINSTALLDIR%Redist\MSVC\%%D\%%F"
    )
    @POPD
  )
  @POPD
)

@DEL vcredist.wxs >nul 2>&1

IF "%Path32%"=="" ( GOTO error-unknown-vs )
IF NOT EXIST "%Path32%" ( GOTO error-32-doesnt-exist )
IF NOT EXIST "%Path64%" ( GOTO error-64-doesnt-exist )

@(
  @ECHO ^<?xml version="1.0" encoding="utf-8"?^>
  @ECHO ^<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi"^>
  @ECHO ^<Fragment^>
  @ECHO ^<DirectoryRef Id="TARGETDIR"^>
  @ECHO ^<Merge Id="VCRedist64Merge" SourceFile="%Path64%" Language="0" DiskId="1" /^>
  @ECHO ^</DirectoryRef^>
  @ECHO ^</Fragment^>
  @ECHO ^<Fragment^>
  @ECHO ^<DirectoryRef Id="TARGETDIR"^>
  @ECHO ^<Merge Id="VCRedist32Merge" SourceFile="%Path32%" Language="0" DiskId="1" /^>
  @ECHO ^</DirectoryRef^>
  @ECHO ^</Fragment^>
  @ECHO ^<Fragment^>
  @ECHO ^<Feature Id="VCRedist64" Title="Visual C++ Runtime (64-bit)" AllowAdvertise="no" Display="hidden" Level="1"^>
  @ECHO ^<MergeRef Id="VCRedist64Merge" /^>
  @ECHO ^</Feature^>
  @ECHO ^</Fragment^>
  @ECHO ^<Fragment^>
  @ECHO ^<Feature Id="VCRedist32" Title="Visual C++ Runtime (32-bit)" AllowAdvertise="no" Display="hidden" Level="1"^>
  @ECHO ^<MergeRef Id="VCRedist32Merge" /^>
  @ECHO ^</Feature^>
  @ECHO ^</Fragment^>
  @ECHO ^</Wix^>
) > vcredist.wxs

@ECHO Built vcredist.wxs
@EXIT /b 0

:error-undefined-vs
@ECHO Error building vcredist.wxs: Visual Studio version not defined.
@EXIT /b 1

:error-unknown-vs
@ECHO Error building vcredist.wxs: Unexpected Visual Studio version.
@EXIT /b 1

:error-32-doesnt-exist
@ECHO Error building vcredist.wxs: Merge Module couldn't be found:
@ECHO %Path32%
@EXIT /b 1

:error-64-doesnt-exist
@ECHO Error building vcredist.wxs: Merge Module couldn't be found:
@ECHO %Path64%
@EXIT /b 1
