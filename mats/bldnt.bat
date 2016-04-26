@echo off
set m=%1
set o=%2
if "%o%"=="" set o=0
echo (time (compile-file "mat")) | ..\bin\%m%\scheme -q
cl /I../boot/%m% /nologo -c -DWIN32 foreign1.c
cl /I../boot/%m% /nologo -c -DWIN32 foreign2.c
cl /I../boot/%m% /nologo -c -DWIN32 foreign3.c
cl /nologo /DWIN32 cat_flush.c
link -dll -out:foreign1.so foreign1.obj foreign2.obj foreign3.obj ..\bin\%m%\csv94.lib
..\bin\%m%\scheme -q mat.so < script.all%o%
