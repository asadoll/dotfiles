@ECHO off
:top
CLS
ECHO Choose a shell:
ECHO [1] cmd
ECHO [2] powershell
ECHO [3] ubuntu
ECHO [4] kali
ECHO [5] mingw64
ECHO [6] bash
ECHO.
ECHO [7] restart elevated
ECHO [8] exit
ECHO.

CHOICE /N /C:12345678 /M "> "
CLS
IF ERRORLEVEL ==8 GOTO end
IF ERRORLEVEL ==7 powershell -Command "Start-Process hyper -Verb RunAs"
IF ERRORLEVEL ==6 bash
IF ERRORLEVEL ==5 C:\Users\aghar\scoop\apps\msys2\current\usr\bin\bash.exe
IF ERRORLEVEL ==4 kali
IF ERRORLEVEL ==3 ubuntu
IF ERRORLEVEL ==2 powershell
IF ERRORLEVEL ==1 cmd

CLS
ECHO Switch or exit?
ECHO [1] Switch
ECHO [2] Exit

CHOICE /N /C:12 /D 2 /T 5 /M "> "
IF ERRORLEVEL ==2 GOTO end
IF ERRORLEVEL ==1 GOTO top

:end