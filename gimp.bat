@echo off
echo %1
for /f "usebackq tokens=*" %%i in (`wsl wslpath "%~1"`) do (
    start wsl -d Debian gimp "%%i"
)
