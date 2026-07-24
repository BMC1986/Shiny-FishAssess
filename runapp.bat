@echo off
setlocal enabledelayedexpansion

:: Search common 64-bit and 32-bit Program Files directories for Rscript.exe
set "R_PATH="
for /d %%i in ("C:\Program Files\R\R-*") do (
    if exist "%%i\bin\Rscript.exe" set "R_PATH=%%i\bin\Rscript.exe"
)

:: Fallback if not found in standard directories
if not defined R_PATH (
    echo Rscript.exe could not be found automatically.
    echo Please add R to your system PATH or specify the path manually.
    pause
    exit /b
)

:: Run the app using the discovered default R installation
echo Found R at: "%R_PATH%"
"%R_PATH%" -e "shiny::runApp(launch.browser=TRUE)"