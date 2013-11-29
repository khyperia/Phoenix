@echo off
if not exist "bin" (mkdir "bin")
cd src
ghc -outputdir "..\obj" -o "..\bin\main" -Wall -O2 --make Main.hs
if not ERRORLEVEL 0 pause
cd ..\
