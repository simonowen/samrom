@echo off

if "%1"=="clean" goto clean

pyz80.py --obj=samrom.bin --exportfile=samrom.sym -o nul samrom.asm
goto end

:clean
if exist samrom.bin del samrom.bin samrom.sym

:end
