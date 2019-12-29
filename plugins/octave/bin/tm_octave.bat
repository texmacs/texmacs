@echo off
rem denis RAUX LIX 2013
rem a simple trick to
set /p DUMMY="verbatim:" <nul
rem octave -v
rem cd $TEXMACS_PATH/plugins/octave/octave; exec octave -qi tm-start.oct
cd %TEXMACS_PATH%\plugins\octave\octave
octave -qi tm-start.m
