@echo off

rem build 64-bit
call "%~p0%build.generic.cmd" VS2017 x64 Release v141
