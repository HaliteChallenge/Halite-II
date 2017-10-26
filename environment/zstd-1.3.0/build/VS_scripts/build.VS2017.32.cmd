@echo off

rem build 32-bit
call "%~p0%build.generic.cmd" VS2017 Win32 Release v141
