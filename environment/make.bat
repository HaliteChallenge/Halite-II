@echo off
setlocal EnableExtensions EnableDelayedExpansion

if defined VisualStudioVersion (
rem vcvarsall has been called already, don't need to do anything ourselves
) else (
set vcvarsall_location_1="%ProgramFiles(x86)%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat"
set vcvarsall_location_2="%ProgramFiles%\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat"
set vcvarsall_location_3="%ProgramFiles(x86)%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set vcvarsall_location_4="%ProgramFiles%\Microsoft Visual Studio 14.0\VC\vcvarsall.bat"
set vcvarsall_location_count=4

for /L %%i in (!vcvarsall_location_count!, -1, 1) do (
    set vcvarsall_location_candidate=!vcvarsall_location_%%i!
    if exist !vcvarsall_location_candidate! set vcvarsall_location=!vcvarsall_location_candidate!
)

if not defined vcvarsall_location (
    echo Failed to find vcvarsall.bat in any of the known places. You have two options:
    echo 1^) Preferred: run vcvarsall.bat yourself before running this script. Check out https://docs.microsoft.com/en-us/cpp/build/building-on-the-command-line for more information.
    echo 2^) Find where vcvarsall.bat file is on your system and add it to the list of locations in this batch file.
    pause
    exit /b 1
)

reg query "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment" /v PROCESSOR_ARCHITECTURE | find /i "x86" > nul
if !ERRORLEVEL! == 0 (
    set vcvarsall_architecture=x86
) else (
    set vcvarsall_architecture=amd64
)

set VSCMD_START_DIR=%CD%
call !vcvarsall_location! !vcvarsall_architecture!
)

mkdir obj 2> nul

cl.exe /std:c++14 /O2 /MT /EHsc /W2 /Fo.\obj\ /Fehalite.exe ^
 ^
 /DZSTD_MULTITHREAD=1 ^
 /D_LIBCPP_COMPILER_MSVC=1 ^
 /DNDEBUG ^
 ^
 /I . ^
 /I zstd-1.3.0\lib ^
 /I zstd-1.3.0\lib\common ^
 ^
 .\core\mapgen\AsteroidCluster.cpp ^
 .\core\mapgen\Generator.cpp ^
 .\core\mapgen\SolarSystem.cpp ^
 .\core\Constants.cpp ^
 .\core\Entity.cpp ^
 .\core\GameEvent.cpp ^
 .\core\Halite.cpp ^
 .\core\hlt.cpp ^
 .\core\Replay.cpp ^
 .\core\SimulationEvent.cpp ^
 .\core\Statistics.cpp ^
 .\networking\BotInputError.cpp ^
 .\networking\Networking.cpp ^
 .\main.cpp ^
 ^
 zstd-1.3.0\lib\common\entropy_common.c ^
 zstd-1.3.0\lib\common\error_private.c ^
 zstd-1.3.0\lib\common\fse_decompress.c ^
 zstd-1.3.0\lib\common\pool.c ^
 zstd-1.3.0\lib\common\threading.c ^
 zstd-1.3.0\lib\common\xxhash.c ^
 zstd-1.3.0\lib\common\zstd_common.c ^
 zstd-1.3.0\lib\compress\fse_compress.c ^
 zstd-1.3.0\lib\compress\huf_compress.c ^
 zstd-1.3.0\lib\compress\zstd_compress.c ^
 zstd-1.3.0\lib\compress\zstdmt_compress.c ^
