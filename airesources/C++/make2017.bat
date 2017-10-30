mkdir obj

SET PATH=C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\MSBuild\15.0\Bin;%PATH%

SET "VSCMD_START_DIR=%CD%"
CALL "C:\Program Files (x86)\Microsoft Visual Studio\2017\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64

cl.exe /std:c++14 /O2 /MT /EHsc /I . /Fo.\obj\ ^
 /D_USE_MATH_DEFINES ^
 .\hlt\hlt_in.cpp .\hlt\location.cpp .\hlt\map.cpp ^
 .\MyBot.cpp ^
 /link /out:MyBot.exe
