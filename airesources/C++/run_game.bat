SET PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%
CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64

cl.exe /std:c++14 /O2 /MT /EHsc /D_USE_MATH_DEFINES .\hlt\hlt_in.cpp .\hlt\location.cpp .\hlt\map.cpp .\MyBot.cpp /link /out:MyBot.exe

.\halite.exe -d "240 160" ".\MyBot.exe" ".\MyBot.exe"
