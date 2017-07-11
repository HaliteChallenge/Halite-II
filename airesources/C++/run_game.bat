SET PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%
CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64
cl.exe /std:c++14 /O2 /MT /EHsc .\MyBot.cpp /link /out:MyBot.exe

..\..\environment\halite.exe -d "96 96" ".\MyBot.exe" ".\MyBot.exe"
