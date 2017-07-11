SET PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%
CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64
cl.exe /std:c++14 /O2 /MT /EHsc .\main.cpp .\core\Halite.cpp /I . .\networking\Networking.cpp .\core\hlt.cpp .\core\mapgen\Generator.cpp .\core\mapgen\SolarSystem.cpp .\miniz\miniz.c /link /out:halite.exe
