SET PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%
CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64
cl.exe /std:c++14 /O2 /MT /EHsc /I . ^
 .\main.cpp ^
 .\core\Constants.cpp .\core\Entity.cpp .\core\GameEvent.cpp .\core\Halite.cpp .\core\hlt.cpp .\core\SimulationEvent.cpp ^
 .\core\Replay.cpp .\core\Statistics.cpp ^
 .\networking\Networking.cpp .\networking\BotInputError.cpp ^
 .\core\mapgen\Generator.cpp .\core\mapgen\SolarSystem.cpp .\core\mapgen\AsteroidCluster.cpp ^
 .\miniz\miniz.c /link /out:halite.exe
