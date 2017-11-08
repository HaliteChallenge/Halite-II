mkdir obj

SET PATH=C:\Program Files (x86)\MSBuild\14.0\Bin;%PATH%

CALL "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\vcvarsall.bat" amd64

cl.exe /std:c++14 /O2 /MT /EHsc /W2 /Fo.\obj\ /Fehalite.exe ^
 ^
 /DZSTD_MULTITHREAD=1 ^
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
