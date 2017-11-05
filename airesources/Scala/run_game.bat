cd hlt
CALL scalac *.scala
cd ..
CALL scalac -cp hlt *.scala
CALL halite.exe -d "240 160" "scala -cp .;hlt MyBot" "scala -cp .;hlt MyBot"
