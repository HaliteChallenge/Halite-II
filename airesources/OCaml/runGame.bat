ocamlbuild -lib unix MyBot.native
ocamlbuild -lib unix RandomBot.native
.\halite.exe -d "30 30" ".\MyBot.exe" ".\RandomBot.exe"



