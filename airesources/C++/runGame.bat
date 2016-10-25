g++ -std=c++11 MyBot.cpp -o MyBot.exe
g++ -std=c++11 RandomBot.cpp -o RandomBot.exe
.\environment -d 30 30 "./MyBot.exe" "./RandomBot.exe"
