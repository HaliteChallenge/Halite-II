csc /t:library /out:HaliteHelper.dll HaliteHelper.cs
csc /reference:HaliteHelper.dll -out:MyBot.exe MyBot.cs
csc /reference:HaliteHelper.dll -out:RandomBot.exe RandomBot.cs 
halite -d "30 30" "MyBot.exe" "RandomBot.exe"
