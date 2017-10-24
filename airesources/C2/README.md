# Halite2StarterCSharp
Halite 2 starter project in C#

## Usage
Copy halite.exe into Halite2 directory and run the batch file to start.

This is just the java project blindly converted to C#.

## Using the Visual Studio Debugger
If you would like to use the powerful, built in VS debugger to debug your code then this is one of the many solutions but it's the one I prefer and use most.

1. Add a reference `using System.Diagnostics;` in MyBot.cs
2. At the beginning of the main method in MyBot.cs, add the following line: `while(!Debugger.IsAttached);`
3. At the end of the first line of the batch file which builds your bot, add `/debug:full` so that it builds with the debug constant defined.
4. Add a `-t` flag at the end of the execution of the simulation in runGame.bat to disable the timeout.
5. Run the simulation. At this point the simulation should freeze on the initialisation of the bot.
6. Ensure that you have set a break point somewhere in your code.
7. In Visual Studio, in the toolbar click on `Debug>Attach to Process` and select MyBot.exe from the list
8. Debug to your heart's desire.
