javac *.java
scalac HaliteBot.scala
scalac MyBot.scala
scalac RandomBot.scala

\halite.exe -d "30 30" "scala MyBot" "scala RandomBot"
