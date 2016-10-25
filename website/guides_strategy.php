<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Strategy Considerations</title>

    <link href="lib/bootstrap.min.css" rel="stylesheet">
    <link href="style/general.css" rel="stylesheet">
    <link href="style/learn.css" rel="stylesheet">
</head>
<body>
    <div class="container">
        <?php include 'includes/navbar.php'; ?>
        <div class="row">
            <?php include 'includes/learn_sidebar.php'; ?>
            <div class="col-sm-9">
                <h1>Strategy Considerations</h1>
                <p>In Halite, there are three basic and crucial characteristics of a position (game-state) that will determine how valuable it is. These are:
                <ul>
                    <li>Territory - the number of sites of the map which the player controls.</li>
                    <li>Production - the sum of the productions of the sites of the map which the player controls.</li>
                    <li>Strength - the sum of the strengths of the sites of the map which the player controls.</li>
                </ul>
                Let's go over each of these in a bit more detail.</p>

                <h3>Territory</h3>
                <p>Territory is the measure of how close a player is to winning the game, since the objective of the game is to reduce the territory controlled by other players on the map to zero and a Halite board has a finite number of sites available. The ultimate goal of a player should be focused on maximizing its own territory and minimizing the territory of other players. Simply controlling a lot of territory is not enough to give a player a high degree of certainty of winning, as the other characteristics (production and strength) can be just as influential in determining the outcome of a game, but controlling territory can also provide subtle advantages to a player which will be outlined later.</p>

                <h3>Production</h3>
                <p>Production is the measure of a player's capacity to produce new strength. It tends to determine the long-term outcome of a player's position, as a player with a high total production can often just wear down other players in a war of attrition and then take their territory once their strength is reduced to zero. Players should be aware that clever tactical movement can compensate for a lower production by dealing more damage with pieces than they take themselves.</p>

                <h3>Strength</h3>
                <p>Strength is the measure of a player's capacity to immediately capture new territory. It tends to determine the short-term outcome of a player's position, as a player with a high total strength can immediately attack and take new territory. Players should note that the placement of this strength is relevant; strength in the center of a player's territory cannot be utilized nearly as quickly as strength near the border.</p>

                <h3>Combat Guide</h3>
                <p>The tactical concept of Halite is quite straightforward and entirely relies on a single principle which we will call "Overkill". Because a piece can deal damage to as many as 5 other pieces simultaneously, this means that it can output up to 5 times its own strength in damage. Take, for example, a scenario where just prior to the combat stage of each turn, player 1 has a single piece with a strength of 15. Player 2 has a piece with strength 25 directly to the north of this piece, and a piece with strength 45 directly to the south of it. Following combat resolution, player 1 will lose the piece in question, and player 2 will remain with both pieces alive, the northern piece with 10 strength and the southern piece with 30 strength. However, player 1 will have lost a total of 15 strength, whereas player 2 will have lost 30 strength. Consequently, this exchange actually went much better for player 1! Player 1 was able to reap this benefit because it was able to damage multiple pieces simultaneously, and the excess damage which one would normally expect from combat with multiple pieces had no effect because player 1 did not have any more strength to lose in that piece. That player 1's piece died during the encounter was critical for the tactical advantage, because it dealt more damage via this "Overkill" effect than it could possibly take.</p>
                <p>Of course, a difference of 15 strength is unlikely to be decisive in a game. However, had these been much higher strength pieces  or if player 1 repeated this tactic many times, we would see a significant gap emerge in terms of the total strength of the players in favor of player 1, and this could then be translated into additional territory and production for player 1, moving closer towards victory. Players should consequently be careful of clumping high value pieces or they may find they may lose all of them to a single piece from an opposing player. Effective use of this tactic is complicated but often makes the difference in distinguishing the good players from the great ones.</p>

                <h3>Minor Characteristics</h3>
                <p>This is a short (and far from complete) list of some less-important but still valuable characteristics of the game to be aware of when playing Halite.
                <ul>
                    <li>Perimeter to area ratio: A player with a high perimeter to area ratio will be able to move newly produced strength to borders more quickly and with less waste than a player with a low ratio, but may also be more vulnerable to attack.</li>
                    <li>Not all production is created equal: High-production zones located directly between spawn points may be a source of contention for players on many-player games. High-production zones nestled deep inside already-controlled territory may be more valuable because they are much more difficult to attack. However, production further from a border takes longer to move to combat regions and so may also be wasted on large maps.</li>
                    <li>Concentration of strength: Strength found near a specific border may be useful in strengthening that border from assault, but will consequently take longer to move to another border if that border comes under heavy assault.</li>
                    <li>Remember that pieces have their strength capped at 255; if a player moves their pieces too often, they'll waste possible production (since sites do not produce when moving), but if they don't move often enough, they'll be forced to combine pieces with too high a strength and lose strength to the 255 cap. This can be very difficult to optimize well, and many players lose a significant amount of their production to this effect.</li>
                </ul>
            </div>
        </div>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
