<html lang="en">
<head>
    <?php include 'includes/header.php'; ?>

    <title>Game Guide</title>

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
                <h1>Game Guide</h1>

                <h3>Overview</h3>

                <p>Halite is a multiplayer turn-based strategy game designed to have simple rules which are easy to learn but yield complex behaviors. This guide will give a description of the Halite game as well as some basic strategies useful for playing the game.</p>

                <p>Halite is played on a wraparound rectangular grid (the map), and each player's objective is to eliminate all other players from the map. Every player moves every piece they control every turn in a direction ϵ { STILL, NORTH, EAST, SOUTH, WEST }, leading to a very high branching factor. Players acquire territory by moving their pieces onto new territory with less strength than their pieces, and this territory will produce new pieces over time; this allows players to expand their control of the map. When pieces of opposing players try to occupy either the same or adjacent square, their pieces will fight, lose strength, and perhaps even die. Consequently, players should neither neglect to expand and increase their territory nor engage in wasteful fighting for little gain.</p>

                <p>Halite maps are generated at the start of each game and consist of a 2D grid of sites which each have three values:
                <ol>
                    <li>Owner - the player which controls the site. At the beginning of a game, almost all of sites are controlled by no player.</li>
                    <li>Strength - a number which indicates the power of the site. Sites with a higher strength are more powerful.</li>
                    <li>Production - a number which indicates how much strength the site gains every turn a player commands the site to move STILL.</li>
                </ol>
                The first two values are dynamic and will change over the course of the game; the last is static and remains constant for each site for the entirety of the game.</p>

                <p>Combat mechanics are very simple but encourage dynamic play. Every turn, every piece will deal an amount of damage equal to its own strength to all enemy pieces attempting to occupy either the same site or cardinally adjacent sites. If a site is controlled by no player, it will neither deal nor take damage from adjacent sites, but will deal damage to and take damage from pieces attempting to occupy that same site. When a piece takes damage, its strength is reduced correspondingly (i.e. a piece with a strength of 10 taking 3 damage becomes a piece with a strength of 7). Pieces engaged in combat with 0 or less remaining strength die and are removed from the game. These rules indicate that two opposing players will never occupy cardinally adjacent sites simultaneously.</p>

                <h3>Strategy Guide</h3>

                <p>In Halite, there are three basic and crucial characteristics of a position (game-state) that will determine how valuable it is. These are:
                <ul>
                    <li>Territory - the number of sites of the map which the player controls.</li>
                    <li>Production - the sum of the productions of the sites of the map which the player controls.</li>
                    <li>Strength - the sum of the strengths of the sites of the map which the player controls.</li>
                </ul>
                Let's go over each of these in a bit more detail.</p>

                <h4>Territory</h4>

                <p>Territory is the measure of how close a player is to winning the game, since the objective of the game is to reduce the territory controlled by other players on the map to zero and a Halite board has a finite number of sites available. The ultimate goal of a player should be focused on maximizing its own territory and minimizing the territory of other players. Simply controlling a lot of territory is not enough to give a player a high degree of certainty of winning, as the other characteristics (production and strength) can be just as influential in determining the outcome of a game, but controlling territory can also provide subtle advantages to a player which will be outlined later.</p>

                <h4>Production</h4>

                <p>Production is the measure of a player's capacity to produce new strength. It tends to determine the long-term outcome of a player's position, as a player with a high total production can often just wear down other players in a war of attrition and then take their territory once their strength is reduced to zero. Players should be aware that clever tactical movement can compensate for a lower production by dealing more damage with pieces than they take themselves.</p>

                <h4>Strength</h4>

                <p>Strength is the measure of a player's capacity to immediately capture new territory. It tends to determine the short-term outcome of a player's position, as a player with a high total strength can immediately attack and take new territory. Players should note that the placement of this strength is relevant; strength in the center of a player's territory cannot be utilized nearly as quickly as strength near the border.</p>

                <h4>Tactical Guide</h4>

                <p>The tactical concept of Halite is quite straightforward and entirely relies on a single principle which we will call "Overkill". Because a piece can deal damage to as many as 5 other pieces simultaneously, this means that it can output up to 5 times its own strength in damage. Take, for example, a scenario where just prior to the combat stage of each turn, player 1 has a single piece with a strength of 15. Player 2 has a piece with strength 25 directly to the north of this piece, and a piece with strength 45 directly to the south of it. Following combat resolution, player 1 will lose the piece in question, and player 2 will remain with both pieces alive, the northern piece with 10 strength and the southern piece with 30 strength. However, player 1 will have lost a total of 15 strength, whereas player 2 will have lost 30 strength. Consequently, this exchange actually went much better for player 1! Player 1 was able to reap this benefit because it was able to damage multiple pieces simultaneously, and the excess damage which one would normally expect from combat with multiple pieces had no effect because player 1 did not have any more strength to lose in that piece. That player 1's piece died during the encounter was critical for the tactical advantage, because it dealt more damage via this "Overkill" effect than it could possibly take.</p>

                <p>Of course, a difference of 15 strength is unlikely to be decisive in a game. However, had these been much higher strength pieces  or if player 1 repeated this tactic many times, we would see a significant gap emerge in terms of the total strength of the players in favor of player 1, and this could then be translated into additional territory and production for player 1, moving closer towards victory. Players should consequently be careful of clumping high value pieces or they may find they may lose all of them to a single piece from an opposing player. Effective use of this tactic is complicated but often makes the difference in distinguishing the good players from the great ones.</p>

                <h4>Minor Characteristics</h4>

                <p>This is a short (and far from complete) list of some less-important but still valuable characteristics of the game to be aware of when playing Halite.
                <ul>
                    <li>Perimeter to area ratio: A player with a high perimeter to area ratio will be able to move newly produced strength to borders more quickly and with less waste than a player with a low ratio, but may also be more vulnerable to attack.</li>
                    <li>Not all production is created equal: High-production zones located directly between spawn points may be a source of contention for players on many-player games. High-production zones nestled deep inside already-controlled territory may be more valuable because they are much more difficult to attack. However, production further from a border takes longer to move to combat regions and so may also be wasted on large maps.</li>
                    <li>Concentration of strength: Strength found near a specific border may be useful in strengthening that border from assault, but will consequently take longer to move to another border if that border comes under heavy assault.</li>
                </ul></p>

            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
