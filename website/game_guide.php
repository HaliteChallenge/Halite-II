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

                <p>Halite is played on a wraparound rectangular grid (the map), and each player's objective is to eliminate all other players from the map. Every player moves every piece they control every turn in a direction ϵ { STILL, NORTH, EAST, SOUTH, WEST }, leading to a very high branching factor. Players acquire territory by moving their pieces onto new territory with less strength than the pieces, and this territory will produce new pieces over time; this allows players to expand their control of the map. When pieces of opposing players try to occupy either the same or adjacent square, their pieces will fight, lose strength, and perhaps even die. Consequently, players should neither neglect to expand and increase their territory nor engage in wasteful fighting for little gain.</p>

                <p>Halite maps are generated at the start of each game and consist of a 2D grid of sites which each have three values:
                <ol>
                    <li>Owner - the player which controls the site. At the beginning of a game, almost all of sites are controlled by no player.</li>
                    <li>Strength - a number which indicates the power of the site. Sites with a higher strength are more powerful.</li>
                    <li>Production - a number which indicates how much strength the site gains every turn a player commands the site to move STILL.</li>
                </ol>
                The first two values are dynamic and will change over the course of the game; the last is static and remains constant for each site for the entirety of the game.</p>

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

                <p>Production is the measure of a player's capacity to produce new strength. It tends to determine the long-term outcome of a player's position, as a player with a high total production can often just wear down other players in a war of attrition and then take their territory once their strength is reduced to zero. Players should be aware that clever tactical movement can compensate for a lower production by dealing more damage with pieces than they take themselves. Additionally, players should note that not all production territory is necessarily created equal, for highly exposed territory with high production</p>

            </div>
        </div>
    </div>

    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
