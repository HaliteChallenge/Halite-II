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
                <p>In this short tutorial, we describe how to evaluate the position of player during a game, how to think about combat and remind the reader of a few subtleties of the game.</p>

                <h2>Evaluating a player's position in Halite</h2>
                <p>Just like for other board games like Chess or Go, not all Halite positions (i.e. the game state) are equally valuable. For a given player, there are 3 components which are most important in defining the quality of their position. These are:
                <ul>
                    <li>Territory - the number of sites of the map which the player controls.</li>
                    <li>Production - the sum of the productions of the sites of the map which the player controls.</li>
                    <li>Strength - the sum of the strengths of the sites of the map which the player controls.</li>
                </ul>
                Let's go over each of these in a bit more detail.</p>

                <h3>Territory</h3>
                <p>Territory is the measure of how close a player is to winning the game, since the goal of the game is to capture the full grid.<br>The ultimate goal of a player should be focused on maximizing its own territory and minimizing the territory of other players.<br>Simply controlling a lot of territory is not enough to give a player a high degree of certainty of winning, as the other characteristics (production and strength) can be just as influential in determining the outcome of a game, but controlling territory can also provide subtle advantages to a player which will be outlined later.</p>

                <h3>Production</h3>
                <p>Production is the measure of a player's capacity to produce new strength.<br>It tends to determine the long-term outcome of a player's position, as a player with a high total production can often just wear down other players in a war of attrition and then take their territory once their strength is reduced to zero.</p>

                <h3>Strength</h3>
                <p>Strength is the measure of a player's capacity to immediately capture new territory.<br>It tends to determine the short-term outcome of a player's position, as a player with a high total strength can immediately attack and take new territory.<br>Players should note that the placement of this strength is relevant; strength in the center of a player's territory cannot be utilized nearly as quickly as strength near the border.</p>

                <h3>Combat Guide</h3>
                <p>In Halite, a single piece can deal damage to as many as 4 other pieces simultaneously, this means that it can output up to 4 times its own strength in damage. This single principle (we call it `overkill`) is at the core of Halite tactics.</p>

                <h3>Minor Characteristics</h3>
                <p>This is a short (and far from complete) list of some less-important but still valuable characteristics of the game to be aware of when playing Halite.
                <ul>
                    <li>Perimeter to area ratio: A player with a high perimeter to area ratio will be able to move newly produced strength to borders more quickly and with less waste than a player with a low ratio, but may also be more vulnerable to attack.</li>
                    <li>Not all production is created equal: High-production zones located directly between spawn points may be a source of contention for players on many-player games. High-production zones nestled deep inside already-controlled territory may be more valuable because they are much more difficult to attack. However, production further from a border takes longer to move to combat regions and so may also be wasted on large maps.</li>
                    <li>Strength found near a specific border may be useful in strengthening that border from assault, but will consequently take longer to move to another border if that border comes under heavy assault.<br>Also remember that pieces have their strength capped at 255; if a player moves their pieces too often, they'll waste possible production (since sites do not produce when moving), but if they don't move often enough, they'll be forced to combine pieces with too high a strength and lose strength to the 255 cap. This can be very difficult to optimize well, and many players lose a significant amount of their production to this effect.</li>
                </ul>
            </div>
        </div>
        <?php include 'includes/footer.php'; ?>
    </div>


    <script src="https://ajax.googleapis.com/ajax/libs/jquery/2.2.4/jquery.min.js"></script>
    <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js"></script>
    <script src="https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js"></script>
    <script src="script/backend.js"></script>
    <script src="script/general.js"></script>
</body>
</html>
