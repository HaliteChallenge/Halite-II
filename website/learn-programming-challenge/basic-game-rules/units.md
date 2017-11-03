---
layout: doc_page
title: Unit Reference
toc: false
description: Reference guide for unit properties of Halite
image: assets/images/opengraph.png
content: website
sort_key: 0
---

## Map

Space is a 2D grid, where grid cells (not grid points) are identified by an X and Y coordinate. The top-left cell is cell (0, 0), with positive X extending towards the right and positive Y extending toward the bottom. All distances are Euclidean `sqrt(dx^2 + dy^2)`. Note that this makes the coordinate system left-handed!

<div class="static-container text-center">
    <img style="width: 60%;height: auto;" src="/assets/images//tutorial-images/map.png">
</div>

## Ship

The basic unit of your fleet; a [nimble ship](the-halite-codex) built by depositing layers of pure Halite You start with three, but you'll want to make more.

<div class="static-container text-center">
    <img style="width: 40%;height: auto;" src="https://storage.cloud.google.com/halite-content/ship-movement.gif">
    <img style="width: 40%;height: auto;" src="https://storage.cloud.google.com/halite-content/ship-combat.gif">
</div>

### Properties

 <table class="table table-leader" style="color:white;">
    <thead>
        <tr>
            <th>Property</th>
            <th>Description</th>
            <th>Default</th>
            <th>Max</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Health</td>
            <td>Don't let this hit 0.</td>
            <td>255</td>
            <td>255</td>
        </tr>
        <tr>
            <td>Velocity</td>
            <td>How fast you're going.</td>
            <td>0</td>
            <td>
                7 units/turn
            </td>
        </tr>
        <tr>
            <td>Attack Damage</td>
            <td>Reduce enemy ships' health.</td>
            <td>64</td>
            <td>64</td>
        </tr>
        <tr>
            <td>Attack Range</td>
            <td>How far away you can attack something.</td>
            <td>5 units</td>
            <td>5 units</td>
        </tr>
    </tbody>
</table>

### Commands
<table class="table table-leader" style="color:white;">
    <thead>
        <tr>
            <th>Command</th>
            <th>Turns</th>
            <th>Description</th>
        </tr>
    </thead>
    <tbody>
    <tr>
        <td>Thrust</td>
        <td>1</td>
        <td>
            <dl>
                <dt>Angle</dt>
                <dd>
                    The direction you want to accelerate in. Note that positive angles go clockwise! <br><br>
                </dd>
                <dt>Magnitude</dt>
                <dd>
                    How much you want to accelerate. 0-7 units/turn.
                </dd>
            </dl>
        </td>
    </tr>
    <tr>
        <td>Dock</td>
        <td>5</td>
        <td>
           Dock a ship to a planet, so that we may use its resources. Ships cannot attack and is vulnerable during dock
        </td>
    </tr>
    <tr>
        <td>Undock</td>
        <td>5</td>
        <td>Undock a ship from a planet, so that you may use it again. Ships cannot attack and is vulnerable during dock
        </td>
    </tr>
    </tbody>
</table>

## Planet

The basic unit that generates Halite ships, you can conquer planets, fight other plays for control of planets and dock to generate more ships from the Halite. Resources are essentially infinite for all planets, the larger the planet the more ships that can dock to it and corresponding generate more ships per turn.

<div class="static-container text-center">
    <img style="width: 50%;height: auto;" src="https://storage.cloud.google.com/halite-content/planet-production.gif">
</div>

### Properties

<table class="table table-leader" style="color:white;">
    <thead>
        <tr>
            <th>Property</th>
            <th>Description</th>
            <th>Default</th>
            <th>Max</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Health</td>
            <td>Varies from planet to planet, function of radius</td>
            <td>Random</td>
            <td>Random</td>
        </tr>
        <tr>
            <td>Radius</td>
            <td>Varies from planet to planet</td>
            <td>Random</td>
            <td>Random</td>
        </tr>
        <tr>
            <td>Dock Radius</td>
            <td>The distance from the planet edge a ship can dock</td>
            <td>4 units</td>
            <td>4 units</td>
        </tr>
        <tr>
            <td>Explosion Radius</td>
            <td>On planet destruction, ships are destroyed if they are within this radius</td>
            <td>10 units</td>
            <td>10 units</td>
        </tr>
         <tr>
            <td>Docking Sport</td>
            <td>A function of the radius. Larger planets have more spots</td>
            <td>Function of planet radius</td>
            <td>Function of planet radius</td>
        </tr>
        <tr>
            <td>Production</td>
            <td>Rate at which ships can be mined from a planet</td>
            <td>1 ship/12 turns</td>
            <td>Per docked ship - (1 ship/12 turns)</td>
        </tr>
        <tr>
            <td>Spawn Radius</td>
            <td>Distance from the planet edge where ships are spawned</td>
            <td>2 units</td>
            <td>2 units</td>
        </tr>
    </tbody>
</table>

