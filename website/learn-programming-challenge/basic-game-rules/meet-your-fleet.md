---
layout: doc_page
title: Meet Your Fleet
toc: false
description: Reference guide for unit properties of Halite
image: assets/images/temp/bot_1.png
content: website
sort_key: 0
---

## Map

Space is a 2D grid, where grid cells (not grid points) are identified by an X and Y coordinate. The top-left cell is cell (0, 0), with positive X extending towards the right and positive Y extending toward the bottom. All distances are Euclidean `sqrt(dx^2 + dy^2)`. Note that this makes the coordinate system left-handed!

## Ship

The basic unit of your fleet; a [nimble ship](the-halite-codex) built by depositing layers of pure Halite You start with three, but you'll want to make more.

### Properties

 <table class="table">
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
<table class="table">
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

<div class="static-container">
    <img style="width: 50%;height: auto;" src="/assets/images//tutorial-images/final-composite.png">
</div>

### Properties

<table class="table">
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
        <td></td>
        <td></td>
        <td></td>
    </tr>
    <tr>
        <td>Radius</td>
        <td></td>
        <td></td>
        <td></td>
    </tr>
    <tr>
        <td>Resources</td>
        <td></td>
        <td></td>
        <td></td>
    </tr>
</tbody>
</table>

