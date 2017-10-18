---
layout: doc_page
title: Downloads and Starter Kits
description: Halite engine and language specific starter kit downloads
image: assets/images/temp/bot_1.png
content: website
---

Download a language specific starter kit as a quick and easy way to get started playing in the Halite AI competition. 

We welcome our community to build new starter kits, if there isn't one for their preferred language. Check out our [guide](creating-a-new-starter-kit) on how to build a new starter bot.

## System Requirements
System requirements are detailed [here](system-requirements).

## Starter kits Download

<div class="table-container">
    <table class="table">
        <thead>
            <tr>
                <td></td>
                <th colspan="{{ site.data.downloads.platforms | size }}" class="text-center">Operating System</th>
            </tr>
            <tr>
                <th>Language</th>
                <td>Version</td>
                {% for platform in site.data.downloads.platforms %}
                <td>{{ platform }}</td>
                {% endfor %}
            </tr>
        </thead>
        <tbody>
            {% for language in site.data.downloads.languages %}
            <tr>
                <td>{{ language.language }}</td>
                <td>{{ language.version }}</td>
                {% for file in language.files %}
                <td><a href="{{ site.baseurl }}/{{ file }}">Download</a></td>
                {% endfor %}
            </tr>
            {% endfor %}
            <tr>
                <td>Only Game Environment</td>
                <td>{{ site.data.downloads.version }}</td>
                <td>NA</td>
                {% for file in site.data.downloads.environments %}
                <td><a href="{{ site.baseurl }}/{{ file }}">Download</a></td>
                {% endfor %}
            </tr>
        </tbody>
    </table>
</div>

## Halite Tools and Source Download

<div class="table-container">
    <table class="table">
        <thead>
            <tr>
                <th>Tool</th>
                <td>Version</td>
                <td>All Platforms</td>
                <td>MacOS</td>
                <td>Linux</td>
                <td>Windows</td>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Halite Client Tool</td>
                <td>0.9.0-Beta</td>
                <td><a href="https://storage.cloud.google.com/halite-assets/HaliteClient.zip">Download</a></td>
                <td>NA</td>
                <td>NA</td>
                <td>NA</td>
            </tr>
            <tr>
                <td>Halite Visualizer</td>
                <td>0.9.0-Beta</td>
                <td>NA</td>
                <td><a href="https://storage.cloud.google.com/halite-assets/visualizer/Halite%20II%20Visualizer-mac.zip">Download</a></td>
                <td><a href="https://github.com/HaliteChallenge/Halite-II/blob/master/tools/standalone_visualizer/README.md">Instructions</a></td>
                <td><a href="https://github.com/HaliteChallenge/Halite-II/blob/master/tools/standalone_visualizer/README.md">Instructions</a></td>
            </tr>
            <tr>
                <td>Source</td>
                <td>NA</td>
                <td><a href="{{ site.baseurl }}/{{ site.data.downloads.source }}">Download</a></td>
                <td>NA</td>
                <td>NA</td>
                <td>NA</td>
            </tr>
        </tbody>
    </table>
</div>
