---
layout: doc_page
title: Downloads and Starter Kits
toc: false
description: Halite engine and language specific starter kit downloads
---

Download a language specific starter kit as a quick and easy way to get started playing in the Halite AI competition. 

We welcome our community to build new starter kits, if there isn't one for their preferred language. Check out our [guide](creating-a-new-starter-kit) on how to build a new starter bot.

Downloads include both the compiled Halite game environment and the starter kit for that language.

## Downloads

System requirements are available [here](system-requirements).

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
                <td>NA</td>
                {% for file in site.data.downloads.environments %}
                <td><a href="{{ site.baseurl }}/{{ file }}">Download</a></td>
                {% endfor %}
            </tr>
        </tbody>
    </table>
</div>

## Source Code

__Current version:__ {{ site.data.downloads.version }}

[Download the source code]({{ site.baseurl }}/{{ site.data.downloads.source }}) or visit our [GitHub repository](https://github.com/HaliteChallenge/Halite-II). 

## Halite Tools

* [Halite Client](): Upload bots, download game data and run regression tests against your bots.
* [Halite Offline Visualizer](): View Halite games offline.
