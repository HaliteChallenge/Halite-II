---
layout: doc_page
title: Downloads and Starter Kits
toc: false
description: Download a Python AI bot, C++ AI bot, or Java AI bot as a quick and easy way to get started playing in the Halite AI competition.
---
Download a language specific starter kit as a quick and easy way to get started playing in the Halite AI competition. 

We welcome the community to build new starter kits if there isn't one for their language of choice. Check out our guide on [how to build a new starter bot](create-new-starter-kit).

## About Your Starter Kit

Downloads include both the compiled Halite game environment and the starter kit for that language.

## Starter Kits by Language or Just the Game Environment

__Linux users:__ Your system must support GCC 4.9 or later (with corresponding GLIBC).

<div class="table-container">
    <table class="table">
        <thead>
            <tr>
                <td></td>
                <th colspan="{{ site.data.downloads.platforms | size }}" class="text-center">Operating System</th>
            </tr>
            <tr>
                <th>Language</th>
                {% for platform in site.data.downloads.platforms %}
                <td>{{ platform }}</td>
                {% endfor %}
            </tr>
        </thead>
        <tbody>
            {% for language in site.data.downloads.languages %}
            <tr>
                <td>{{ language.language }}</td>
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

## Download The Halite Source Code

If you want to dig deeper into the inner workings of Halite, you can also [download the source code]({{ site.baseurl }}/{{ site.data.downloads.source }}) of the game itself. 

__Current version:__ {{ site.data.downloads.version }}

