---
layout: doc_page
title: Downloads and Starter Kits
toc: true
description: Download a Python AI bot, C++ AI bot, or Java AI bot as a quick and easy way to get started playing in the Halite AI competition.
---
Download a Python, C++, or Java AI starter kit as a quick and easy way to get started playing in the Halite AI competition.

## About Your Starter Kit

These downloads include both the Halite game environment (pre-compiled) as well as the starter kit for that language, unless no operating system is specified.

## Download All Kits

[Source code download]({{ site.baseurl }}/{{ site.data.downloads.source }})

__Current version:__ {{ site.data.downloads.version }}

## Starter Kits by Language / Game Environment

__Linux users note:__ Your system must support GCC 4.9 or later (with corresponding GLIBC).

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
            <td>(no language, only game environment)</td>
            <td>empty zip…?</td>
            {% for file in site.data.downloads.environments %}
            <td><a href="{{ site.baseurl }}/{{ file }}">Download</a></td>
            {% endfor %}
        </tr>
    </tbody>
</table>

## Standalone Visualizer

Removed pending automated builds.

<!--

Only available for MacOS at the moment. (Sorry!)

[MacOS DMG]({{ site.baseurl }}/assets/downloads/Standalone_MacOS.dmg)

-->
