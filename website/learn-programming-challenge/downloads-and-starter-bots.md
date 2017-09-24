---
layout: doc_page
title: Downloads and Starter Kits
toc: false
description: Download a Python AI bot, C++ AI bot, or Java AI bot as a quick and easy way to get started playing in the Halite AI competition.
---
Download a language specific starter kit as a quick and easy way to get started playing in the Halite AI competition. 


We welcome the community to build new starter kits if there isnt one for their language of choice. Check out our [guide]() on how to build a new starter bot.

## About Your Starter Kit

Downloads include both the compiled Halite game environment and the starter kit for that language.

## Download All Kits

[Download]({{ site.baseurl }}/{{ site.data.downloads.source }})

## Game Environment version

__Version:__ {{ site.data.downloads.version }}

## Starter Kits by Language / Game Environment

__Linux users:__ Your system must support GCC 4.9 or later (with corresponding GLIBC).

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

## Halite Tools

* [Halite Client](https://www.dropbox.com/s/ifn743v9a785x6h/hlt_client.zip?dl=0): For uploading bots from your terminal

* Offline Game Visualizer: TBD
