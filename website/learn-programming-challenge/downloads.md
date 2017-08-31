---
layout: doc_page
title: Downloads
toc: true
---

See the [Quick Start]({{ site.baseurl }}/learn) to get started.

These downloads include both the Halite game environment (pre-compiled) as well as the starter kit for that language, unless no operating system is specified.

[Source code download]({{ site.baseurl }}/{{ site.data.downloads.source }})

__Current version:__ {{ site.data.downloads.version }}

## Game Environments and Starter Kits

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

## Sample Bots

C++, Java, and Python3.

[Download here]({{ site.baseurl }}/assets/downloads/sample_bots.zip)

## Standalone Visualizer

Removed pending automated builds.

<!--

Only available for MacOS at the moment. (Sorry!)

[MacOS DMG]({{ site.baseurl }}/assets/downloads/Standalone_MacOS.dmg)

-->
