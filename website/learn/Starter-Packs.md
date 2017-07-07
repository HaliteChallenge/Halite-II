---
layout: doc_page
title: Starter Packs
toc: true
---

## Directions

## Game Environment

## Starter Packs

{% for file in site.static_files %}
    {% if file.path contains 'assets/starter_kits' %}
- [{{ file.name }}]({{ site.baseurl }}{{ file.path }})
    {% endif %}
{% endfor %}
