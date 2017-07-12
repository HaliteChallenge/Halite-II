---
layout: doc_page
title: Starter Packs
toc: true
---

## Game Environment

{% for file in site.static_files %}
    {% if file.path contains 'assets/environments' %}
- [{{ file.name }}]({{ site.baseurl }}{{ file.path }})
    {% endif %}
{% endfor %}

## Starter Packs

{% for file in site.static_files %}
    {% if file.path contains 'assets/starter_kits' %}
- [{{ file.name }}]({{ site.baseurl }}{{ file.path }})
    {% endif %}
{% endfor %}
