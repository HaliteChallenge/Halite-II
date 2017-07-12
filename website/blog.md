---
layout: page
title: Halite Competition Blog
toc: false
full_width: true
permalink: /blog/
---

{% for post in site.posts %}	
<h2><a href="{{ post.url }}">{{ post.title }}</a></h2>

<p>
<strong>{{ post.date | date: "%B %e, %Y" }}</strong> . {{ post.category }} . 
<a href="{{ post.url }}"></a></p>
{% endfor %}	

{% if site.posts.size == 0 %}
Sorry, no posts yet!
{% endif %}