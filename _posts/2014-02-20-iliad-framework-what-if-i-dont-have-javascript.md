---
id: 43
title: 'Iliad Framework, what if I don&#039;t have Javascript?'
date: 2014-02-20T18:44:43+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=43
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/18/iliad-framework-lesson-five.html) we talked about how events are implemented, and you saw that actions are implemented with Javascript code.

Really? You want to keep Javascript off your browsing experience? Firefox, in the new releases, even don&#8217;t have a setting to disable Javascript without installing a plugin.

The iliad framework can also degrade to a non ajax version of your application without loosing any functionality. To enable this behaviour you must evaluate this code in a new workspace:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILAnchorElement&lt;/span> &lt;span class="nf">useAjaxOnlyForActions:&lt;/span> &lt;span class="bp">nil&lt;/span></code></pre>
</div>

Now disable Javascript in your browser and see your application working application with Javascript disabled. This is part of the Iliad goodness!