---
id: 33
title: Iliad Framework, how to create your first application
date: 2014-02-13T18:37:11+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=33
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
Hi. In this post we will create a basic web application using the Iliad Framework.

Please consult the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/12/iliad-introduction.html) if you need to install the web framework in your image.

An Iliad web application consists in a class that extends from `ILApplication` so lets create one:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILApplication&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBlogHelloIliad&lt;/span>
  &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
  &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
  &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
  &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

Now we need to declare where our application will be published. To configure the starting point for our web app we need to override the `path` class method:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad class&gt;&gt;path"&lt;/span>
&lt;span class="nf">path&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="s">'leonardoBlog'&lt;/span></code></pre>
</div>

Every Iliad Application implements the Front Controller partner. This means that it will receive every request addressed to the application from the http server.

The `index` controller will handle the request to the `/leonardoBlog` address, just like your `index.html` file. Let&#8217;s implement an `index` controller on the instance side of our `LcBlogHelloIliad` class, remember to put this method on the `controllers` protocol:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad&gt;&gt;index protocol controllers"&lt;/span>
&lt;span class="nf">index&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1:&lt;/span> &lt;span class="s">'Hi world!'&lt;/span> ]</code></pre>
</div>

Now, to check that everything is correct, you can go to <http://localhost:7070/leonardoBlog>. The controller method must return a &#8220;buildable object&#8221;. What&#8217;s a buildable object?

A buildable object can be many things&#8230; widgets, applications, and blocks of code. In the example I returned a block that, from an `ILHTMLBuilderElement` create an `h1` HTML element.

Please browse the code from the `ILHTMLBuilderElement` class. You will find many useful methods to create basically every HTML element you want.