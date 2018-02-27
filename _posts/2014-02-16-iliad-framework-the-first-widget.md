---
id: 37
title: Iliad Framework, the first widget
date: 2014-02-16T18:40:36+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=37
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In this lesson we will create the first widget using the Iliad framework.

We have studied applications and controllers but the core of the Iliad framework is the concept of **widget**. Every page served by the Iliad framework is composed by widgets, as widget are written on the page as HTML tags. A widget is:

  * _stateful_ and this means that the instace variables of the widget class are conserved between HTTP requests in the session state;
  * _reusable_ every widget class can (and will) be used many times even in the same session using multiple widgets;
  * _a container_ of child widgets.

Every widget is a subclass of the `ILWidget` class. Let&#8217;s create out first widget:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcCounterWidget&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

The contents of a widget must be defined overriding the `contents` method of the widget class:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcCounterWidget&gt;&gt;contents protocol building"&lt;/span>
&lt;span class="nf">contents&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'I''m a widget!'&lt;/span> ]</code></pre>
</div>

When I said that **every** page server by the Iliad framework is composed by widgets I was actually right because also the `ILApplication` class extends `ILWidget`!

To attach out widget to the application we will add an instance variable to the application class to store the widget instance.

Create a new accessor for the `firstWidget` component that create the instance if needed:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad&gt;&gt;firstWidget protocol accessing"&lt;/span>
&lt;span class="nf">firstWidget&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">firstWidget&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="nv">firstWidget&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">LcCounterWidget&lt;/span> &lt;span class="nb">new&lt;/span> ]</code></pre>
</div>

Now I will create a new controller just to show how the widget can be rendered:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad&gt;&gt;widgetExample protocol contollers"&lt;/span>
&lt;span class="nf">widgetExample&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Hi! Widget example'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">build:&lt;/span>&lt;span class="bp">self&lt;/span> &lt;span class="nf">firstWidget&lt;/span> ]</code></pre>
</div>

As always _pay attention_ on putting the `widgetExample` in the right protocol!

Now you can go here: <http://localhost:7070/leonardoBlog/widgetExample> to see the example working.

This article will be long but I should make a remark on the `firstWidget` accessor method. We could also use the `initialize` method to create a new instance of the widget when the `LcBlogHelloIliad` class is created and this will actually work but, if you choose my implementation (which is copyied from the Iliad examples) you can reuse your old browser session to try new code and this is really good, believe me.