---
id: 39
title: Iliad Framework, reacting to events
date: 2014-02-17T18:41:57+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=39
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
For now we have created a web application with a child widget that generate a static HTML. Now we need to receive and send data to make this application interactive.

I would like to create with you the example that you already find in the Iliad framework examples, the counter example. You can already see it in action at this link: <http://localhost:7070/examples/counters>.

A counter is simply an integer that starts with zero and can be incremented and decremented using two links. We start with creating an instance variable in the `LcCounterWidget` class that will hold the current value of the counter:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcCounterWidget&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'value'&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

We create the accessors accordingly with what we have said in the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/16/iliad-lesson-three.html).

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcCounterWidget&gt;&gt;value protocol accessing"&lt;/span>
&lt;span class="nf">value&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">value&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="nv">value&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="m">0&lt;/span> ]&lt;span class="p">.&lt;/span>

&lt;span class="c">"LcCounterWidget&gt;&gt;value: protocol accessing"&lt;/span>
&lt;span class="nf">value:&lt;/span>&lt;span class="nv">aNumber&lt;/span>
    &lt;span class="nv">value&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">aNumber&lt;/span></code></pre>
</div>

Now I create the actions corresponding to the links under the counter:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcCounterWidget&gt;&gt;increase protocol accessing"&lt;/span>
&lt;span class="nf">increase&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">value:&lt;/span> (&lt;span class="bp">self&lt;/span> &lt;span class="nf">value&lt;/span> &lt;span class="nf">+&lt;/span> &lt;span class="m">1&lt;/span>)&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">markDirty&lt;/span>&lt;span class="p">.&lt;/span>

&lt;span class="c">"LcCounterWidget&gt;&gt;decrease protocol accessing"&lt;/span>
&lt;span class="nf">decrease&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">value:&lt;/span> (&lt;span class="bp">self&lt;/span> &lt;span class="nf">value&lt;/span> &lt;span class="nf">-&lt;/span> &lt;span class="m">1&lt;/span>)&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">markDirty&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

As you may see these actions barely increment and decrement the value using the accessor method and then call the `markDirty` method, who means that the component has to be redrawn on the client side.

Now we need to modify the `contents` method to display the value of the counter on the page:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcCounterWidget&gt;&gt;firstWidget protocol building"&lt;/span>
&lt;span class="nf">contents&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span>
        &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">value&lt;/span> &lt;span class="nf">asString&lt;/span>&lt;span class="p">.&lt;/span>
        &lt;span class="nv">e&lt;/span> &lt;span class="nf">a&lt;/span>
        &lt;span class="nf">text:&lt;/span> &lt;span class="s">'++'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">action:&lt;/span> [ &lt;span class="bp">self&lt;/span> &lt;span class="nf">increase&lt;/span> ]&lt;span class="p">.&lt;/span>
        &lt;span class="nv">e&lt;/span> &lt;span class="nf">a&lt;/span>
        &lt;span class="nf">text:&lt;/span> &lt;span class="s">'--'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">action:&lt;/span> [ &lt;span class="bp">self&lt;/span> &lt;span class="nf">decrease&lt;/span> ] ]</code></pre>
</div>

Now you can try the URL: <http://localhost:7070/leonardoBlog/widgetExample> to see that our implementation is working.