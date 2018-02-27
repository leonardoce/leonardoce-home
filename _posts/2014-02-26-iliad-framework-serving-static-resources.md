---
id: 45
title: Iliad Framework, serving static resources
date: 2014-02-26T18:46:31+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=45
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous lesson](http://leonardoce.github.io/programming/smalltalk/2014/02/20/iliad-framework-lesson-six.html) we talked about how to use an Iliad webapp with a browser without Javascript enabled.

In this lesson we will talk about static resources to be included in your web-app.

When you will deploy your web application in the &#8220;real world&#8221; you want your static resources served by a specific web server as Apache but, while you are developing your application, you can make use of the web server embedded inside the Pharo Smalltalk environment: Komanche.

Iliad makes really easy to serve static resources. Now we will learn how to embed in your Pharo image [Bootstrap JS](http://getbootstrap.com). Let&#8217;s assume you already downloaded the compiled distribution to a file named `bootstrap-3.1.1-dist.zip` and that you extracted it in a folder named `~/tmp/bootstrap-3.1.1-dist`.

To include it in your Pharo image you need to derive a class from `ILMemoryDirectory` for every Bootstrap JS subdirectory like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILMemoryDirectory&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBootstrapCss&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILMemoryDirectory&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBootstrapFonts&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILMemoryDirectory&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBootstrapJs&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

Now we tell the classes to load all the resources contained in the corresponding directories. Open a workspace and execute the following commands:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">LcBootstrapCss&lt;/span> &lt;span class="nf">addAllFilesIn:&lt;/span>&lt;span class="s">'/home/leonardo/tmp/bootstrap-3.1.1-dist/css'&lt;/span>&lt;span class="p">.&lt;/span>
&lt;span class="nc">LcBootstrapJs&lt;/span> &lt;span class="nf">addAllFilesIn:&lt;/span>&lt;span class="s">'/home/leonardo/tmp/bootstrap-3.1.1-dist/js'&lt;/span>&lt;span class="p">.&lt;/span>
&lt;span class="nc">LcBootstrapFonts&lt;/span> &lt;span class="nf">addAllFilesIn:&lt;/span>&lt;span class="s">'/home/leonardo/tmp/bootstrap-3.1.1-dist/fonts'&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

You will see that a series of method are getting created under the `files` protocol. This is were the content of Bootstrap JS will be loaded.

Now we need to configure the subdirectory under the resources will be published. We just need to create a `path` method under the `accessing` protocol like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapJs&gt;&gt;path protocol accessing"&lt;/span>
&lt;span class="nf">path&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="s">'bootstrap/js'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapFonts&gt;&gt;path protocol accessing"&lt;/span>
&lt;span class="nf">path&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="s">'bootstrap/fonts'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapCss&gt;&gt;path protocol accessing"&lt;/span>
&lt;span class="nf">path&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="s">'bootstrap/css'&lt;/span></code></pre>
</div>

Now we need to register these new classes to be loaded by the Iliad framework and the code to register the app must also be executed when you load your class in a fresh image. This is the place where the `initialize` method of the class side comes in:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapJs class&gt;&gt;initialize protocol initialization"&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="nc">ILFileHandler&lt;/span> &lt;span class="nf">addDirectory:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nb">new&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapFonts class&gt;&gt;initialize protocol initialization"&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="nc">ILFileHandler&lt;/span> &lt;span class="nf">addDirectory:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nb">new&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBootstrapCss class&gt;&gt;initialize protocol initialization"&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="nc">ILFileHandler&lt;/span> &lt;span class="nf">addDirectory:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nb">new&lt;/span></code></pre>
</div>

Now the initialization must be done by hand in a workspace:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">LcBootstrapJs&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span>
&lt;span class="nc">LcBootstrapCss&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span>
&lt;span class="nc">LcBootstrapFonts&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

Try using a web browser pointing to <http://localhost:7070/bootstrap/js/bootstrap.js> and you will see the Bootstrap Js code. The same thing happens with the other directories.

Check that also the following URLS get served:

  * <http://localhost:7070/bootstrap/css/bootstrap.css>
  * <http://localhost:7070/bootstrap/fonts/glyphicons-halflings-regular.ttf>

Now you are sure that everything is working.