---
id: 47
title: Iliad Framework, how to customize the page generation
date: 2014-02-27T18:48:11+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=47
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/26/iliad-memory-directory.html) we talked about how to include static resources in the Pharo image to be server with our application.

Let&#8217;s start with a new web application and, as we have seen in the [relative post](http://leonardoce.github.io/programming/smalltalk/2014/02/13/iliad-lesson-one.html), we must create a class deriving from `ILApplication`, write the `path` method and an empty `index` controller:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILApplication&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBlogProjectNotes&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcApplication class&gt;&gt;path protocol accessing"&lt;/span>
&lt;span class="nf">path&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="s">'ProjectNotes'&lt;/span></code></pre>
</div>

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcApplication&gt;&gt;index protocol controllers"&lt;/span>
&lt;span class="nf">index&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Hi!'&lt;/span> ]</code></pre>
</div>

Remember to put the `index` method in the protocol `controllers`!

Now we must integrate all the bootstrap code from the static resources generated in the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/26/iliad-memory-directory.html) in the page.

Do to that we must override the `updatePage:` method from `ILApplication` like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcApplication&gt;&gt;updatePage protocol updating"&lt;/span>
&lt;span class="nf">updatePage:&lt;/span> &lt;span class="nv">aPage&lt;/span>
    &lt;span class="c">"Includes bootstrap JS"&lt;/span>

    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">link&lt;/span>
      &lt;span class="nf">rel:&lt;/span> &lt;span class="s">'stylesheet'&lt;/span>&lt;span class="p">;&lt;/span>
      &lt;span class="nf">href:&lt;/span> &lt;span class="s">'/bootstrap/css/bootstrap.min.css'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">link&lt;/span>
      &lt;span class="nf">rel:&lt;/span> &lt;span class="s">'stylesheet'&lt;/span>&lt;span class="p">;&lt;/span>
      &lt;span class="nf">href:&lt;/span> &lt;span class="s">'/bootstrap/css/bootstrap-theme.min.css'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">javascript&lt;/span> &lt;span class="nf">src:&lt;/span> &lt;span class="s">'/bootstrap/js/bootstrap.min.js'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">meta&lt;/span>
      &lt;span class="nf">httpEquiv:&lt;/span> &lt;span class="s">'X-UA-Compatible'&lt;/span>&lt;span class="p">;&lt;/span>
      &lt;span class="nf">content:&lt;/span> &lt;span class="s">'IE=edge'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">meta&lt;/span>
      &lt;span class="nf">name:&lt;/span> &lt;span class="s">'viewport'&lt;/span>&lt;span class="p">;&lt;/span>
      &lt;span class="nf">content:&lt;/span> &lt;span class="s">'width=device-width, initial-scale=1'&lt;/span>&lt;span class="p">.&lt;/span>

    &lt;span class="c">"We want a title for our app"&lt;/span>
    &lt;span class="nv">aPage&lt;/span> &lt;span class="nf">head&lt;/span> &lt;span class="nf">title:&lt;/span> &lt;span class="s">'A project note-taking app'&lt;/span></code></pre>
</div>

The `updatePage:` method get called when the page has been constructed and before its contents are sent to the browser. In this method you can customize the generated page as you want.

Voila&#8217;! We have integrated Bootstrap JS.