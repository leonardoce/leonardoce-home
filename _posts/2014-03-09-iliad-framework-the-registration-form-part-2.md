---
id: 58
title: Iliad Framework, the registration form (part 2)
date: 2014-03-09T18:54:04+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=58
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
<div class="post">
  <p>
    In the <a href="http://leonardoce.github.io/programming/smalltalk/2014/03/07/pnotes-registration-page.html">previous post</a> we build a registration form and in this one we will attach it to our application.
  </p>
  <p>
    We start adding an instance variable to our application to contain the registration widget and another instance variable to contain the current user:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILApplication&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBlogProjectNotes&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'loginWidget registrationWidget currentuser'&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
  </div>
  <p>
    As we have done for the <code>loginWidget</code>, we create an accessor method that will construct the widget if it hasn&#8217;t been instanciated:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">registrationWidget&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">registrationWidget&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="nv">registrationWidget&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnCreateUser&lt;/span> &lt;span class="nb">new&lt;/span> ]</code></pre>
  </div>
  <p>
    We also create accessors for the current user instance variable:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">currentuser:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nv">currentuser&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span> &lt;span class="nf">methodsFor:&lt;/span> &lt;span class="s">'accessing'&lt;/span>&lt;span class="nf">!&lt;/span>
&lt;span class="nf">currentuser&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">currentuser&lt;/span></code></pre>
  </div>
  <p>
    Now we create a controller for the registration page:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'controllers'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">register&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'container'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">build:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">registrationWidget&lt;/span> ]</code></pre>
  </div>
  <p>
    Now we have our new controller and we can test it from the login page. The controller name, <code>register</code>, match with the <code>href</code> in the login page.
  </p>
</div>