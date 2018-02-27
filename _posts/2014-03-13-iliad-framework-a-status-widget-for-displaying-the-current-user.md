---
id: 61
title: Iliad Framework, a status widget for displaying the current user
date: 2014-03-13T18:55:09+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=61
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
    In the <a href="http://leonardoce.github.io/programming/smalltalk/2014/03/09/pnotes-registration-page-2.html">previous post</a> we completed the users registration form. Now we can use the login form.
  </p>
  <p>
    To show the current user in the next pages we will implement a current user heading as a widget:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#PnCurrentUserHeading&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
  </div>
  <p>
    In the contents of this widget we will use the <code>application</code> method of the <code>ILWidget</code> class to access the current application and the current user:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCurrentUserHeading&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">contents&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">div&lt;/span>
        &lt;span class="nf">cssClass:&lt;/span> &lt;span class="s">'navbar navbar-static-top bs-docs-nav'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">header&lt;/span> &lt;span class="o">|&lt;/span>
                    &lt;span class="nv">header&lt;/span> &lt;span class="nf">div&lt;/span>
                        &lt;span class="nf">class:&lt;/span> &lt;span class="s">'navbar-brand'&lt;/span>&lt;span class="p">;&lt;/span>
                        &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Project Notes for '&lt;/span> &lt;span class="nf">,&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">application&lt;/span> &lt;span class="nf">currentuser&lt;/span> &lt;span class="nf">email&lt;/span>&lt;span class="p">.&lt;/span>
                    (&lt;span class="nv">header&lt;/span> &lt;span class="nf">ul&lt;/span>
                        &lt;span class="nf">cssClass:&lt;/span> &lt;span class="s">'nav navbar-nav'&lt;/span>&lt;span class="p">;&lt;/span>
                        &lt;span class="nf">li&lt;/span>)
                        &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">logout&lt;/span> &lt;span class="o">|&lt;/span>
                            &lt;span class="nv">logout&lt;/span> &lt;span class="nf">a&lt;/span>
                                &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Logout'&lt;/span>&lt;span class="p">;&lt;/span>
                                &lt;span class="nf">action:&lt;/span> [ &lt;span class="bp">self&lt;/span> &lt;span class="nf">logout&lt;/span> ] ] ] ]</code></pre>
  </div>
  <p>
    We also included a <code>logout</code> action that reset the current user and redirect the application to the login page:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCurrentUserHeading&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">logout&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">application&lt;/span> &lt;span class="nf">currentuser:&lt;/span>&lt;span class="bp">nil&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">redirectToLocal:&lt;/span> &lt;span class="s">'login'&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
  </div>
  <p>
    We include this widget in the application class like we have done before with the login and the registration page:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILApplication&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBlogProjectNotes&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'loginWidget registrationWidget currentuser currentUserWidget'&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span>

&lt;span class="nf">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span> &lt;span class="nf">methodsFor:&lt;/span> &lt;span class="s">'accessing'&lt;/span>&lt;span class="nf">!&lt;/span>
&lt;span class="nf">currentUserWidget&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">currentUserWidget&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="nv">currentUserWidget&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnCurrentUserHeading&lt;/span> &lt;span class="nb">new&lt;/span> ]</code></pre>
  </div>
  <p>
    Now we create a new <code>notes</code> controller:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'controllers'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">notes&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">build:&lt;/span>(&lt;span class="bp">self&lt;/span> &lt;span class="nf">currentUserWidget&lt;/span>)&lt;span class="p">.&lt;/span> ]</code></pre>
  </div>
</div>