---
id: 63
title: Iliad Framework, index page
date: 2014-03-30T18:56:13+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=63
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
    In this post we will create a page to display the notes that we created with the project notes app. We will use the widget that we have created in the <a href="http://leonardoce.github.io/programming/smalltalk/2014/03/13/pnotes-status-widget.html">previous lesson</a> to display the current user and we will create a form to search inside notes.
  </p>
  <h1>
    Creating the PnViewNotes widget
  </h1>
  <p>
    Let&#8217;s start by creating a new widget that will be the content of the new page:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#PnViewNotes&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'searchstring'&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
  </div>
  <p>
    We created an instance variable, <code>searchString</code>, to remember the search string that will used to search inside the notes.
  </p>
  <h1>
    The rendering methods of the PnViewNotes widget
  </h1>
  <p>
    The interesting bits of this example are inside the rendering methods:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nf">contents&lt;/span>
&lt;span class="k">!&lt;/span>&lt;span class="nc">PnViewNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Project notes'&lt;/span>&lt;span class="p">.&lt;/span>
        &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Use this application as a scratch pad. You can &lt;/span>
&lt;span class="s">        attach to every page a series of tags to ease searching.'&lt;/span>&lt;span class="p">.&lt;/span>

        &lt;span class="nv">e&lt;/span> &lt;span class="nf">build:&lt;/span>&lt;span class="bp">self&lt;/span> &lt;span class="nf">searchForm&lt;/span>&lt;span class="p">.&lt;/span>
        &lt;span class="nv">e&lt;/span> &lt;span class="nf">build:&lt;/span>&lt;span class="bp">self&lt;/span> &lt;span class="nf">searchResults&lt;/span> ]</code></pre>
  </div>
  <p>
    Let&#8217;s present the search form:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnViewNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">searchForm&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">form&lt;/span> &lt;span class="nf">build:&lt;/span>[ &lt;span class="o">:&lt;/span>&lt;span class="nv">form&lt;/span> &lt;span class="o">|&lt;/span>
            &lt;span class="nv">form&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'input-group'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">build:&lt;/span>[&lt;span class="o">:&lt;/span>&lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">a&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'glyphicon glyphicon-plus input-group-addon'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span>[&lt;span class="bp">self&lt;/span> &lt;span class="nf">addNewNote&lt;/span>]&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">beSubmitOnChange&lt;/span>&lt;span class="p">;&lt;/span>
                    &lt;span class="nf">attributeAt:&lt;/span>&lt;span class="s">'placeholder'&lt;/span> &lt;span class="nf">put:&lt;/span>&lt;span class="s">'Search string'&lt;/span>&lt;span class="p">;&lt;/span>
                    &lt;span class="nf">action:&lt;/span>[ &lt;span class="o">:&lt;/span>&lt;span class="nv">value&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">searchstring&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">value&lt;/span>&lt;span class="p">.&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">doSearch&lt;/span> ]&lt;span class="p">.&lt;/span>
                 ] ] ]</code></pre>
  </div>
  <p>
    As you see we have used the BootstrapJs CSS declarations with the <code>class:</code> method of elements. When our element doesn&#8217;t support an attribute, like the <code>placeholder</code> attribute and the <code>ILInputElement</code> objects<code>, we can manually place attributes with the method</code>attributeAt:put:`.
  </p>
  <p>
    Another interesting bit is the <code>beSubmitOnChange</code> method, that will cause the <code>ILInputElement</code> to call the server every time the user changes the text field content, without waiting for the form to be submitted.
  </p>
  <p>
    Every time the search string is modified we place the new content in the <code>searchstring</code> instance variable and we invoke the <code>doSearch</code> method to search inside the notes.
  </p>
  <p>
    As we haven&#8217;t already implemented the notes DAO we delay the discussion of the <code>doSearch</code> method, that we implement like this:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnViewNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">doSearch&lt;/span></code></pre>
  </div>
  <p>
    The same thing happens with the <code>addNewNote</code> method:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnViewNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">addNewNote&lt;/span></code></pre>
  </div>
  <p>
    Until we haven&#8217;n implemented the <code>PnNote</code> object and its DAO we delay the implementation of the <code>searchResults</code> method:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnViewNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">searchResults&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> ]</code></pre>
  </div>
  <p>
    In this lesson we left many method unimplemented, but in the following one we implement the <code>PnNote</code> object and its DAO.
  </p>
</div>