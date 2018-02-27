---
id: 54
title: Pharo, Checking emails with regexp
date: 2014-03-04T18:52:07+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=54
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/03/03/users-dao-intermezzo.html) we build a login form and we saw that we should provide an user registration form.

We are going to build a registration form so we need to check emails. I will create a new class `PnUtils` to contain all the string-checking utilities that we need for our App:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">Object&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#PnUtils&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
</div>

Now, in a class method, I will implement my email-checking:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUtils class&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'as yet unclassified'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">checkEmail:&lt;/span> &lt;span class="nv">anEmail&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">anEmail&lt;/span> &lt;span class="nf">asUppercase&lt;/span> &lt;span class="nf">matchesRegex:&lt;/span>  &lt;span class="s">'[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z][A-Z][A-Z]?[A-Z]?'&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

As you see I check the email addresses using the RegExp feature of Smalltalk. The regexp is taken from [this site](http://www.regular-expressions.info/index.html).

I know that at this times my should send an activation email to let the user activate yourself but I won&#8217;t do it as I don&#8217;t want to mess up with SMTP configuration.

In this discussion I want to show you how simple is to use regexp from Pharo Smalltalk, without loading any external libraries.

Ok. We must test it to see if it&#8217;s working or not:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">TestCase&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#PnUtilsTest&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog-Tests'&lt;/span>

&lt;span class="nf">!&lt;/span>&lt;span class="nc">PnUtilsTest&lt;/span> &lt;span class="nf">methodsFor:&lt;/span> &lt;span class="s">'tests'&lt;/span>&lt;span class="nf">!&lt;/span>
&lt;span class="nf">testEmailOk&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span> (&lt;span class="nc">PnUtils&lt;/span> &lt;span class="nf">checkEmail:&lt;/span> &lt;span class="s">'leonardoce@interfree.it'&lt;/span> )&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span> (&lt;span class="nc">PnUtils&lt;/span> &lt;span class="nf">checkEmail:&lt;/span> &lt;span class="s">'leonardoce@interfree.com'&lt;/span> )&lt;span class="p">.&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUtilsTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'tests'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">testEmailNotOk&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span> (&lt;span class="nc">PnUtils&lt;/span> &lt;span class="nf">checkEmail:&lt;/span> &lt;span class="s">'leonardoce@interfree.sirtr'&lt;/span> ) &lt;span class="nf">not&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

The tests should all be working.