---
id: 51
title: Iliad Framework, the users-dao intermezzo
date: 2014-03-03T18:50:22+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=51
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/03/02/iliad-login-page.html) we build a login form. In that form we included a &#8220;Register&#8221; link and so we must provide a way to make our users entries persistent.

Using Smalltalk the image is already persistent, saved every time you need reloaded from the image file. To cut a long story short if you don&#8217;t need ACID transactions and a strong mechanism to backup and restore data you can use the image as your database.

This may look strange (and do look strange) to non-smalltalkers but this [wonderful post](http://onsmalltalk.com/simple-image-based-persistence-in-squeak/) explains very well why using your image as a persistence layer isn&#8217;t a crazy idea.

For our didactic purpose we can safely use the image.

We need to store users so we start by creating an `PnUser` class:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">Object&lt;/span>&lt;span class="k"> subclass: &lt;/span>&lt;span class="ss">#PnUser&lt;/span>
&lt;span class="k">    instanceVariableNames: &lt;/span>&lt;span class="s">'realname surname email md5pwd'&lt;/span>
&lt;span class="k">    classVariableNames: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    poolDictionaries: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    category: &lt;/span>&lt;span class="s">'LeonardoBlog'&lt;/span>&lt;span class="k">!&lt;/span></code></pre>
</div>

As you see this is a really simple class. You can automatically generate the accessors method using the Pharo browser. You will obtain something like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">email&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">email&lt;/span>

&lt;span class="nf">password:&lt;/span>&lt;span class="nv">pwd&lt;/span>
    &lt;span class="nf">md5pwd&lt;/span> &lt;span class="o">:=&lt;/span>  (&lt;span class="nc">MD5&lt;/span> &lt;span class="nf">hashMessage:&lt;/span>&lt;span class="nv">pwd&lt;/span>) &lt;span class="nf">hex&lt;/span>&lt;span class="p">.&lt;/span>

&lt;span class="nf">realname:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nv">realname&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">email:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nf">email&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">realname&lt;/span>
    &lt;span class="err">^&lt;/span> &lt;span class="nf">realname&lt;/span>

&lt;span class="nf">surname:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nf">surname&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">surname&lt;/span>
    &lt;span class="err">^&lt;/span> &lt;span class="nf">surname&lt;/span>

&lt;span class="nf">email&lt;/span>
    &lt;span class="err">^&lt;/span> &lt;span class="nf">email&lt;/span>

&lt;span class="nf">email:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nf">email&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">realname&lt;/span>
    &lt;span class="err">^&lt;/span> &lt;span class="nf">realname&lt;/span>

&lt;span class="nf">surname:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nf">surname&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span>

&lt;span class="nf">surname&lt;/span>
    &lt;span class="err">^&lt;/span> &lt;span class="nf">surname&lt;/span>

&lt;span class="nf">realname:&lt;/span> &lt;span class="nv">anObject&lt;/span>
    &lt;span class="nf">realname&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">anObject&lt;/span></code></pre>
</div>

Instead of storing the password we store the MD5 hash code and so we have these methods:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">password:&lt;/span>&lt;span class="nv">pwd&lt;/span>
    &lt;span class="nv">md5pwd&lt;/span> &lt;span class="o">:=&lt;/span>  (&lt;span class="nc">MD5&lt;/span> &lt;span class="nf">hashMessage:&lt;/span>&lt;span class="nv">pwd&lt;/span>) &lt;span class="nf">hex&lt;/span>&lt;span class="p">.&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">hasPassword:&lt;/span>&lt;span class="nv">aString&lt;/span>
    &lt;span class="o">^&lt;/span> (&lt;span class="nc">MD5&lt;/span> &lt;span class="nf">hashMessage:&lt;/span>&lt;span class="nv">aString&lt;/span>) &lt;span class="nf">hex&lt;/span> &lt;span class="nf">=&lt;/span> &lt;span class="nv">md5pwd&lt;/span> &lt;span class="p">.&lt;/span></code></pre>
</div>

The `MD5` class has all the code needed to compute the `MD5` hash code of a string. Pharo has a lot of utilities to ease the life of developers.

Ok. Now we need a class to store users informations. We call it `PnUserDAO` and we will use a `Dictionary` to store users informations: the key will be the email and the value the `PnUser` object.

Let&#8217;s start with the class definition:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">Object&lt;/span>&lt;span class="k"> subclass: &lt;/span>&lt;span class="ss">#PnUserDAO&lt;/span>
&lt;span class="k">    instanceVariableNames: &lt;/span>&lt;span class="s">'usersDictionary'&lt;/span>
&lt;span class="k">    classVariableNames: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    poolDictionaries: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    category: &lt;/span>&lt;span class="s">'LeonardoBlog'&lt;/span>&lt;span class="k">!&lt;/span></code></pre>
</div>

When we initialize a DAO we create a new `Dictionary` where we will store the users:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'initialization'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="bp">super&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">usersDictionary&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">Dictionary&lt;/span> &lt;span class="nb">new&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

The first thing we need is a method to store a new user:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">register:&lt;/span>&lt;span class="nv">anUser&lt;/span>
    &lt;span class="nv">usersDictionary&lt;/span> &lt;span class="nf">at:&lt;/span> &lt;span class="nv">anUser&lt;/span> &lt;span class="nf">email&lt;/span> &lt;span class="nf">put:&lt;/span> &lt;span class="nv">anUser&lt;/span></code></pre>
</div>

Then we need a method to retrieve an user from the email:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">userForEmail:&lt;/span> &lt;span class="nv">anEmail&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">usersDictionary&lt;/span> &lt;span class="nf">at:&lt;/span> &lt;span class="nv">anEmail&lt;/span> &lt;span class="nf">ifAbsent:&lt;/span> &lt;span class="bp">nil&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

We created a DAO and now we need to create a test case, just to see if everything is working. Before every test we create a new DAO with a test user and after the test we delete the DAO just created:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">TestCase&lt;/span>&lt;span class="k"> subclass: &lt;/span>&lt;span class="ss">#PnUserDAOTest&lt;/span>
&lt;span class="k">    instanceVariableNames: &lt;/span>&lt;span class="s">'dao'&lt;/span>
&lt;span class="k">    classVariableNames: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    poolDictionaries: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    category: &lt;/span>&lt;span class="s">'LeonardoBlog-Tests'&lt;/span>&lt;span class="k">!&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'running'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">setUp&lt;/span>
    &lt;span class="o">|&lt;/span>&lt;span class="nv"> testUser &lt;/span>&lt;span class="o">|&lt;/span>
    &lt;span class="bp">super&lt;/span> &lt;span class="nf">setUp&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnUserDAO&lt;/span> &lt;span class="nb">new&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">testUser&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnUser&lt;/span> &lt;span class="nb">new&lt;/span>
        &lt;span class="nf">surname:&lt;/span> &lt;span class="s">'Test user'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">realname:&lt;/span> &lt;span class="s">'Test name'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">password:&lt;/span> &lt;span class="s">'test'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">email:&lt;/span> &lt;span class="s">'test@test.eu'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">register:&lt;/span> &lt;span class="nv">testUser&lt;/span>&lt;span class="p">.&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'running'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">tearDown&lt;/span>
    &lt;span class="bp">super&lt;/span> &lt;span class="nf">tearDown&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="bp">nil&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

Now we can test the `register:` method:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'tests'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">testUserCreation&lt;/span>
    &lt;span class="o">|&lt;/span>&lt;span class="nv">u&lt;/span>&lt;span class="o">|&lt;/span>
    &lt;span class="nv">u&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnUser&lt;/span> &lt;span class="nb">new&lt;/span> &lt;span class="nf">email:&lt;/span> &lt;span class="s">'leonardo@leo.it'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">realname:&lt;/span>&lt;span class="s">'Leonardo'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">surname:&lt;/span>&lt;span class="s">'Test'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">register:&lt;/span>&lt;span class="nv">u&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span> &lt;span class="s">'leonardo@leo.it'&lt;/span>) &lt;span class="nf">isNotNil&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

The login method needs a way to check for an user and a password:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'authentication'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">userForEmail:&lt;/span> &lt;span class="nv">anEmail&lt;/span> &lt;span class="nf">password:&lt;/span>&lt;span class="nv">pwd&lt;/span>
    &lt;span class="o">|&lt;/span>&lt;span class="nv">user&lt;/span>&lt;span class="o">|&lt;/span>
    &lt;span class="nv">user&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">userForEmail:&lt;/span> &lt;span class="nv">anEmail&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">user&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="bp">nil&lt;/span> ]&lt;span class="p">.&lt;/span>
    (&lt;span class="nv">user&lt;/span> &lt;span class="nf">hasPassword:&lt;/span> &lt;span class="nv">pwd&lt;/span>) &lt;span class="nb">ifTrue:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="nv">user&lt;/span> ] &lt;span class="nb">ifFalse:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="bp">nil&lt;/span>]&lt;span class="p">.&lt;/span></code></pre>
</div>

Let&#8217;s test it:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'tests'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">testAuthentication&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span>&lt;span class="s">'test@test.eu'&lt;/span> &lt;span class="nf">password:&lt;/span>&lt;span class="s">'test'&lt;/span> ) &lt;span class="nf">isNotNil&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span>&lt;span class="s">'test@test.eu'&lt;/span> &lt;span class="nf">password:&lt;/span>&lt;span class="s">'testnot'&lt;/span> ) &lt;span class="nf">isNil&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

As we will offer an user deregistration procedure we need a `unregister:` method:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">unregister:&lt;/span> &lt;span class="nv">anEmail&lt;/span>
    &lt;span class="nv">usersDictionary&lt;/span> &lt;span class="nf">removeKey:&lt;/span> &lt;span class="nv">anEmail&lt;/span> &lt;span class="nf">ifAbsent:&lt;/span> [ ]&lt;span class="p">.&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="bp">self&lt;/span></code></pre>
</div>

We test it:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'tests'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">testRemovingUnknownUser&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">unregister:&lt;/span> &lt;span class="s">'nonexitent'&lt;/span>

&lt;span class="nf">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span> &lt;span class="nf">methodsFor:&lt;/span> &lt;span class="s">'tests'&lt;/span>&lt;span class="nf">!&lt;/span>
&lt;span class="nf">testRemovingKnownUser&lt;/span>
    &lt;span class="o">|&lt;/span>&lt;span class="nv">u&lt;/span>&lt;span class="o">|&lt;/span>
    &lt;span class="nv">u&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnUser&lt;/span> &lt;span class="nb">new&lt;/span> &lt;span class="nf">email:&lt;/span> &lt;span class="s">'leonardo@leo.it'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">realname:&lt;/span>&lt;span class="s">'Leonardo'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">surname:&lt;/span>&lt;span class="s">'Test'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">register:&lt;/span>&lt;span class="nv">u&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span> &lt;span class="s">'leonardo@leo.it'&lt;/span>) &lt;span class="nf">isNotNil&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">unregister:&lt;/span>&lt;span class="s">'leonardo@leo.it'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span> &lt;span class="s">'leonardo@leo.it'&lt;/span>) &lt;span class="nf">isNil&lt;/span>&lt;span class="p">.&lt;/span></code></pre>
</div>

Now we create a method to remove all known users:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAO&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'util'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">deleteAllUsers&lt;/span>
    &lt;span class="nv">usersDictionary&lt;/span> &lt;span class="nf">removeAll&lt;/span></code></pre>
</div>

This is the test:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnUserDAOTest&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'tests'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">testDeleteAllUsers&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span>&lt;span class="s">'test@test.eu'&lt;/span> &lt;span class="nf">password:&lt;/span>&lt;span class="s">'test'&lt;/span> ) &lt;span class="nf">isNotNil&lt;/span> &lt;span class="p">.&lt;/span>
    &lt;span class="nv">dao&lt;/span> &lt;span class="nf">deleteAllUsers&lt;/span> &lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">assert:&lt;/span>(&lt;span class="nv">dao&lt;/span> &lt;span class="nf">userForEmail:&lt;/span>&lt;span class="s">'test@test.eu'&lt;/span> &lt;span class="nf">password:&lt;/span>&lt;span class="s">'test'&lt;/span> ) &lt;span class="nf">isNil&lt;/span> &lt;span class="p">.&lt;/span></code></pre>
</div>

Well&#8230; if everything is ok you have all units test working correctly and you can be happy!

![PnUserDAO test cases](http://leonardoce.github.io/assets/pnotes-user-dao-tests.png)