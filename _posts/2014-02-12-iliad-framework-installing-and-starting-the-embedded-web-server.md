---
id: 31
title: Iliad Framework, installing and starting the embedded web server
date: 2014-02-12T18:30:51+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=31
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
This post introduces the [Iliad Web Framework](http://www.iliadproject.org/), which is a webapp framework that you can use with the [Pharo](http://www.pharo-project.org/) Smalltalk implementation to create dynamic and fast websites.

I hope that you already used Pharo and the Smalltalk language but, if you are a real beginner, try these books before reading this post.

  * [Stefane Ducasse collection of free Smalltalk books](http://stephane.ducasse.free.fr/FreeBooks.html)
  * [Pharo by example](http://pharobyexample.org/)
  * [Deep into Pharo](http://www.deepintopharo.com/)

You will find that Smalltalk is a really enjoyable programming language and also worth learning because it changes the way you think about OOP.

## How to get Pharo

Getting Pharo installed is really simple. Go to the [Pharo website](http://www.pharo-project.org/) and download the latest release. I find the beta release (3.0) pretty solid and I advice you to download the latest installer.

&#8220;Installer&#8221; is a misnomer regarding Pharo as it&#8217;s sufficient to download the zip file for your platform and uncompress in a folder to get Pharo installed. When you have unzipped the contents of the Pharo distribution you can execute the `pharo` script (or EXE file) in the main directory.

## How to load Iliad in your image

When you have started the Pharo GUI click on empty space on the screen and the &#8220;World&#8221; menu will appear:

![Pharo World Menu](http://leonardoce.github.io/assets/pharo-world-menu.png)

from the menu choose &#8220;Tools&#8221; then &#8220;Configuration browser&#8221;. In the configuration browser window search &#8220;Iliad&#8221;:

![Configuration browser, Iliad](http://leonardoce.github.io/assets/pharo-config-browser-iliad.png)

now click on &#8220;Install Stable Version&#8221; and wait until the Iliad Framework get loaded. While you wait admire the &#8220;Metacello&#8221; project manager at work: it&#8217;s collecting all the needed packages, downloading from the Internet and compiling the code.

Congratulations! Now you are ready to develop with the Iliad framework.

## How to start the embedded web server

Open a workspace from the &#8220;World Menu&#8221; and enter:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">IliadKom&lt;/span> &lt;span class="nf">startOn:&lt;/span> &lt;span class="m">7070&lt;/span></code></pre>
</div>

select the code line and open the contextual menu right-clicking on the selected text. From the contextual menu choose &#8220;Do it&#8221;. Pharo doesn&#8217;t speek to much so you may think that nothing happened but you can open the &#8220;Browse&#8221; application that is embedded with the Iliad Framework pointing your browser [here](http://localhost:7070/browse).

## Ending

You loaded the Iliad web framework in your image and started the embedded web server. In the next post we will build a new web application.