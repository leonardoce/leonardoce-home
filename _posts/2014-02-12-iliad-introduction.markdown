---
layout: post
title:  "Iliad Framework, installing and starting the embedded web server"
date:   2014-02-12 21:28:24
categories: programming smalltalk
---


This post introduces the [Iliad Web Framework][iliad-web], which is a
webapp framework that you can use with the [Pharo][pharo-web]
Smalltalk implementation to create dynamic and fast websites.

I hope that you already used Pharo and the Smalltalk language but, if
you are a real beginner, try these books before reading this post.

* [Stefane Ducasse collection of free Smalltalk books][freebooks-ducasse]
* [Pharo by example][pharo-by-example]
* [Deep into Pharo][deep-into-pharo]

You will find that Smalltalk is a really enjoyable programming
language and also worth learning because it changes the way you think
about OOP.

## How to get Pharo

Getting Pharo installed is really simple. Go to the
[Pharo website][pharo-web] and download the latest release. I find the
beta release (3.0) pretty solid and I advice you to download the
latest installer.

"Installer" is a misnomer regarding Pharo as it's sufficient to
download the zip file for your platform and uncompress in a folder to
get Pharo installed. When you have unzipped the contents of the Pharo
distribution you can execute the `pharo` script (or EXE file) in the
main directory.

## How to load Iliad in your image

When you have started the Pharo GUI click on empty space on the screen
and the "World" menu will appear:

![Pharo World Menu]({{ site.url }}/assets/pharo-world-menu.png)

from the menu choose "Tools" then "Configuration browser". In the
configuration browser window search "Iliad":

![Configuration browser, Iliad]({{ site.url }}/assets/pharo-config-browser-iliad.png)

now click on "Install Stable Version" and wait until the Iliad
Framework get loaded. While you wait admire the "Metacello" project
manager at work: it's collecting all the needed packages, downloading
from the Internet and compiling the code.

Congratulations! Now you are ready to develop with the Iliad
framework.

## How to start the embedded web server

Open a workspace from the "World Menu" and enter:

{% highlight smalltalk %}
IliadKom startOn: 7070
{% endhighlight %}

select the code line and open the contextual menu right-clicking on
the selected text. From the contextual menu choose "Do it". Pharo
doesn't speek to much so you may think that nothing happened but you
can open the "Browse" application that is embedded with the Iliad
Framework pointing your browser [here][browse-link].

## Ending

You loaded the Iliad web framework in your image and started the
embedded web server. In the next post we will build a new web
application.

The next article is [here][next-article]

[iliad-web]: http://www.iliadproject.org/
[pharo-web]: http://www.pharo-project.org/
[freebooks-ducasse]: http://stephane.ducasse.free.fr/FreeBooks.html
[pharo-by-example]: http://pharobyexample.org/
[deep-into-pharo]: http://www.deepintopharo.com/
[browse-link]: http://localhost:7070/browse
[next-article]: {% post_url 2014-02-13-iliad-lesson-one %}
