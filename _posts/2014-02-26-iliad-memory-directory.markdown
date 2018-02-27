---
layout: post
title:  "Iliad Framework, serving static resources"
date:   2014-02-26 21:28:24
categories: programming smalltalk
---

In the [previous lesson][previous-post] we talked about how to use an
Iliad webapp with a browser without Javascript enabled.

In this lesson we will talk about static resources to be included in
your web-app.

When you will deploy your web application in the "real world" you want
your static resources served by a specific web server as Apache but,
while you are developing your application, you can make use of the web
server embedded inside the Pharo Smalltalk environment: Komanche.

Iliad makes really easy to serve static resources. Now we will learn
how to embed in your Pharo image [Bootstrap JS][bootstrap-js]. Let's
assume you already downloaded the compiled distribution to a file
named `bootstrap-3.1.1-dist.zip` and that you extracted it in a folder
named `~/tmp/bootstrap-3.1.1-dist`.

To include it in your Pharo image you need to derive a class from
`ILMemoryDirectory` for every Bootstrap JS subdirectory like this:

{% highlight smalltalk %}
ILMemoryDirectory subclass: #LcBootstrapCss
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'LeonardoBlog'
{% endhighlight %}

{% highlight smalltalk %}
ILMemoryDirectory subclass: #LcBootstrapFonts
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'LeonardoBlog'
{% endhighlight %}

{% highlight smalltalk %}
ILMemoryDirectory subclass: #LcBootstrapJs
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'LeonardoBlog'
{% endhighlight %}

Now we tell the classes to load all the resources contained in the
corresponding directories. Open a workspace and execute the following
commands:

{% highlight smalltalk %}
LcBootstrapCss addAllFilesIn:'/home/leonardo/tmp/bootstrap-3.1.1-dist/css'.
LcBootstrapJs addAllFilesIn:'/home/leonardo/tmp/bootstrap-3.1.1-dist/js'.
LcBootstrapFonts addAllFilesIn:'/home/leonardo/tmp/bootstrap-3.1.1-dist/fonts'.
{% endhighlight %}

You will see that a series of method are getting created under the
`files` protocol. This is were the content of Bootstrap JS will be
loaded.

Now we need to configure the subdirectory under the resources will be
published. We just need to create a `path` method under the
`accessing` protocol like this:

{% highlight smalltalk %}
"LcBootstrapJs>>path protocol accessing"
path
    ^ 'bootstrap/js'
{% endhighlight %}

{% highlight smalltalk %}
"LcBootstrapFonts>>path protocol accessing"
path
    ^ 'bootstrap/fonts'
{% endhighlight %}

{% highlight smalltalk %}
"LcBootstrapCss>>path protocol accessing"
path
    ^ 'bootstrap/css'
{% endhighlight %}

Now we need to register these new classes to be loaded by the Iliad
framework and the code to register the app must also be executed when
you load your class in a fresh image. This is the place where the
`initialize` method of the class side comes in:

{% highlight smalltalk %}
"LcBootstrapJs class>>initialize protocol initialization"
initialize
    ILFileHandler addDirectory: self new
{% endhighlight %}

{% highlight smalltalk %}
"LcBootstrapFonts class>>initialize protocol initialization"
initialize
    ILFileHandler addDirectory: self new
{% endhighlight %}

{% highlight smalltalk %}
"LcBootstrapCss class>>initialize protocol initialization"
initialize
    ILFileHandler addDirectory: self new
{% endhighlight %}

Now the initialization must be done by hand in a workspace:

{% highlight smalltalk %}
LcBootstrapJs initialize.
LcBootstrapCss initialize.
LcBootstrapFonts initialize.
{% endhighlight %}

Try using a web browser pointing to
[http://localhost:7070/bootstrap/js/bootstrap.js](http://localhost:7070/bootstrap/js/bootstrap.js)
and you will see the Bootstrap Js code. The same thing happens with
the other directories.

Check that also the following URLS get served:

- [http://localhost:7070/bootstrap/css/bootstrap.css](http://localhost:7070/bootstrap/css/bootstrap.css)
- [http://localhost:7070/bootstrap/fonts/glyphicons-halflings-regular.ttf](http://localhost:7070/bootstrap/fonts/glyphicons-halflings-regular.ttf)

Now you are sure that everything is working.

The next post is [here][next-post].

[previous-post]: {% post_url 2014-02-20-iliad-framework-lesson-six %}
[next-post]: {% post_url 2014-02-27-iliad-framework-customizing-page %}
[bootstrap-js]: http://getbootstrap.com
