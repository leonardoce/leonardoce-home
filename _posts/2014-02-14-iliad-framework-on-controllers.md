---
id: 35
title: Iliad Framework, on controllers
date: 2014-02-14T18:39:21+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=35
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/13/iliad-lesson-one.html) about the Iliad Framework we build a basic web application that, in the default controller, write an &#8220;Hello World&#8221; HTML.

Now we will try to clarify how the Front Controller framework works. Every controller (remember that it must be in the `controllers` protocol) is mapped to a &#8220;page&#8221; of your application.

These things are best explained with an example. A controlled named `firstTest` in an application whose `path` method returns `leonardoBlog` will be mapped to the URL `leonardoBlog/firstTest`.

In the same way the controller `secondTest` will we mapped to `leonardoBlog/secondTest`. The only exception is the `index` controller that being the default controller is mapped to the application URL: `leonardoBlog`. Enough said. Let&#8217;s add some code to our application.

We create another controller named `firstTest` like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad&gt;&gt;firstTest (protocol controllers)"&lt;/span>
&lt;span class="nf">firstTest&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1:&lt;/span> &lt;span class="s">'This in the first test'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">a&lt;/span> &lt;span class="nf">linkToLocal:&lt;/span> &lt;span class="s">'index'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Go to the index controller'&lt;/span> ]</code></pre>
</div>

Now if we go to <http://localhost:7070/leonardoBlog/firstTest> we should see the result of the new controller.

We also created a link to the index controller using the method `linkToLocal:` of the `ILLinkableElement` class. Note that we haven&#8217;t specified the URL of the application. If you will need to change the url of the application you will not have to remember when you have put the application name.

We can also modify the original `index` controller to include a link to the `firstTest` page like this:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="c">"LcBlogHelloIliad&gt;&gt;index (protocol controllers)"&lt;/span>
&lt;span class="nf">index&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1:&lt;/span> &lt;span class="s">'Hi world!'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">a&lt;/span> &lt;span class="nf">linkToLocal:&lt;/span> &lt;span class="s">'firstTest'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Go to firstTest controller'&lt;/span> ]</code></pre>
</div>