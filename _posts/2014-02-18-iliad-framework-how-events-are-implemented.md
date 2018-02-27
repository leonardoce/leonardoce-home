---
id: 41
title: Iliad Framework, how events are implemented
date: 2014-02-18T18:43:44+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=41
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
n the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/02/17/iliad-framework-lesson-four.html) we implemented the counter example. Now I would like to talk about how the event&#8217;s black magic actually works from the browser point of view.

If you are curious like me you have opened the browser source view just to see how the generated HTML looks like and you have found something like this:

<div class="highlight">
  <pre><code class="language-html">&lt;span class="nt">&lt;html&lt;/span> &lt;span class="na">lang=&lt;/span>&lt;span class="s">"en"&lt;/span> &lt;span class="na">xmlns=&lt;/span>&lt;span class="s">"http://www.w3.org/1999/xhtml"&lt;/span> &lt;span class="na">xml:lang=&lt;/span>&lt;span class="s">"en"&lt;/span>&lt;span class="nt">&gt;&lt;/span>
&lt;span class="nt">&lt;head&gt;&lt;/span>
&lt;span class="nt">&lt;script &lt;/span>&lt;span class="na">type=&lt;/span>&lt;span class="s">"text/javascript"&lt;/span> &lt;span class="na">src=&lt;/span>&lt;span class="s">"/javascripts/jquery-1.4.4.min.js"&lt;/span>&lt;span class="nt">&gt;&lt;/span> &lt;span class="nt">&lt;/script&gt;&lt;/span>
&lt;span class="nt">&lt;script &lt;/span>&lt;span class="na">type=&lt;/span>&lt;span class="s">"text/javascript"&lt;/span> &lt;span class="na">src=&lt;/span>&lt;span class="s">"/javascripts/no_conflict.js"&lt;/span>&lt;span class="nt">&gt;&lt;/span> &lt;span class="nt">&lt;/script&gt;&lt;/span>
&lt;span class="nt">&lt;script &lt;/span>&lt;span class="na">type=&lt;/span>&lt;span class="s">"text/javascript"&lt;/span> &lt;span class="na">src=&lt;/span>&lt;span class="s">"/javascripts/iliad.js"&lt;/span>&lt;span class="nt">&gt;&lt;/span> &lt;span class="nt">&lt;/script&gt;&lt;/span>
&lt;span class="nt">&lt;meta&lt;/span> &lt;span class="na">http-equiv=&lt;/span>&lt;span class="s">"Content-Type"&lt;/span> &lt;span class="na">content=&lt;/span>&lt;span class="s">"text/html; charset=utf-8"&lt;/span>&lt;span class="nt">/&gt;&lt;/span>
&lt;span class="nt">&lt;/head&gt;&lt;/span>
&lt;span class="nt">&lt;body&gt;&lt;/span>
&lt;span class="nt">&lt;p&gt;&lt;/span>Hi! Widget example&lt;span class="nt">&lt;/p&gt;&lt;/span>
&lt;span class="nt">&lt;div&lt;/span> &lt;span class="na">class=&lt;/span>&lt;span class="s">"45926"&lt;/span>&lt;span class="nt">&gt;&lt;/span>
&lt;span class="nt">&lt;p&gt;&lt;/span>0&lt;span class="nt">&lt;/p&gt;&lt;/span>
&lt;span class="nt">&lt;a&lt;/span> &lt;span class="na">href=&lt;/span>&lt;span class="s">"javascript:{}"&lt;/span> &lt;span class="na">onclick=&lt;/span>&lt;span class="s">"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);"&lt;/span>&lt;span class="nt">&gt;&lt;/span>++&lt;span class="nt">&lt;/a&gt;&lt;/span>
&lt;span class="nt">&lt;a&lt;/span> &lt;span class="na">href=&lt;/span>&lt;span class="s">"javascript:{}"&lt;/span> &lt;span class="na">onclick=&lt;/span>&lt;span class="s">"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);"&lt;/span>&lt;span class="nt">&gt;&lt;/span>--&lt;span class="nt">&lt;/a&gt;&lt;/span>
&lt;span class="nt">&lt;/div&gt;&lt;/span>
&lt;span class="nt">&lt;/body&gt;&lt;/span>
&lt;span class="nt">&lt;/html&gt;&lt;/span></code></pre>
</div>

Our widget has been wrapped in a `div` tag that have, in my example, the class `45926`. We have created two actions and these actions have been written as two Javascript calls with two parameters:

<div class="highlight">
  <pre><code class="language-html">&lt;span class="nt">&lt;a&lt;/span> &lt;span class="na">href=&lt;/span>&lt;span class="s">"javascript:{}"&lt;/span> &lt;span class="na">onclick=&lt;/span>&lt;span class="s">"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);"&lt;/span>&lt;span class="nt">&gt;&lt;/span>++&lt;span class="nt">&lt;/a&gt;&lt;/span>
&lt;span class="nt">&lt;a&lt;/span> &lt;span class="na">href=&lt;/span>&lt;span class="s">"javascript:{}"&lt;/span> &lt;span class="na">onclick=&lt;/span>&lt;span class="s">"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);"&lt;/span>&lt;span class="nt">&gt;&lt;/span>--&lt;span class="nt">&lt;/a&gt;&lt;/span></code></pre>
</div>

The parameter &#8220;action&#8221; is different because, on the server, two different actions have to be invoked, but the &#8220;state&#8221; variable has the same value. In effect, when we have generated the page, the state was the same between the two links.

Now I will click on the `++` link and capture the Ajax request and the server response for you:

<div class="highlight">
  <pre><code class="language-text">GET http://localhost:7070/leonardoBlog/widgetExample?_action=45927&_state=71db1789
Accept:application/json, text/javascript, */*; q=0.01
Accept-Encoding:gzip,deflate,sdch
Accept-Language:en-US,en;q=0.8,it;q=0.6
Connection:keep-alive
Cookie:_iliad685744=587b7bac-82f8-4e95-84bc-7a39b13aa458
Host:localhost:7070
Referer:http://localhost:7070/leonardoBlog/widgetExample
X-Requested-With:XMLHttpRequest

{
"head": [],
"widgets":
    {
      "45926":
      "&lt;div class=\"45926\"&gt;&lt;p&gt;1&lt;/p&gt;&lt;a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45934&amp;_state=71db1789&quot;);\"&gt;++&lt;/a&gt;&lt;a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45935&amp;_state=71db1789&quot;);\"&gt;--&lt;/a&gt;&lt;/div&gt;"
    }
}
</code></pre>
</div>

Now the black magic is explained by itself: the page on the browser has made an Ajax request to the server telling him that it has to execute the action with the code `45927` on the state `71db1789` and the server response tell to the client that he must replace the HTML content of the widget `45926` with the new content. The server knows that which widgets must be redrawn using the `markDirty` method.

The meaning of the `action` and of the `class` fields should now be clear to you.

The `state` parameter, as you may see, doesn&#8217;t change between action invokations. Its role is managing multiple windows for the same session, for example when you have multiple tabs opened for the same session. When you have multiple tabs opened for the same session every tab has a different value for the `state` parameter.