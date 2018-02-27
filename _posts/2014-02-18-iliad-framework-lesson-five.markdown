---
layout: post
title:  "Iliad Framework, how events are implemented"
date:   2014-02-18 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we implemented the counter
example. Now I would like to talk about how the event's black magic
actually works from the browser point of view.

If you are curious like me you have opened the browser source view
just to see how the generated HTML looks like and you have found
something like this:

{% highlight html %}
<html lang="en" xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
<script type="text/javascript" src="/javascripts/jquery-1.4.4.min.js"> </script>
<script type="text/javascript" src="/javascripts/no_conflict.js"> </script>
<script type="text/javascript" src="/javascripts/iliad.js"> </script>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
</head>
<body>
<p>Hi! Widget example</p>
<div class="45926">
<p>0</p>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);">++</a>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);">--</a>
</div>
</body>
</html>
{% endhighlight %}

Our widget has been wrapped in a `div` tag that have, in my example,
the class `45926`. We have created two actions and these actions have
been written as two Javascript calls with two parameters:

{% highlight html %}
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45931&amp;_state=71db1789&quot;);">++</a>
<a href="javascript:{}" onclick="iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45932&amp;_state=71db1789&quot;);">--</a>
{% endhighlight %}

The parameter "action" is different because, on the server, two
different actions have to be invoked, but the "state" variable has the
same value. In effect, when we have generated the page, the state was
the same between the two links.

Now I will click on the `++` link and capture the Ajax request and the
server response for you:

    GET http://localhost:7070/leonardoBlog/widgetExample?_action=45927&_state=71db1789
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
		  "<div class=\"45926\"><p>1</p><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45934&amp;_state=71db1789&quot;);\">++</a><a href=\"javascript:{}\" onclick=\"iliad.evaluateAction(&quot;/leonardoBlog/widgetExample?_action=45935&amp;_state=71db1789&quot;);\">--</a></div>"
	    }
	}

Now the black magic is explained by itself: the page on the browser
has made an Ajax request to the server telling him that it has to
execute the action with the code `45927` on the state `71db1789` and
the server response tell to the client that he must replace the HTML
content of the widget `45926` with the new content. The server knows
that which widgets must be redrawn using the `markDirty` method.

The meaning of the `action` and of the `class` fields should now be
clear to you.

The `state` parameter, as you may see, doesn't change between action
invokations. Its role is managing multiple windows for the same
session, for example when you have multiple tabs opened for the same
session. When you have multiple tabs opened for the same session every
tab has a different value for the `state` parameter.

The next article is [here][next-post].

[previous-post]: {% post_url 2014-02-17-iliad-framework-lesson-four %}
[next-post]:  {% post_url 2014-02-20-iliad-framework-lesson-six %}
