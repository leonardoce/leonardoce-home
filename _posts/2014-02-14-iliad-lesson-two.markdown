---
layout: post
title:  "Iliad Framework, on controllers"
date:   2014-02-14 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] about the Iliad Framework we
build a basic web application that, in the default controller, write
an "Hello World" HTML.

Now we will try to clarify how the Front Controller framework
works. Every controller (remember that it must be in the `controllers`
protocol) is mapped to a "page" of your application.

These things are best explained with an example. A controlled named
`firstTest` in an application whose `path` method returns
`leonardoBlog` will be mapped to the URL `leonardoBlog/firstTest`.

In the same way the controller `secondTest` will we mapped to
`leonardoBlog/secondTest`. The only exception is the `index`
controller that being the default controller is mapped to the
application URL: `leonardoBlog`. Enough said. Let's add some code to
our application.

We create another controller named `firstTest` like this:

{% highlight smalltalk %}
"LcBlogHelloIliad>>firstTest (protocol controllers)"
firstTest
    ^ [ :e | e h1: 'This in the first test'.
    e a linkToLocal: 'index'; text: 'Go to the index controller' ]
{% endhighlight %}

Now if we go to
[http://localhost:7070/leonardoBlog/firstTest](http://localhost:7070/leonardoBlog/firstTest)
we should see the result of the new controller.

We also created a link to the index controller using the method
`linkToLocal:` of the `ILLinkableElement` class. Note that we haven't
specified the URL of the application. If you will need to change the
url of the application you will not have to remember when you have put
the application name.

We can also modify the original `index` controller to include a link
to the `firstTest` page like this:

{% highlight smalltalk %}
"LcBlogHelloIliad>>index (protocol controllers)"
index
    ^ [ :e | e h1: 'Hi world!'.
    e a linkToLocal: 'firstTest'; text: 'Go to firstTest controller' ]
{% endhighlight %}

The next post is [here][next-post].

[previous-post]: {% post_url 2014-02-13-iliad-lesson-one %}
[next-post]:  {% post_url 2014-02-16-iliad-lesson-three %}
