---
layout: post
title:  "Iliad Framework, what if I don't have Javascript?"
date:   2014-02-20 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we talked about how events are
implemented, and you saw that actions are implemented with Javascript
code.

Really? You want to keep Javascript off your browsing experience?
Firefox, in the new releases, even don't have a setting to disable
Javascript without installing a plugin.

The iliad framework can also degrade to a non ajax version of your
application without loosing any functionality. To enable this
behaviour you must evaluate this code in a new workspace:

{% highlight smalltalk %}
ILAnchorElement useAjaxOnlyForActions: nil
{% endhighlight %}

Now disable Javascript in your browser and see your application
working application with Javascript disabled. This is part of the Iliad
goodness!

The next post is [here][next-post]

[previous-post]: {% post_url 2014-02-18-iliad-framework-lesson-five %}
[next-post]: {% post_url 2014-02-26-iliad-memory-directory %}
