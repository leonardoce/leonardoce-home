---
layout: post
title:  "Iliad Framework, how to customize the page generation"
date:   2014-02-27 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we talked about how to include
static resources in the Pharo image to be server with our application.

Let's start with a new web application and, as we have seen in the
[relative post][post-about-webapp], we must create a class deriving
from `ILApplication`, write the `path` method and an empty `index`
controller:

{% highlight smalltalk %}
ILApplication subclass: #LcBlogProjectNotes
    instanceVariableNames: ''
    classVariableNames: ''
    category: 'LeonardoBlog'
{% endhighlight %}

{% highlight smalltalk %}
"LcApplication class>>path protocol accessing"
path
    ^ 'ProjectNotes'
{% endhighlight %}

{% highlight smalltalk %}
"LcApplication>>index protocol controllers"
index
    ^ [ :e | e h1 text:'Hi!' ]
{% endhighlight %}

Remember to put the `index` method in the protocol `controllers`!

Now we must integrate all the bootstrap code from the static resources
generated in the [previous post][previous-post] in the page.

Do to that we must override the `updatePage:` method from
`ILApplication` like this:

{% highlight smalltalk %}
"LcApplication>>updatePage protocol updating"
updatePage: aPage
    "Includes bootstrap JS"
    
    aPage head link
      rel: 'stylesheet';
      href: '/bootstrap/css/bootstrap.min.css'.
    aPage head link
      rel: 'stylesheet';
      href: '/bootstrap/css/bootstrap-theme.min.css'.
    aPage head javascript src: '/bootstrap/js/bootstrap.min.js'.
    aPage head meta
      httpEquiv: 'X-UA-Compatible';
      content: 'IE=edge'.
    aPage head meta
      name: 'viewport';
	  content: 'width=device-width, initial-scale=1'.
    
    "We want a title for our app"
    aPage head title: 'A project note-taking app'
{% endhighlight %}

The `updatePage:` method get called when the page has been constructed
and before its contents are sent to the browser. In this method you
can customize the generated page as you want.

Voila'! We have integrated Bootstrap JS. 

The next post is [here][next-post].

[previous-post]: {% post_url 2014-02-26-iliad-memory-directory %}
[next-post]: {% post_url 2014-03-02-iliad-login-page %}
[post-about-webapp]: {% post_url 2014-02-13-iliad-lesson-one %}
