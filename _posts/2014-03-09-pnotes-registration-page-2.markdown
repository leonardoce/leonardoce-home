---
layout: post
title:  "Iliad Framework, the registration form (part 2)"
date:   2014-03-09 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we build a registration form and
in this one we will attach it to our application.

We start adding an instance variable to our application to contain the
registration widget and another instance variable to contain the
current user:

{% highlight smalltalk %}
ILApplication subclass: #LcBlogProjectNotes
	instanceVariableNames: 'loginWidget registrationWidget currentuser' 
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
{% endhighlight %}

As we have done for the `loginWidget`, we create an accessor method
that will construct the widget if it hasn't been instanciated:

{% highlight smalltalk %}
!LcBlogProjectNotes methodsFor: 'accessing'!
registrationWidget
	^ registrationWidget ifNil: [ registrationWidget := PnCreateUser new ]
{% endhighlight %}

We also create accessors for the current user instance variable:

{% highlight smalltalk %}
!LcBlogProjectNotes methodsFor: 'accessing'!
currentuser: anObject
	currentuser := anObject

!LcBlogProjectNotes methodsFor: 'accessing'!
currentuser
	^ currentuser
{% endhighlight %}

Now we create a controller for the registration page:

{% highlight smalltalk %}
!LcBlogProjectNotes methodsFor: 'controllers'!
register
	^ [ :e | e div class:'container'; build: self registrationWidget ]
{% endhighlight %}

Now we have our new controller and we can test it from the login
page. The controller name, `register`, match with the `href` in the
login page.

[previous-post]: {% post_url 2014-03-07-pnotes-registration-page %}
[next-post]: {% post_url 2014-03-13-pnotes-status-widget %}

