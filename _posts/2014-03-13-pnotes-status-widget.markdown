---
layout: post
title:  "Iliad Framework, a status widget for displaying the current user"
date:   2014-03-13 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we completed the users
registration form. Now we can use the login form.

To show the current user in the next pages we will implement a current
user heading as a widget:

{% highlight smalltalk %}
ILWidget subclass: #PnCurrentUserHeading
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
{% endhighlight %}

In the contents of this widget we will use the `application` method of
the `ILWidget` class to access the current application and the current
user:

{% highlight smalltalk %}
!PnCurrentUserHeading methodsFor: 'building'!
contents
	^ [ :e | 
	e div
		cssClass: 'navbar navbar-static-top bs-docs-nav';
		build: [ :header | 
					header div
						class: 'navbar-brand';
						text: 'Project Notes for ' , self application currentuser email.
					(header ul
						cssClass: 'nav navbar-nav';
						li)
						build: [ :logout | 
							logout a
								text: 'Logout';
								action: [ self logout ] ] ] ]
{% endhighlight %}

We also included a `logout` action that reset the current user and
redirect the application to the login page:

{% highlight smalltalk %}
!PnCurrentUserHeading methodsFor: 'actions'!
logout
	self application currentuser:nil.
	self redirectToLocal: 'login'.
{% endhighlight %}

We include this widget in the application class like we have done
before with the login and the registration page:

{% highlight smalltalk %}
ILApplication subclass: #LcBlogProjectNotes
	instanceVariableNames: 'loginWidget registrationWidget currentuser currentUserWidget'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'

!LcBlogProjectNotes methodsFor: 'accessing'!
currentUserWidget
	^ currentUserWidget ifNil: [ currentUserWidget := PnCurrentUserHeading new ]
{% endhighlight %}

Now we create a new `notes` controller:

{% highlight smalltalk %}
!LcBlogProjectNotes methodsFor: 'controllers'!
notes
	^ [ :e | e build:(self currentUserWidget). ]
{% endhighlight %}


[previous-post]: {% post_url 2014-03-09-pnotes-registration-page-2 %}
