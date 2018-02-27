---
layout: post
title:  "Iliad Framework, index page"
date:   2014-03-30 21:28:24
categories: programming smalltalk
---


In this post we will create a page to display the notes that we created
with the project notes app. We will use the widget that we have created
in the [previous lesson][previous-post] to display the current user and
we will create a form to search inside notes.

# Creating the PnViewNotes widget

Let's start by creating a new widget that will be the content of the new
page:

{% highlight smalltalk %}
ILWidget subclass: #PnViewNotes
	instanceVariableNames: 'searchstring'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
{% endhighlight %}

We created an instance variable, `searchString`, to remember the search
string that will used to search inside the notes.

# The rendering methods of the PnViewNotes widget

The interesting bits of this example are inside the rendering methods:

{% highlight smalltalk %}
contents
!PnViewNotes methodsFor: 'building'!
	^ [ :e | e h1 text:'Project notes'.
		e p text:'Use this application as a scratch pad. You can 
		attach to every page a series of tags to ease searching.'. 
		
		e build:self searchForm.
		e build:self searchResults ]
{% endhighlight %}

Let's present the search form:

{% highlight smalltalk %}
!PnViewNotes methodsFor: 'building'!
searchForm
	^ [ :e | e form build:[ :form |
			form div class:'input-group'; build:[:row |
				row a class:'glyphicon glyphicon-plus input-group-addon'; action:[self addNewNote].
				row input class:'form-control'; beSubmitOnChange; 
					attributeAt:'placeholder' put:'Search string'; 
					action:[ :value | searchstring := value. self doSearch ].
				 ] ] ]
{% endhighlight %}

As you see we have used the BootstrapJs CSS declarations with the 
`class:` method of elements. When our element doesn't support an
attribute, like the `placeholder` attribute and the `ILInputElement` objects`, 
we can manually place attributes with the method `attributeAt:put:`.

Another interesting bit is the `beSubmitOnChange` method, that will
cause the `ILInputElement` to call the server every time the user 
changes the text field content, without waiting for the form to
be submitted.

Every time the search string is modified we place the new content
in the `searchstring` instance variable and we invoke the `doSearch`
method to search inside the notes.

As we haven't already implemented the notes DAO we delay the discussion
of the `doSearch` method, that we implement like this:

{% highlight smalltalk %}
!PnViewNotes methodsFor: 'actions'!
doSearch
{% endhighlight %}

The same thing happens with the `addNewNote` method:

{% highlight smalltalk %}
!PnViewNotes methodsFor: 'actions'!
addNewNote
{% endhighlight %}

Until we haven'n implemented the `PnNote` object and its DAO we delay the
implementation of the `searchResults` method:

{% highlight smalltalk %}
!PnViewNotes methodsFor: 'building'!
searchResults
	^ [ :e | ]
{% endhighlight %}

In this lesson we left many method unimplemented, but in the following one we
implement the `PnNote` object and its DAO.

[previous-post]: {% post_url 2014-03-13-pnotes-status-widget %}
