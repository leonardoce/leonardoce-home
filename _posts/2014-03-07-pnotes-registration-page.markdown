---
layout: post
title:  "Iliad Framework, the registration form"
date:   2014-03-07 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we talked about checking emails
addresses, just to show how we can use regexps in Pharo.

In this post we are returning to the Iliad Framework and we will be a
registration form.

We start, as we have done for the login form, creating a component for
the registration form. This component will then be integrated into the
application and another controller will be build.

{% highlight smalltalk %}
ILWidget subclass: #PnCreateUser
	instanceVariableNames: 'email password password2 name surname errors'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'!
{% endhighlight %}

As you can see the instance variable of this component, like what was
happening in the registration form, are derived from the variable
state of the component. We created an instance variable for every data
that the user will enter and a `errors` fields that will be empty whenxb
there are no errors.

The initialize method clear all the instance variables:

{% highlight smalltalk %}
!PnCreateUser methodsFor: 'initialization'!
initialize
	super initialize.
	name := ''.
	surname := ''.
	password := ''.
	password2 := ''.
	email := ''.
	errors := ''.! !
{% endhighlight %}

As you saw in the login form when you are programming in Smalltalk you
usually create short methods, more short than the method you will be
used to. This is an example:


{% highlight smalltalk %}
!PnCreateUser methodsFor: 'building'!
contents
	^ [ :e | 
	e h1 text: 'Register your account'.
	e p
		text:
			'You will use your email to login'.
	e form
		build: [ :form | 
			self buildEmailRow: form.
			
			self buildNameRow: form.
			self buildSurnameRow: form.
			
			self buildPasswordRow: form.
			self buildRepeatPasswordRow: form.
			
			self buildErrorMessageRow: form.
			
			form button
				class: 'btn btn-default';
				text: 'Register to project notes';
				action: [ self registerAction ] ] ]! !

!PnCreateUser methodsFor: 'building'!
buildErrorMessageRow: aForm
	errors
		ifNotEmpty: [ (aForm div class: 'alert alert-warning') ul
		build: [ :box | (errors findTokens: String cr) do: [ :msg | box li text: msg ] ] ]! !

!PnCreateUser methodsFor: 'building'!
buildPasswordRow: aForm
	aForm div class: (errors ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Password: '.
				row input class:'form-control';  type: 'password'; action: [ :text | password := text ]; value:'' ]! !

!PnCreateUser methodsFor: 'building'!
buildEmailRow: aForm
	aForm div class: (errors ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Email'.
				row input class:'form-control'; action: [ :text | email:= text ]; value:email]! !

!PnCreateUser methodsFor: 'building'!
buildSurnameRow: aForm
	aForm div class: (errors ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Surname'.
				row input class:'form-control'; action: [ :text | surname:= text ]; value:email]! !

!PnCreateUser methodsFor: 'building'!
buildRepeatPasswordRow: aForm
	aForm div class: (errors ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Repeat Password: '.
				row input class:'form-control'; type: 'password'; action: [ :text | password2 := text ]; value:'']! !

!PnCreateUser methodsFor: 'building'!
buildNameRow: aForm
	aForm div class: (errors ifNotNil: [ 'form-group has-error' ] ifNil:  [ 'form-group' ]);
			build: [ : row | 
				row label text:'Name'.
				row input class:'form-control'; action: [ :text | name:= text ]; value:email]! !
{% endhighlight %}

In the previous example there is nothing interesting: just creating a
form like the login form but with more fields. This is the error
checking method that we will be calling before the registration action:

{% highlight smalltalk %}
!PnCreateUser methodsFor: 'actions'!
checkErrors
	errors := ''.
	email ifEmpty: [ errors := errors, 'The email must not be empty because you need to login in the system', String cr ].
	(PnUtils checkEmail: email) ifFalse: [ errors := errors, 'This doesn''t look like a real email to me', String cr ].
	name ifEmpty: [ errors := errors, 'The name is a required field', String cr ].
	surname ifEmpty: [ errors := errors, 'The surname is a required field', String cr ].
	(password, password2) ifEmpty: [ errors := errors, 'You must enter a password', String cr ].
	(password = password2) ifFalse: [ errors := errors, 'The password are not matching', String cr ].
	! !
{% endhighlight %}

As you see I only fill the `errors` instance variable with the error
message and the row building methods will do the rest.

Now the registration action, that is more interesting:

{% highlight smalltalk %}
!PnCreateUser methodsFor: 'actions'!
registerAction
	| u |
	self markDirty; checkErrors.
	errors ifNotEmpty: [ ^ self. ].
	
	u := PnUser new realname:name; surname: surname; email: email; password: password.
	PnUserDAO current register:u.
	self show:(ILInformationWidget new informationString: 'We created an user for you. Click ok to go to the app.') 
		onAnswer: [ : e | self redirectToLocal: 'notes' ].! !
{% endhighlight %}

The interesting bits are in the DAO usage, that "persist" the `PnUser`
object, and the `show:onAnswer:` method.

The `show:onAnswer:` method of the `ILWidget` class can handle the
control flow for you. In the next page rendering process the invoking
widget will be substituted with the passed widget, in this case
`ILInformationWidget`. When the shown widget (`ILInformationWidget`)
will call the `answer` method the control flow will return to the
block passed to `show:onAnswer:`.

This block redirect the browser to the `notes` controller, which will
handle the notes creation and visualization proces.

The next post is [here][next-post].

[previous-post]: {% post_url 2014-03-04-checking-emails %}
[next-post]: {% post_url 2014-03-09-pnotes-registration-page-2 %}
