---
layout: post
title:  "Pharo, Checking emails with regexp"
date:   2014-03-04 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we build a login form and we saw
that we should provide an user registration form.

We are going to build a registration form so we need to check
emails. I will create a new class `PnUtils` to contain all the
string-checking utilities that we need for our App:

{% highlight smalltalk %}
Object subclass: #PnUtils
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'
{% endhighlight %}

Now, in a class method, I will implement my email-checking:

{% highlight smalltalk %}
!PnUtils class methodsFor: 'as yet unclassified'!
checkEmail: anEmail
	^ anEmail asUppercase matchesRegex:  '[A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z][A-Z][A-Z]?[A-Z]?'.
{% endhighlight %}

As you see I check the email addresses using the RegExp feature of
Smalltalk. The regexp is taken from [this site][email-regexp-site].

I know that at this times my should send an activation email to let
the user activate yourself but I won't do it as I don't want to mess
up with SMTP configuration.

In this discussion I want to show you how simple is to use regexp from
Pharo Smalltalk, without loading any external libraries.

Ok. We must test it to see if it's working or not:

{% highlight smalltalk %}
TestCase subclass: #PnUtilsTest
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog-Tests'

!PnUtilsTest methodsFor: 'tests'!
testEmailOk
	self assert: (PnUtils checkEmail: 'leonardoce@interfree.it' ).
	self assert: (PnUtils checkEmail: 'leonardoce@interfree.com' ).	

!PnUtilsTest methodsFor: 'tests'!
testEmailNotOk
	self assert: (PnUtils checkEmail: 'leonardoce@interfree.sirtr' ) not.
{% endhighlight %}

The tests should all be working.

The next post is [here][next-post].

[previous-post]: {% post_url 2014-03-03-users-dao-intermezzo %}
[next-post]: {% post_url 2014-03-07-pnotes-registration-page %}
[email-regexp-site]: http://www.regular-expressions.info/index.html
