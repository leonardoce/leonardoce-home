---
layout: post
title:  "Iliad Framework, the users-dao intermezzo"
date:   2014-03-03 21:28:24
categories: programming smalltalk
---

In the [previous post][previous-post] we build a login form. In that
form we included a "Register" link and so we must provide a way
to make our users entries persistent.

Using Smalltalk the image is already persistent, saved every time you
need reloaded from the image file. To cut a long story short if you
don't need ACID transactions and a strong mechanism to backup and
restore data you can use the image as your database.

This may look strange (and do look strange) to non-smalltalkers but
this [wonderful post][image-based-persistence] explains very well why
using your image as a persistence layer isn't a crazy idea.

For our didactic purpose we can safely use the image.

We need to store users so we start by creating an `PnUser` class:

{% highlight smalltalk %}
Object subclass: #PnUser
	instanceVariableNames: 'realname surname email md5pwd'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'!
{% endhighlight %}

As you see this is a really simple class. You can automatically
generate the accessors method using the Pharo browser. You will obtain
something like this:

{% highlight smalltalk %}
!PnUser methodsFor: 'accessing'!
email
	^ email

password:pwd
	md5pwd :=  (MD5 hashMessage:pwd) hex.

realname: anObject
	realname := anObject

email: anObject
	email := anObject

realname
	^ realname

surname: anObject
	surname := anObject

surname
	^ surname

email
	^ email

email: anObject
	email := anObject

realname
	^ realname

surname: anObject
	surname := anObject

surname
	^ surname

realname: anObject
	realname := anObject
{% endhighlight %}

Instead of storing the password we store the MD5 hash code and so we
have these methods:

{% highlight smalltalk %}
!PnUser methodsFor: 'accessing'!
password:pwd
	md5pwd :=  (MD5 hashMessage:pwd) hex.

!PnUser methodsFor: 'accessing'!
hasPassword:aString
	^ (MD5 hashMessage:aString) hex = md5pwd .
{% endhighlight %}

The `MD5` class has all the code needed to compute the `MD5` hash code
of a string. Pharo has a lot of utilities to ease the life of
developers.

Ok. Now we need a class to store users informations. We call it
`PnUserDAO` and we will use a `Dictionary` to store users
informations: the key will be the email and the value the `PnUser`
object.

Let's start with the class definition:

{% highlight smalltalk %}
Object subclass: #PnUserDAO
	instanceVariableNames: 'usersDictionary'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog'!
{% endhighlight %}

When we initialize a DAO we create a new `Dictionary` where we will
store the users:

{% highlight smalltalk %}
!PnUserDAO methodsFor: 'initialization'!
initialize
	super initialize.
	usersDictionary := Dictionary new.
{% endhighlight %}

The first thing we need is a method to store a new user:

{% highlight smalltalk %}
!PnUserDAO methodsFor: 'accessing'!
register:anUser
	usersDictionary at: anUser email put: anUser
{% endhighlight %}

Then we need a method to retrieve an user from the email:

{% highlight smalltalk %}
!PnUserDAO methodsFor: 'accessing'!
userForEmail: anEmail
	^ usersDictionary at: anEmail ifAbsent: nil.
{% endhighlight %}

We created a DAO and now we need to create a test case, just to see if
everything is working. Before every test we create a new DAO with a
test user and after the test we delete the DAO just created:

{% highlight smalltalk %}
TestCase subclass: #PnUserDAOTest
	instanceVariableNames: 'dao'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'LeonardoBlog-Tests'!

!PnUserDAOTest methodsFor: 'running'!
setUp
	| testUser |
	super setUp.
	dao := PnUserDAO new.
	testUser := PnUser new
		surname: 'Test user';
		realname: 'Test name';
		password: 'test';
		email: 'test@test.eu'.
	dao register: testUser.

!PnUserDAOTest methodsFor: 'running'!
tearDown
	super tearDown.
	dao := nil.
{% endhighlight %}

Now we can test the `register:` method:

{% highlight smalltalk %}
!PnUserDAOTest methodsFor: 'tests'!
testUserCreation
	|u|
	u := PnUser new email: 'leonardo@leo.it'; realname:'Leonardo'; surname:'Test'.
	dao register:u.
	self assert:(dao userForEmail: 'leonardo@leo.it') isNotNil.
{% endhighlight %}

The login method needs a way to check for an user and a password:

{% highlight smalltalk %}
!PnUserDAO methodsFor: 'authentication'!
userForEmail: anEmail password:pwd
	|user|
	user := self userForEmail: anEmail.
	user ifNil: [ ^ nil ].
	(user hasPassword: pwd) ifTrue: [ ^ user ] ifFalse: [ ^ nil].
{% endhighlight %}

Let's test it:

{% highlight smalltalk %}
!PnUserDAOTest methodsFor: 'tests'!
testAuthentication
	self assert:(dao userForEmail:'test@test.eu' password:'test' ) isNotNil.
	self assert:(dao userForEmail:'test@test.eu' password:'testnot' ) isNil.
{% endhighlight %}

As we will offer an user deregistration procedure we need a
`unregister:` method:
 
{% highlight smalltalk %}
!PnUserDAO methodsFor: 'accessing'!
unregister: anEmail
	usersDictionary removeKey: anEmail ifAbsent: [ ].
	^ self
{% endhighlight %}

We test it:

{% highlight smalltalk %}
!PnUserDAOTest methodsFor: 'tests'!
testRemovingUnknownUser
	dao unregister: 'nonexitent'

!PnUserDAOTest methodsFor: 'tests'!
testRemovingKnownUser
	|u|
	u := PnUser new email: 'leonardo@leo.it'; realname:'Leonardo'; surname:'Test'.
	dao register:u.
	self assert:(dao userForEmail: 'leonardo@leo.it') isNotNil.
	dao unregister:'leonardo@leo.it'.
	self assert:(dao userForEmail: 'leonardo@leo.it') isNil.
{% endhighlight %}

Now we create a method to remove all known users:

{% highlight smalltalk %}
!PnUserDAO methodsFor: 'util'!
deleteAllUsers
	usersDictionary removeAll
{% endhighlight %}

This is the test:

{% highlight smalltalk %}
!PnUserDAOTest methodsFor: 'tests'!
testDeleteAllUsers
	self assert:(dao userForEmail:'test@test.eu' password:'test' ) isNotNil .
	dao deleteAllUsers .
	self assert:(dao userForEmail:'test@test.eu' password:'test' ) isNil .
{% endhighlight %}

Well... if everything is ok you have all units test working correctly
and you can be happy!

![PnUserDAO test cases]({{ site.url }}/assets/pnotes-user-dao-tests.png)

The next post is [here][next-post].

[image-based-persistence]: http://onsmalltalk.com/simple-image-based-persistence-in-squeak/
[previous-post]: {% post_url 2014-03-02-iliad-login-page %}
[next-post]: {% post_url 2014-03-04-checking-emails %}
