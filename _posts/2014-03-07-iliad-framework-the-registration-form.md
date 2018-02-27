---
id: 56
title: Iliad Framework, the registration form
date: 2014-03-07T18:53:07+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=56
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
In the [previous post](http://leonardoce.github.io/programming/smalltalk/2014/03/04/checking-emails.html) we talked about checking emails addresses, just to show how we can use regexps in Pharo.

In this post we are returning to the Iliad Framework and we will be a registration form.

We start, as we have done for the login form, creating a component for the registration form. This component will then be integrated into the application and another controller will be build.

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span>&lt;span class="k"> subclass: &lt;/span>&lt;span class="ss">#PnCreateUser&lt;/span>
&lt;span class="k">    instanceVariableNames: &lt;/span>&lt;span class="s">'email password password2 name surname errors'&lt;/span>
&lt;span class="k">    classVariableNames: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    poolDictionaries: &lt;/span>&lt;span class="s">''&lt;/span>
&lt;span class="k">    category: &lt;/span>&lt;span class="s">'LeonardoBlog'&lt;/span>&lt;span class="k">!&lt;/span></code></pre>
</div>

As you can see the instance variable of this component, like what was happening in the registration form, are derived from the variable state of the component. We created an instance variable for every data that the user will enter and a `errors` fields that will be empty whenxb there are no errors.

The initialize method clear all the instance variables:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'initialization'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="bp">super&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">name&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">surname&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">password&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">password2&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">email&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>&lt;span class="k">! !&lt;/span></code></pre>
</div>

As you saw in the login form when you are programming in Smalltalk you usually create short methods, more short than the method you will be used to. This is an example:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">contents&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Register your account'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span>
        &lt;span class="nf">text:&lt;/span>
            &lt;span class="s">'You will use your email to login'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">form&lt;/span>
        &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">form&lt;/span> &lt;span class="o">|&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildEmailRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>

            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildNameRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildSurnameRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>

            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildPasswordRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildRepeatPasswordRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>

            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildErrorMessageRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>

            &lt;span class="nv">form&lt;/span> &lt;span class="nf">button&lt;/span>
                &lt;span class="nf">class:&lt;/span> &lt;span class="s">'btn btn-default'&lt;/span>&lt;span class="p">;&lt;/span>
                &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Register to project notes'&lt;/span>&lt;span class="p">;&lt;/span>
                &lt;span class="nf">action:&lt;/span> [ &lt;span class="bp">self&lt;/span> &lt;span class="nf">registerAction&lt;/span> ] ] ]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildErrorMessageRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">errors&lt;/span>
        &lt;span class="nf">ifNotEmpty:&lt;/span> [ (&lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> &lt;span class="s">'alert alert-warning'&lt;/span>) &lt;span class="nf">ul&lt;/span>
        &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">box&lt;/span> &lt;span class="o">|&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">findTokens:&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span>) &lt;span class="nf">do:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">msg&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">box&lt;/span> &lt;span class="nf">li&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="nv">msg&lt;/span> ] ] ]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildPasswordRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Password: '&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span>  &lt;span class="nf">type:&lt;/span> &lt;span class="s">'password'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">password&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="s">''&lt;/span> ]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildEmailRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Email'&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="err">email&lt;/span>&lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="nv">email&lt;/span>]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildSurnameRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Surname'&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="err">surname&lt;/span>&lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="nv">email&lt;/span>]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildRepeatPasswordRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Repeat Password: '&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">type:&lt;/span> &lt;span class="s">'password'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">password2&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="s">''&lt;/span>]&lt;span class="k">! !&lt;/span>

&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildNameRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Name'&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="err">name&lt;/span>&lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="nv">email&lt;/span>]&lt;span class="k">! !&lt;/span></code></pre>
</div>

In the previous example there is nothing interesting: just creating a form like the login form but with more fields. This is the error checking method that we will be calling before the registration action:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">checkErrors&lt;/span>
    &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">email&lt;/span> &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'The email must not be empty because you need to login in the system'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    (&lt;span class="nc">PnUtils&lt;/span> &lt;span class="nf">checkEmail:&lt;/span> &lt;span class="nv">email&lt;/span>) &lt;span class="nb">ifFalse:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'This doesn''t look like a real email to me'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    &lt;span class="nv">name&lt;/span> &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'The name is a required field'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    &lt;span class="nv">surname&lt;/span> &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'The surname is a required field'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    (&lt;span class="nv">password&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nv">password2&lt;/span>) &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'You must enter a password'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    (&lt;span class="nv">password&lt;/span> &lt;span class="nf">=&lt;/span> &lt;span class="nv">password2&lt;/span>) &lt;span class="nb">ifFalse:&lt;/span> [ &lt;span class="nv">errors&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">errors&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="s">'The password are not matching'&lt;/span>&lt;span class="nf">,&lt;/span> &lt;span class="nc">String&lt;/span> &lt;span class="nf">cr&lt;/span> ]&lt;span class="p">.&lt;/span>
    &lt;span class="k">! !&lt;/span></code></pre>
</div>

As you see I only fill the `errors` instance variable with the error message and the row building methods will do the rest.

Now the registration action, that is more interesting:

<div class="highlight">
  <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnCreateUser&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">registerAction&lt;/span>
    &lt;span class="o">|&lt;/span>&lt;span class="nv"> u &lt;/span>&lt;span class="o">|&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">markDirty&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">checkErrors&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">errors&lt;/span> &lt;span class="nf">ifNotEmpty:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="bp">self&lt;/span>&lt;span class="p">.&lt;/span> ]&lt;span class="p">.&lt;/span>

    &lt;span class="nv">u&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnUser&lt;/span> &lt;span class="nb">new&lt;/span> &lt;span class="nf">realname:&lt;/span>&lt;span class="nv">name&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">surname:&lt;/span> &lt;span class="nv">surname&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">email:&lt;/span> &lt;span class="nv">email&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">password:&lt;/span> &lt;span class="nv">password&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nc">PnUserDAO&lt;/span> &lt;span class="nf">current&lt;/span> &lt;span class="nf">register:&lt;/span>&lt;span class="nv">u&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">show:&lt;/span>(&lt;span class="nc">ILInformationWidget&lt;/span> &lt;span class="nb">new&lt;/span> &lt;span class="nf">informationString:&lt;/span> &lt;span class="s">'We created an user for you. Click ok to go to the app.'&lt;/span>)
        &lt;span class="nf">onAnswer:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">redirectToLocal:&lt;/span> &lt;span class="s">'notes'&lt;/span> ]&lt;span class="p">.&lt;/span>&lt;span class="k">! !&lt;/span></code></pre>
</div>

The interesting bits are in the DAO usage, that &#8220;persist&#8221; the `PnUser` object, and the `show:onAnswer:` method.

The `show:onAnswer:` method of the `ILWidget` class can handle the control flow for you. In the next page rendering process the invoking widget will be substituted with the passed widget, in this case `ILInformationWidget`. When the shown widget (`ILInformationWidget`) will call the `answer` method the control flow will return to the block passed to `show:onAnswer:`.

This block redirect the browser to the `notes` controller, which will handle the notes creation and visualization proces.