---
id: 49
title: Iliad Framework, the login form
date: 2014-03-02T18:49:11+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=49
geo_public:
  - "0"
categories:
  - Senza categoria
tags:
  - iliad
  - pharo
  - smalltalk
---
<div class="post">
  <p>
    In the <a href="http://leonardoce.github.io/programming/smalltalk/2014/02/27/iliad-framework-customizing-page.html">previous post</a> we talked about customizing the generated page to include references to the <a href="http://getbootstrap.com">Bootstrap</a> project.
  </p>
  <p>
    This is the first lession where we start working on a complete example, a projecs-notes app usable from desktop and mobile web browsers using the twitter <a href="http://getbootstrap.com">bootstrap framework</a>.
  </p>
  <p>
    Now we will start working on a login page.
  </p>
  <p>
    To build a new page with a form I will start making a component that will be built from the application controller named <code>login</code>. The controller will be named <code>PnProjectLogin</code>.
  </p>
  <p>
    Let&#8217;s start building this new component creating a class whose superclass is <code>ILWidget</code>:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILWidget&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#PnProjectLogin&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'username password loginError'&lt;/span>
    &lt;span class="nf">classVariableNames:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">poolDictionaries:&lt;/span> &lt;span class="s">''&lt;/span>
    &lt;span class="nf">category:&lt;/span> &lt;span class="s">'LeonardoBlog'&lt;/span></code></pre>
  </div>
  <p>
    remember that widgets are stateful: the instance variables will be populated by the application and by the user.
  </p>
  <ul>
    <li>
      <code>username</code> will be the text entered by the user in the login form;
    </li>
    <li>
      <code>password</code> as previous;
    </li>
    <li>
      <code>loginError</code> will be the string of the error message of the login or <code>nil</code> if there is no error message.
    </li>
  </ul>
  <p>
    I won&#8217;t show the accessor methods because they can be automatically generated by the Pharo browser but I will show the initializer method:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'initialization'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">initialize&lt;/span>
    &lt;span class="bp">super&lt;/span> &lt;span class="nf">initialize&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">loginError&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="bp">nil&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">username&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">password&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">''&lt;/span>&lt;span class="p">.&lt;/span>&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    Let&#8217;s start by creating a method to build the username field and the relative label:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildLoginRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> (&lt;span class="nv">loginError&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Login:'&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">username&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ]&lt;span class="p">;&lt;/span> &lt;span class="nf">value:&lt;/span>&lt;span class="nv">username&lt;/span> ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    See how interesting it the <code>build:</code> method: it takes a block and evaluate it passing, as argument, the receiver. Before executing the passed block the method opens the element and closes it when the block finished. In the previous example the <code>aForm div</code> execution results in a <code>DIV</code> element that get automatically closed when the block has terminated its execution.
  </p>
  <p>
    Another interesting thing is the <code>class:</code> method invocation on the <code>aForm div</code> element: to comply with the bootstrap rules we need to insert the <code>has-error</code> class when this element of the form is wrong. To do this we check the <code>loginError</code> contents.
  </p>
  <p>
    In the <code>input</code> element, the action block takes the element text content.
  </p>
  <p>
    This method is somewhat similiar:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildPasswordRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span>(&lt;span class="nv">loginError&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [ &lt;span class="s">'form-group has-error'&lt;/span> ] &lt;span class="nf">ifNil:&lt;/span>  [ &lt;span class="s">'form-group'&lt;/span> ])&lt;span class="p">;&lt;/span>
            &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span> &lt;span class="nv">row&lt;/span> &lt;span class="o">|&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">label&lt;/span> &lt;span class="nf">text:&lt;/span>&lt;span class="s">'Password'&lt;/span>&lt;span class="p">.&lt;/span>
                &lt;span class="nv">row&lt;/span> &lt;span class="nf">input&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'form-control'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">type:&lt;/span>&lt;span class="s">'password'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">action:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">text&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">password&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nv">text&lt;/span> ] ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    As you see, while the login field get constructed with a value (or the previous one), the password is always empty.
  </p>
  <p>
    Ok. Now we will construct the error message row of the form:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildErrorMessageRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    &lt;span class="nv">loginError&lt;/span> &lt;span class="nf">ifNotNil:&lt;/span> [
    (&lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> &lt;span class="s">'alert alert-warning'&lt;/span>) &lt;span class="nf">text:&lt;/span> &lt;span class="nv">loginError&lt;/span> ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    Obviously we have to build the errors row only if there are errors! Well. Now we are missing the &#8220;register&#8221; link.
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">buildRegisterRow:&lt;/span> &lt;span class="nv">aForm&lt;/span>
    (&lt;span class="nv">aForm&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span> &lt;span class="s">'form-group'&lt;/span>) &lt;span class="nf">a&lt;/span>
        &lt;span class="nf">href:&lt;/span> &lt;span class="s">'register'&lt;/span>&lt;span class="p">;&lt;/span>
        &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Want to register?'&lt;/span>&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    Let&#8217;s bind all together in the <code>contents</code> method:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'building'&lt;/span>&lt;span class="k"> stamp: 'LeonardoCecchi 2/28/2014 23:03'!&lt;/span>
&lt;span class="nf">contents&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">h1&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Project Notes - Login'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span>
        &lt;span class="nf">text:&lt;/span>
            &lt;span class="s">'Project Notes is a note taking app that you&lt;/span>
&lt;span class="s">            can use for your project. It will store memos for&lt;/span>
&lt;span class="s">            you and for your team.'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">p&lt;/span> &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Believe me, you really need this app!!'&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="nv">e&lt;/span> &lt;span class="nf">form&lt;/span>
        &lt;span class="nf">build:&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">form&lt;/span> &lt;span class="o">|&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildLoginRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildPasswordRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildErrorMessageRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="bp">self&lt;/span> &lt;span class="nf">buildRegisterRow:&lt;/span> &lt;span class="nv">form&lt;/span>&lt;span class="p">.&lt;/span>
            &lt;span class="nv">form&lt;/span> &lt;span class="nf">button&lt;/span>
                &lt;span class="nf">class:&lt;/span> &lt;span class="s">'btn btn-default'&lt;/span>&lt;span class="p">;&lt;/span>
                &lt;span class="nf">text:&lt;/span> &lt;span class="s">'Login!!'&lt;/span>&lt;span class="p">;&lt;/span>
                &lt;span class="nf">action:&lt;/span> [ &lt;span class="bp">self&lt;/span> &lt;span class="nf">loginAction&lt;/span> ] ] ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    In the <code>loginAction</code> method, which is called when the user press on the &#8220;Login&#8221; button we execute the <code>loginAction</code> method of this class, which is defined like this:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">PnProjectLogin&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'actions'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">loginAction&lt;/span>
    &lt;span class="bp">self&lt;/span> &lt;span class="nf">markDirty&lt;/span> &lt;span class="p">.&lt;/span>
    &lt;span class="nv">username&lt;/span> &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="nv">loginError&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">'Please enter the user name'&lt;/span> ]&lt;span class="p">.&lt;/span>
    &lt;span class="nv">password&lt;/span> &lt;span class="nf">ifEmpty:&lt;/span> [ &lt;span class="o">^&lt;/span> &lt;span class="nv">loginError&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="s">'Please enter the password'&lt;/span> ]&lt;span class="p">.&lt;/span>
    &lt;span class="nv">loginError&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="bp">nil&lt;/span>&lt;span class="p">.&lt;/span>
    &lt;span class="c">"self redirectToLocal: 'browseProject'."&lt;/span></code></pre>
  </div>
  <p>
    In this method, which for now is only a stub, we check for empty data and then we will call the main page.
  </p>
  <p>
    Now we need to integrate this widget in our application, <code>LcBlogProjectNotes</code>. We must add an instance variable named <code>loginWidget</code>.
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="nc">ILApplication&lt;/span> &lt;span class="nf">subclass:&lt;/span> &lt;span class="ss">#LcBlogProjectNotes&lt;/span>
    &lt;span class="nf">instanceVariableNames:&lt;/span> &lt;span class="s">'loginWidget&lt;/span>
&lt;span class="s">    classVariableNames: ''&lt;/span>
&lt;span class="s">    poolDictionaries: ''&lt;/span>
&lt;span class="s">    category: '&lt;/span>&lt;span class="nf">LeonardoBlog&lt;/span>&lt;span class="err">'&lt;/span></code></pre>
  </div>
  <p>
    As we discussed in the <a href="http://leonardoce.github.io/programming/smalltalk/2014/02/16/iliad-lesson-three.html">relative article</a> we need an accessor method that lazily creates the widget:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'accessing'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">loginWidget&lt;/span>
    &lt;span class="o">^&lt;/span> &lt;span class="nv">loginWidget&lt;/span> &lt;span class="nf">ifNil:&lt;/span> [ &lt;span class="nv">loginWidget&lt;/span> &lt;span class="o">:=&lt;/span> &lt;span class="nc">PnProjectLogin&lt;/span> &lt;span class="nb">new&lt;/span> ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    Now we can create the controller method:
  </p>
  <div class="highlight">
    <pre><code class="language-smalltalk">&lt;span class="k">!&lt;/span>&lt;span class="nc">LcBlogProjectNotes&lt;/span>&lt;span class="k"> methodsFor: &lt;/span>&lt;span class="s">'controllers'&lt;/span>&lt;span class="k">!&lt;/span>
&lt;span class="nf">login&lt;/span>
    &lt;span class="o">^&lt;/span> [ &lt;span class="o">:&lt;/span>&lt;span class="nv">e&lt;/span> &lt;span class="o">|&lt;/span> &lt;span class="nv">e&lt;/span> &lt;span class="nf">div&lt;/span> &lt;span class="nf">class:&lt;/span>&lt;span class="s">'container'&lt;/span>&lt;span class="p">;&lt;/span> &lt;span class="nf">build:&lt;/span> &lt;span class="bp">self&lt;/span> &lt;span class="nf">loginWidget&lt;/span> ]&lt;span class="k">! !&lt;/span></code></pre>
  </div>
  <p>
    If you go to <a href="http://localhost:7070/ProjectNotes/login">http://localhost:7070/ProjectNotes/login</a> you should see the following result:
  </p>
  <p>
    <img src="http://leonardoce.github.io/assets/pnotes-login.png" alt="Project notes login form" />
  </p>
  <p>
    Yeah! We succesfuly build our first form, even with error checking. When the <code>loginError</code> is not empty the form looks like this:
  </p>
  <p>
    <img src="http://leonardoce.github.io/assets/pnotes-login-error.png" alt="Project notes login form error" />
  </p>
</div>