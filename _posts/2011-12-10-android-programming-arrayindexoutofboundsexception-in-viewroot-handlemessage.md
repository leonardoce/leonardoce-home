---
id: 27
title: 'Android programming: ArrayIndexOutOfBoundsException in ViewRoot.handleMessage'
date: 2011-12-10T09:28:00+00:00
author: leonardoce
layout: post
guid: http://leonardoce.wordpress.com/?p=27
tagazine-media:
  - 'a:7:{s:7:"primary";s:0:"";s:6:"images";a:0:{}s:6:"videos";a:0:{}s:11:"image_count";s:1:"0";s:6:"author";s:8:"28728821";s:7:"blog_id";s:8:"30167237";s:9:"mod_stamp";s:19:"2011-12-10 09:30:11";}'
categories:
  - Uncategorized
tags:
  - android
  - java
  - sdk
---
I had fought with this exception for a week:

<pre class="example">Thread [&lt;1&gt; main] (Suspended (exception ArrayIndexOutOfBoundsException))
ViewRoot.handleMessage(Message) line: 1895
ViewRoot(Handler).dispatchMessage(Message) line: 99
Looper.loop() line: 130
ActivityThread.main(String[]) line: 3695
Method.invokeNative(Object, Object[], Class, Class[], Class, int, boolean) line: not available [native method]
Method.invoke(Object, Object...) line: 507
ZygoteInit$MethodAndArgsCaller.run() line: 842
ZygoteInit.main(String[]) line: 600
NativeStart.main(String[]) line: not available [native method]</pre>

When I saw the exception backtrace I tought that I was doing something wrong with my View hierarchy that was damaging the state of the ViewRoot instance.

Then I realized that I was actually accessing an array with an invalid index from an event handler that is actually called from the Android class `android.view.core.ViewRoot`!

Then I understood what was happening. I was using an array with an invalid index in a method that was called by the onTouch event handler!

So, the exception backtrace should be something like this:

<pre class="example">Thread [&lt;1&gt; main] (Suspended (exception ArrayIndexOutOfBoundsException))
MyClassHere.myMethodHere() line: 123
...
...
ViewRoot.handleMessage(Message) line: 1895
ViewRoot(Handler).dispatchMessage(Message) line: 99
Looper.loop() line: 130
ActivityThread.main(String[]) line: 3695
Method.invokeNative(Object, Object[], Class, Class[], Class, int, boolean) line: not available [native method]
Method.invoke(Object, Object...) line: 507
ZygoteInit$MethodAndArgsCaller.run() line: 842
ZygoteInit.main(String[]) line: 600
NativeStart.main(String[]) line: not available [native method]</pre>

Why isn&#8217;t my class appearing in the backtrace? I don&#8217;t know!

So, remember that, when you have an Exception throwing from `ViewRoot.handleMessage` probably you are raising that exception from an event handler that is called from that method!