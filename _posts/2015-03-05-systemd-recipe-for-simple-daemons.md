---
id: 114
title: Systemd recipe for simple daemons
date: 2015-03-05T17:00:46+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=114
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/UUJ7SkW4T8V
publicize_twitter_user:
  - leonardo_cecchi
publicize_twitter_url:
  - http://t.co/9Ugiw61W84
categories:
  - linux
tags:
  - Linux
  - Systemd
---
In the previous post we discussed about upstart job files.
In this post will do the same with Sytemd.
<!--more-->

If your daemon honours the same conditions of the previous post:

  * don&#8217;t fork;
  * can simply been stopped by a SIGSTOP
  * logs to the console;

then systemd calls that daemons &#8220;simple&#8221;, and you should create a simple systemd unit file like this:

{% highlight c %}
[Unit]
Description=Your description here
After=network.target

[Service]
User=the user the daemon will run as
Env=your environment variables here
ExecStart=your daemon file here

[Install]
WantedBy=multi-user.target
{% endhighlight %}

You should place this file in <tt>/usr/lib/systemd/system</tt> with name <tt>[daemonname].service</tt>.
Now you can use the following commands:

{% highlight c %}
$ systemd start <daemonname> # Start this service now
$ systemd stop <daemonname> # Stop this service now
$ systemd status <daemonname> # Get this daemon status
$ journalctl -u <daemonname> # Get the stdout of your daemon
{% endhighlight %}

If you want your service to be started in the boot process you can use this command:

{% highlight c %}
$ systemd enable <daemonname>
{% endhighlight %}

Systemd, from this point of view, is simple and easy.