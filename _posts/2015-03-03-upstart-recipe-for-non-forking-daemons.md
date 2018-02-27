---
id: 106
title: Upstart recipe for non-forking daemons
date: 2015-03-03T18:04:51+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=106
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/MUreR68LHAj
publicize_twitter_user:
  - leonardo_cecchi
publicize_twitter_url:
  - http://t.co/bnoJhkz8R5
categories:
  - linux
tags:
  - Ubuntu Server
  - Upstart
---
Do you need to start your non-forking daemon on Ubuntu Server 15.04? You can use this recipe.
<!--more-->

I know that probably systemd will be the default init system even in Ubuntu Server 15.04 but, in the mean time, we have Ubuntu Server 14.04.2 and on that OS the init system is Upstart.

It turns out that Upstart has an [introduction here](http://upstart.ubuntu.com/cookbook/). It is well written and understandable.

If your daemon daemon:

  * don&#8217;t fork;
  * can simply been stopped by a SIGSTOP
  * logs to the console;

you can write a job description like the following one. It has to be placed in <tt>/etc/init/[yourname.conf]</tt>.

{% highlight c %}
description "Your description here"

start on runlevel [2345]
stop on runlevel [!2345]

env [any environment variables you may need]
setuid [the users that the daemon will run as]
respawn

exec [the name of your daemon here]
{% endhighlight %}

Than you can use the following commands:

{% highlight c %}
$ service start <jobname> # starts the job
$ service stop <jobname> # stops the job
$ service status <jobname> # gets the service status
{% endhighlight %}

The output of your daemon will be in <tt>/var/log/upstart/[jobname]</tt>. Isn&#8217;t that sweet?