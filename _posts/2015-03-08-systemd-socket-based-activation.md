---
id: 116
title: Systemd socket based activation
date: 2015-03-08T07:43:56+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=116
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/2joWrAxYZCV
publicize_twitter_user:
  - leonardo_cecchi
publicize_twitter_url:
  - http://t.co/wv6K0WmHvR
categories:
  - linux
tags:
  - Linux
  - socket
  - Systemd
---
In the previous post we have seen how to start a simple deamon with <tt>systemd</tt>. Now we will use the socket based activation of this init system to make our daemon start when it is needed.
<!--more-->

Let&#8217;s start with a basic server which only prints a message.

{% highlight c %}
#include <stdio.h>
#include <stdlib.h>
#include "CommonLib/net_socket.h"

void panic(lerror *error) {
	  
lstring *buf = NULL;

l_assert(error!=NULL);
	  
buf = lstring_new();
	  
buf = lerror_fill_f(error, buf);
	  
fprintf(stderr, "%s", buf);
	  
lstring_delete(buf);

abort();
}

int main() {
	  
lerror *myError = NULL;
	  
TCPListenSocket *listeningSocket = NULL;
	  
TCPSocket *socket = NULL;

listeningSocket = TCPListenSocket_new("0.0.0.0",
		  
"3233", &myError);
	  
if (myError!=NULL) panic(myError);

puts("Hello server is accepting connections");
	  
fflush(stdout);

while(1) {
		  
socket = TCPListenSocket_accept(listeningSocket,
			  
&myError);
		  
if (myError!=NULL) panic(myError);

TCPSocket_send_string(socket,
			  
"Hello from the server!", &myError);
		  
if (myError!=NULL) panic(myError);

TCPSocket_destroy(socket);
		  
socket = NULL;
	  
}

TCPListenSocket_destroy(listeningSocket);
	  
return 0;
}
{% endhighlight %}

This code is based on the [CommonLib](http://github.com/leonardoce/CommonLib) library.
Now, if systemd started us, we need to get the listening file descriptor and to do so with must include the systemd header file:

{% highlight c %}
#include <systemd/sd-daemon.h>
{% endhighlight %}

and to replace:

{% highlight c %}
	  
listeningSocket = TCPListenSocket_new("0.0.0.0",
		  
"3233", &myError);
	  
if (myError!=NULL) panic(myError);
{% endhighlight %}

with:

{% highlight c %}
	  
if (sd_listen_fds(0)==0) {
		  
listeningSocket = TCPListenSocket_new("0.0.0.0",
			  
"3233", &myError);
		  
if (myError!=NULL) panic(myError);
	  
} else {
		  
listeningSocket = TCPListenSocket_new_from_fd(
			  
SD_LISTEN_FDS_START + 0,
			  
"localhost:3233");
		  
puts("Hello server received socket from SystemD");
		  
fflush(stdout);
	  
}
{% endhighlight %}

The <tt>sd_listen_fds</tt> function will count the sockets passed by systemd and, if systemd started us, the file descriptor <tt>SD_LISTEN_FDS_START + 0</tt>, the first, is encapsulated in a <tt>TCPListenSocket</tt> object. The server then follows in the same way. Simple, isn&#8217;t?

Now our server is socket activatable. We must tell it to systemd writing a <tt>my_server.socket</tt> like this:

{% highlight c %}
[Socket]
ListenStream=3233

[Install]
WantedBy=sockets.target
{% endhighlight %}

We must also write a <tt>my_server.service</tt> file like this:

{% highlight c %}
[Unit]
Description=<description here>
After=network.target

[Service]
User=<user name here>
ExecStart=<executable file name here>
{% endhighlight %}

Note that this service file has not the <tt>install</tt> section.

These files must be placed in our <tt>/usr/lib/systemd/system</tt> directory.
We can make systemd preallocate the socket for us with:

{% highlight c %}
\# systemctl start my_server.socket
{% endhighlight %}

If you ask your system for the running processes you will not see the daemon process.
When you will make the first connection systemd will start it.