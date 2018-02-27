---
id: 138
title: DBus tutorial, part 2
date: 2015-03-13T20:00:35+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=138
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/Gm4qYJDBqwq
publicize_twitter_user:
  - leonardo_cecchi
publicize_twitter_url:
  - http://t.co/M5AyEZnv44
categories:
  - linux
  - programming
tags:
  - c
  - Dbus
  - Linux
---
In the previous post we talked about DBUS buses and connection names. In this post we will have a look other basic concepts: object paths and interface names.
<!--more-->

When you start d-feet and you select a software &#8220;well-known&#8221; name, such as <tt>org.freedesktop.UPower</tt>, you see, on the right side, a list of objects paths.

Every dbus server applications expose objects and every object has a path, which is a slash separated name like <tt>/org/freedesktop/UPower</tt>.

Every object implements one or more interfaces. An interface is a association of methods and properties that another app can call or get/set. Every interface is identified by a name, which is, like the connection name, a reverse domain name separated by dots (ex. <tt>org.freedesktop.DBus.Properties</tt>).

Just to summarize:

  * a DBUS server application has a well-known name (ex. <tt>org.freedesktop.UPower</tt>);
  * a DBUS server application exposes one on more objects (ex. <tt>/org/freedesktop/UPower</tt>, <tt>/org/freedesktop/UPower/devices/BAT_1</tt>);
  * any object implements one or more interfaces (ex. <tt>org.freedesktop.DBus.Properties</tt>).

You can think of interfaces like the Java or C# ones but in my opinion the most easy way to learn what an interface is to look at d-feet, which has a really good graphical representation of interfaces:

[<img src="/assets/images/posts/uploads/2015/03/immagine_interfacce1.png?w=300" alt="immagine_interfacce" width="300" height="197" class="alignnone size-medium wp-image-142" />](/assets/images/posts/uploads/2015/03/immagine_interfacce1.png)

Ok. Enough explanations. Let&#8217;s start with some code.
In the following example we will call the <tt>GetCriticalAction</tt> of the <tt>org.freedesktop.UPower</tt> interface exposed by the <tt>/org/freedesktop/UPower</tt> object of the <tt>org.freedesktop.UPower</tt> application. Uff&#8230; what a naming!

The <tt>GetCriticalAction</tt> method has the signature:

> GetCriticalAction: () -> (String action) 

That means that this method doesn&#8217;t take any arguments and returns a string, which is named _action_.

{% highlight c %}
#include <dbus/dbus.h>
#include <stdio.h>
#include <stdlib.h>

static void check_and_abort(DBusError *error);

int main() {
	  
DBusConnection *connection = NULL;
	  
DBusError error;
	  
DBusMessage *msgQuery = NULL;
	  
DBusMessage *msgReply = NULL;
	  
const char *interfaceName = NULL;
	  
const char *versionValue = NULL;

dbus_error_init(&error);
	  
connection = dbus_bus_get(DBUS_BUS_SYSTEM, &error);
	  
check_and_abort(&error);

interfaceName = "org.freedesktop.UPower";

msgQuery = dbus_message_new_method_call(
		  
interfaceName,
		  
"/org/freedesktop/UPower",
		  
"org.freedesktop.UPower",
		  
"GetCriticalAction");

msgReply = dbus_connection_send_with_reply_and_block(connection, msgQuery, 1000, &error);
	  
check_and_abort(&error);
	  
dbus_message_unref(msgQuery);

dbus_message_get_args(msgReply, &error, DBUS_TYPE_STRING, &versionValue, DBUS_TYPE_INVALID);

printf("The critical action is: %s\n", versionValue);

dbus_message_unref(msgReply);

return 0;
}

static void check_and_abort(DBusError *error) {
	  
if (!dbus_error_is_set(error)) return;
	  
puts(error->message);
	  
abort();
}
{% endhighlight %}

Let&#8217;s talk about the critical bits. In the previous post you have just seen how to make a connection to the DBus session bus; in this example we are using the system bus. This is the meaning (obvious) of <tt>dbus_bus_get</tt> call.

The <tt>dbus_message_new_method_call</tt> is used to create a new message that must be sent to the bus. The arguments are the coordinates of the process, the object, the interface and the method to call:

{% highlight c %}
msgQuery = dbus_message_new_method_call(
	  
interfaceName,
	  
"/org/freedesktop/UPower",
	  
"org.freedesktop.UPower",
	  
"GetCriticalAction");
{% endhighlight %}

When the message is created the function <tt>dbus_connection_send_with_reply_and_block</tt> will send the message on the bus and wait for the response of the other application for one second (the 1000 in argument list). The function returns the reply:

{% highlight c %}
msgReply = dbus_connection_send_with_reply_and_block(connection, msgQuery, 1000, &error);
{% endhighlight %}

Every DBus message created or returned must be deallocated with the <tt>dbus_message_unref</tt> call:

{% highlight c %}
dbus_message_unref(msgQuery);
{% endhighlight %}

Now we must extract the result string from the reply message and we do it with <tt>dbus_message_get_args</tt>, which is ok for basic types:

{% highlight c %}
dbus_message_get_args(msgReply, &error, DBUS_TYPE_STRING, &versionValue, DBUS_TYPE_INVALID);
{% endhighlight %}

Let&#8217;s try this code:

{% highlight c %}
$ ./provadue
The critical action is: HybridSleep
{% endhighlight %}

Ok. We have just called a DBUS-exposed object method!

In the next post we will talk about properties.