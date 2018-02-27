---
id: 154
title: DBus tutorial, part 3
date: 2015-03-17T19:52:56+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=154
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/VhWuBaREQPF
publicize_twitter_user:
  - leonardo_cecchi
publicize_twitter_url:
  - http://t.co/Wkl50CYQ3B
categories:
  - programming
tags:
  - c
  - Dbus
  - Linux
---
In the previous post we have seen how to call a simple method in a DBus exposed object. In this post we will talk about data-types and about properties.
<!--more-->

In the examples that we have seen we talked only about strings but DBus has many datatypes, and they can be basic types and container types.
[The basic types](http://dbus.freedesktop.org/doc/dbus-specification.html#type-system) are:

  * byte (type <tt>y</tt>);
  * boolean (type <tt>b</tt>);
  * int/uint 16/32 bit (type <tt>n,q,i,u</tt>);
  * double (type <tt>d</tt>);
  * unix_fd (type <tt>h</tt>);
  * string (type <tt>s</tt>);
  * object_path (type <tt>o</tt>);
  * signature (type <tt>g</tt>).

As you have seen every basic type is identified by a type character. This type character is used in the signatures of the methods and of the methods arguments and of the result type. Now we will see the [container types](http://dbus.freedesktop.org/doc/dbus-specification.html#type-system):

  * struct (type <tt>([field types])</tt>, ad ex. a struct composed of two fields, a string and an integer, has type <tt>(sn)</tt>);
  * array (type <tt>a[element type]</tt>, ad ex. an array of integers has the type <tt>an</tt>);
  * variant (type <tt>v</tt>);
  * dict (type <tt>{[key type][value type]}</tt>, ad ex. a dictionary that maps strings to integers has type <tt>{sn}</tt>).

So, with the exception of the variant type, every container type has a signature composed of two or more characters.
The variant type is strange indeed, because it can represent every valid DBus type and the message will contain the signature of the actual passed value. The variant type is really useful because it enables DBus to manage properties. Here it is the d-feet representation of the <tt>org.freedesktop.DBus.Properties</tt> interface:

[<img src="/assets/images/posts/uploads/2015/03/dbus_energy.png?w=660" alt="dbus_energy" width="660" height="477" class="alignnone size-large wp-image-161" />](/assets/images/posts/uploads/2015/03/dbus_energy.png)

As you see the <tt>Get</tt> method has the signature:

> Get: (String interface, String propname) -> (Variant value) 

How could you express the signature of the <tt>Get</tt> method if you wouldn&#8217;t have the <tt>Variant</tt> type?
As you can imagine the <tt>Get</tt> method let&#8217;s you get the value of a property, the <tt>GetAll</tt> method let&#8217;s you get the value of all the properties and the <tt>Set</tt> method let&#8217;s you set the value of a property. Every method takes as argument the name of the interface so a property can be in more than two interface without misunderstandings.

Ok. Enough talk. Let&#8217;s see some code.

I want to write a simple battery monitor and I want to use the UPower daemon for that. UPower exposes an object for every battery in our computer and I want to use the object at path <tt>/org/freedesktop/UPower/devices/battery_BAT1</tt>.

{% highlight c %}
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dbus/dbus.h>

static void abort_on_error(DBusError *error);
static DBusMessage *create_property_get_message(const char *bus_name, const char *path, const char *iface, const char *propname);
static double extract_double_from_variant(DBusMessage *reply, DBusError *error);
static double get_double_property(DBusConnection *connection, const char *bus_name, const char *path, const char *iface, const char *propname, DBusError *error);

int main() {
	  
DBusConnection *connection = NULL;
	  
DBusError error;
	  
double energy = 0;
	  
double fullEnergy = 0;

dbus_error_init(&error);
	  
connection = dbus_bus_get(DBUS_BUS_SYSTEM, &error);
	  
abort_on_error(&error);

energy = get_double_property(connection, "org.freedesktop.UPower",
				       
"/org/freedesktop/UPower/devices/battery_BAT1",
				       
"org.freedesktop.UPower.Device",
				       
"Energy",
				       
&error);
	  
abort_on_error(&error);

fullEnergy = get_double_property(connection, "org.freedesktop.UPower",
				       
"/org/freedesktop/UPower/devices/battery_BAT1",
				       
"org.freedesktop.UPower.Device",
				       
"EnergyFull",
				       
&error);

abort_on_error(&error);

printf("%lf", (energy*100)/fullEnergy);

return 0;
}

static void abort_on_error(DBusError *error) {
	  
if (dbus_error_is_set(error)) {
		  
fprintf(stderr, "%s", error->message);
		  
abort();
	  
}
}
{% endhighlight %}

To get the current battery level we want to read the <tt>Energy</tt> property of the and the <tt>EnergyFull</tt> as exposed by UPower at the object <tt>/org/freedesktop/UPower/devices/battery_BAT1</tt> then we display the battery level percentage. The code gets interesting in the <tt>get_double_property</tt>:

{% highlight c %}
static double get_double_property(DBusConnection *connection, const char *bus_name, const char *path, const char *iface, const char *propname, DBusError *error) {
	  
DBusError myError;
	  
double result = 0;
	  
DBusMessage *queryMessage = NULL;
	  
DBusMessage *replyMessage = NULL;

dbus_error_init(&myError);

queryMessage = create_property_get_message(bus_name, path, iface, propname);
	  
replyMessage = dbus_connection_send_with_reply_and_block(connection,
						    
queryMessage,
						    
1000,
						    
&myError);
	  
dbus_message_unref(queryMessage);
	  
if (dbus_error_is_set(&myError)) {
		  
dbus_move_error(&myError, error);
		  
return 0;
	  
}

result = extract_double_from_variant(replyMessage, &myError);
	  
if (dbus_error_is_set(&myError)) {
		  
dbus_move_error(&myError, error);
		  
return 0;
	  
}

dbus_message_unref(replyMessage);

return result;
}
{% endhighlight %}

As you see extracting a property value means calling the <tt>Get</tt> method of the <tt>org.freedesktop.DBus.Properties</tt> interface. The value is then extracted from the DBus response with the <tt>extract_double_from_variant</tt> function. The <tt>create_property_get_message</tt> just creates a plain DBus method calling message:

{% highlight c %}
static DBusMessage *create_property_get_message(const char *bus_name, const char *path, const char *iface, const char *propname) {
	  
DBusMessage *queryMessage = NULL;

queryMessage = dbus_message_new_method_call(bus_name, path,
						      
"org.freedesktop.DBus.Properties",
						      
"Get");
	  
dbus_message_append_args(queryMessage,
				   
DBUS_TYPE_STRING, &iface,
				   
DBUS_TYPE_STRING, &propname,
				   
DBUS_TYPE_INVALID);

return queryMessage;
}
{% endhighlight %}

The <tt>extract_double_from_variant</tt> function, instead, is really interesting as the double value must be extracted from the variant return type as you would do with a DBus structure:

{% highlight c %}
static double extract_double_from_variant(DBusMessage *reply, DBusError *error) {
	  
DBusMessageIter iter;
	  
DBusMessageIter sub;
	  
double result;

dbus_message_iter_init(reply, &iter);

if (DBUS_TYPE_VARIANT != dbus_message_iter_get_arg_type(&iter)) {
		  
dbus_set_error_const(error, "reply_should_be_variant", "This message hasn&#8217;t a variant response type");
		  
return 0;
	  
}

dbus_message_iter_recurse(&iter, &sub);

if (DBUS_TYPE_DOUBLE != dbus_message_iter_get_arg_type(&sub)) {
		  
dbus_set_error_const(error, "variant_should_be_double", "This variant reply message must have double content");
		  
return 0;
	  
}

dbus_message_iter_get_basic(&sub, &result);
	  
return result;
}
{% endhighlight %}

Here we see a new DBus library type: the <tt>DBusMessageIter</tt>. This type let&#8217;s you explore the data attached to a DBus message.

When you initialize an iterator with the <tt>dbus_message_iter_init</tt> call the iterator is placed on the first element. You can iterate to the next element with <tt>dbus_message_iter_next</tt> or check with <tt>dbus_message_iter_has_next</tt> if the iterator has a next element.

You can only extract values for the basic types using the <tt>dbus_message_iter_get_basic</tt> call. When you need to read a container type, such as our variant type, you need to initialize a sub-iterator with the <tt>dbus_message_iter_recurse</tt> call. Iterators are stack-only structures and must not be deallocated.

Now you can work with container types. In the next post we will talk about DBus introspection.