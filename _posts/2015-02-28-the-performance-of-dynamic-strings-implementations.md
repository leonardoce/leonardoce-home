---
id: 65
title: The performance of dynamic strings implementations
date: 2015-02-28T15:00:48+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=65
geo_public:
  - "0"
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/Q998Li56kCF
publicize_twitter_url:
  - http://t.co/3Ph19gBB5K
publicize_twitter_user:
  - leonardo_cecchi
categories:
  - Senza categoria
tags:
  - c
---
Let&#8217;s say you want to create a nice string abstract data type to use in your C code. You can end with a structure like this:

{% highlight c %}
typedef struct mstring mstring;

struct mstring {
      
int buflen;
      
int len;
      
char *buf;
};

mstring *mstring_new() {
      
mstring *result;

// insert memory allocation errors check here
      
result = (mstring*)malloc(sizeof(struct mstring));
      
result->len = 0;
      
result->buflen = 128;
      
result->buf = (char *)malloc(result->buflen);

return result;
}

void mstring_append(mstring *dest, const char *src) {
      
&#8230;
}
{% endhighlight %}

  * the **buflen** field keeps track of the length of the allocated buffer;
  * the **len** field keeps track of the length of the string, so that you can know the string length in constant time;
  * the **buf** field keeps track of the memory address where the string is allocated. This field will be changed when you reallocate the string when the buffer is full.

It sound nice and you can also have manipulation functions that reallocate the string when necessary. They will be really useful! Let&#8217;s try:

{% highlight c %}
mstring *m = mstring_new();

mstring_append(m, "Hi this is me! I&#8217;m ");
mstring_append(m, myNameHere);
printf("%s", m->buf);

mstring_delete(m); m=NULL;
{% endhighlight %}

This is nice indeed! But is really not that efficient from a memory point of view.
In your heap you will have:

  * The string descriptor (12 byte on a 32-bit architecture)
  * The string contents (variable length)

Your implementation of the holy tuple `malloc` and `free` must keep track of the string descriptor and of the strings contents, which can be (and will be) non-contiguous in memory.

You can do really better allocating the string descriptor directly before the string contents and having `mstring_new` return the address of the string contents instead of the string descriptor. Your strings will be C-compatible and more efficient.
Let&#8217;s try again:

{% highlight c %}
typedef char *fstring;

struct fstring_header {
      
int buflen;
      
int len;
};

fstring fstring_new() {
      
struct fstring_header *header;
      
fstring result;

header = (struct fstring_header*) malloc(128);
      
header->buflen=128-sizeof(struct fstring_header);
      
header->len=0;

result = ((char*)header)+sizeof(struct fstring_header);
      
result[0]=0;

return result;
}
{% endhighlight %}

But now your `fstring_append` function can&#8217;t simply change the `buf` field when a buffer reallocation is needed. You must to reallocate the entire string, descriptor included.

{% highlight c %}
fstring* fstring_append(fstring *dest, const char *src) {
      
&#8230;.
}
{% endhighlight %}

So your code will change like this:

{% highlight c %}
fstring *m = fstring_new();

m = fstring_append(m, "Hi this is me! I&#8217;m ");
m = fstring_append(m, myNameHere);
printf("%s", m);

fstring_delete(m); m=NULL;
{% endhighlight %}

You may say that this is premature optimization but wait! You saved:

  * one malloc
  * one free

and now the string descriptor is also attached to the string, and your cache will be happy. Really more efficient.
In my tests this string implementation is two times more efficient that the previous.

Want to try a really good dynamic string implementation? Try [SDS](https://github.com/antirez/sds). This is used in [Redis](https://github.com/antirez/redis).
You can also try my C all-purpose library here: [CommonLib](https://github.com/leonardoce/CommonLib).