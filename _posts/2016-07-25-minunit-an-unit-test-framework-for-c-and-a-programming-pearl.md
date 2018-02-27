---
id: 147
title: MinUnit, an unit test framework for C and a programming pearl
date: 2016-07-25T22:22:12+00:00
author: leonardoce
layout: post
guid: https://leonardoce.wordpress.com/?p=147
geo_public:
  - "0"
publicize_twitter_user:
  - leonardo_cecchi
publicize_google_plus_url:
  - https://plus.google.com/110105274872261823309/posts/fcMNTqwnvJc
categories:
  - programming
tags:
  - c
---
Sometimes on the Internet you find a programming pearl and you must share with others what you have found.
<!--more-->

I&#8217;ve found a programming pearl here:

http://www.jera.com/techinfo/jtns/jtn002.html

The pearl is:

{% highlight c %}
/* file: minunit.h */
#define mu_assert(message, test) do { if (!(test)) return message; } while (0)
#define mu_run_test(test) do { char *message = test(); tests_run++; \
                                  
if (message) return message; } while (0)
extern int tests_run;
{% endhighlight %}

Just 3 lines of clear, readable code, represent an unit test framework for C. You can use that like this:

{% highlight c %}
   
#include <stdio.h>
   
#include "minunit.h"

int tests_run = 0;

int foo = 7;
   
int bar = 4;

static char * test_foo() {
       
mu_assert("error, foo != 7", foo == 7);
       
return 0;
   
}

static char * test_bar() {
       
mu_assert("error, bar != 5", bar == 5);
       
return 0;
   
}

static char * all_tests() {
       
mu_run_test(test_foo);
       
mu_run_test(test_bar);
       
return 0;
   
}

int main(int argc, char **argv) {
       
char *result = all_tests();
       
if (result != 0) {
           
printf("%s\n", result);
       
}
       
else {
           
printf("ALL TESTS PASSED\n");
       
}
       
printf("Tests run: %d\n", tests_run);

return result != 0;
   
}
{% endhighlight %}

Remember what Uncle Bob said?
You must express your problem in the chosen programming language in a manner that the language must appear to be designed exactly for the problem that you are trying to solve.

Isn&#8217;t that wonderful?