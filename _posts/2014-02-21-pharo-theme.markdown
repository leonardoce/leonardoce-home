---
layout: post
title:  "Pharo theme, how to install mine"
date:   2014-02-21 21:28:24
categories: programming smalltalk
---

Have you seen my custom Pharo theme [here][pharo-theme-address]? If
you don't want to use the link you can see a screenshot:

![Configuration browser, Iliad]({{ site.url }}/assets/pharo-config-browser-iliad.png)

Do you want to install my theme or customize it? Execute the following
code line from a workspace:

{% highlight smalltalk %}
Gofer new
	url: 'http://smalltalkhub.com/mc/LeonardoCecchi/LCSolarizedTheme/main';
	package: 'LCSolarizedTheme';
	load.
{% endhighlight %}

When you have executed this line a new theme `Solarized Dark Theme`
will appear in the appearance system settings category. Enjoy!

[pharo-theme-address]: {% post_url 2014-02-12-iliad-introduction %}
