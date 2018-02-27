---
layout: post
title:  "Archlinux networking commands"
categories: archlinux shell
---

I'm used with distributions that have networking commands like
`netstat`, `ifconfig`, etc in their default installation so, when I
started using ArchLinux on daily basis, I found that these commands
are missing.

In effect ArchLinux doesn't have these commands on a base install but
you can install them using:

{% highlight console %}
pacman -S net-tools
{% endhighlight %}

I'm curious and so I immediatly thinked: the ArchLinux mantainers are
really smart so why they have choosen not to install `net-tools` by
default? Answer: they have alternative tools.

These alternative tools are [ss][ss-man-page] and
[ip][ip-man-page]. This is an example:

{% highlight console %}
$ ss -l | grep tcp
nl     UNCONN     4352   0              tcpdiag:ss/27803                *
nl     UNCONN     768    0              tcpdiag:kernel                 *
tcp    UNCONN     0      0                    *:ipproto-68               *:*
tcp    UNCONN     0      0           10.5.7.203:ptp                   *:*
tcp    UNCONN     0      0            127.0.0.1:ptp                   *:*
tcp    UNCONN     0      0                    *:ptp                   *:*
tcp    UNCONN     0      0                    *:ipproto-57859               *:*
tcp    UNCONN     0      0                   :::ipproto-23550              :::*
tcp    UNCONN     0      0         fe80::a00:27ff:fefb:86d0:ptp                  :::*
tcp    UNCONN     0      0                  ::1:ptp                  :::*
tcp    UNCONN     0      0                   :::ptp                  :::*
tcp    LISTEN     0      128                  *:ssh                   *:*
tcp    LISTEN     0      128                 :::ssh                  :::*
{% endhighlight %}

and you can use `ip` like this:

{% highlight console %}
1: lo: <LOOPBACK,UP,LOWER_UP> mtu 65536 qdisc noqueue state UNKNOWN group default 
    link/loopback 00:00:00:00:00:00 brd 00:00:00:00:00:00
    inet 127.0.0.1/8 scope host lo
       valid_lft forever preferred_lft forever
    inet6 ::1/128 scope host 
       valid_lft forever preferred_lft forever
2: enp0s3: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc pfifo_fast state UP group default qlen 1000
    link/ether 08:00:27:fb:86:d0 brd ff:ff:ff:ff:ff:ff
    inet 10.5.7.203/17 brd 10.5.127.255 scope global enp0s3
       valid_lft forever preferred_lft forever
    inet6 fe80::a00:27ff:fefb:86d0/64 scope link 
       valid_lft forever preferred_lft forever
{% endhighlight %}

[ss-man-page]: http://linux.die.net/man/8/ss
[ip-man-page]: http://linux.die.net/man/8/ip
