---
id: 7
title: Air 2.7 and Menu Softkey on Android Honeycomb
date: 2011-12-05T20:24:00+00:00
author: leonardoce
layout: post
guid: http://leonardoce.wordpress.com/?p=5
categories:
  - Uncategorized
---
If you are developing using Air 2.6 you are probably using the menu softkey to make users navigating the application.

![/assets/images/posts/uploads/2011/12/wpid-menu_softkey_on_android.png](/assets/images/posts/uploads/2011/12/wpid-menu_softkey_on_android.png)

When you will upgrade to Flash Builder 4.6 the menu key will disappear!

In effect, as the bug report [2936827](https://bugbase.adobe.com/index.cfm?event=bug&id=2936827) reported against Air 2.7 explains, Air 2.7 is targeting, by default, Android SDK 11. This makes your menu key disappearing.

I think you, and our, must try to not use the menu softkey but, in the mean time, you can re-enable the menu soft key setting up the `targetSdkVersion` in your AIR application descriptor:

<pre class="example">&lt;android&gt;
        &lt;manifestAdditions&gt;&lt;![CDATA[
                        &lt;manifest android:installLocation="auto"&gt;
                                &lt;uses-sdk android:minSdkVersion="8" android:targetSdkVersion="10"/&gt;
                                [....]
                        &lt;/manifest&gt;
                        [....]
                ]]&gt;
        &lt;/manifestAdditions&gt;
&lt;/android&gt;</pre>