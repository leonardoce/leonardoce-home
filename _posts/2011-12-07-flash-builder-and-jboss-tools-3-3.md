---
id: 16
title: Flash Builder and JBoss Tools 3.3
date: 2011-12-07T22:05:00+00:00
author: leonardoce
layout: post
guid: http://leonardoce.wordpress.com/?p=16
categories:
  - Uncategorized
tags:
  - flash builder
  - jboss
---
Let say you have Flash Builder and you would like to deploy your application on JBoss AS 7. Maybe you also want to control your JBoss from the IDE.

First of all you must have Flash Builder 4.6, because JBoss Tools 3.3 work only with Eclipse Indigo and only the latest Flash Builder is based on Indigo.

If you have Flash Builder 4.5 you must upgrade to the new release and you must uninstall the old one and install the new release.

When you are uninstalling Flash Builder 4.5 you **want** to uncheck the &#8220;Disable&#8221; option on the first screen, so that the uninstalling process do not delete your previous Flash Builder license.

![/assets/images/posts/uploads/2011/12/wpid-flash_builder_disattiva.png](/assets/images/posts/uploads/2011/12/wpid-flash_builder_disattiva.png)

Now you can install the new Flash Builder release, it will migrate your previous settings.

When you have installed the new Flash Builder you must install the Eclipse J2EE tools. To install the J2EE tools you must add the Indigo update URL to your Flash Builder installation.

First of all select _Install New Software_ from the _Help_ menu in your Flash Builder installation and add to the update sites the Indigo Url, which is <http://download.eclipse.org/releases/indigo/>. This process is explained in the following screenshots:

![/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_1.png](/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_1.png)

![/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_2.png](/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_2.png)

![/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_3.png](/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_3.png)

Now you can choose the &#8220;Indigo&#8221; site to get the package list populated. You must install _Eclipse Java EE Developer Tools_ and _Eclipse Web Developer Tools_.

Then you can follow the same procedure using the JBoss Tools update site, <http://download.jboss.org/jbosstools/updates/development/indigo/>. When you are done you **must** disable the update sites you have added because if you install any updates from the Indigo update site Flash Builder will stop working. Updating base Eclipse in a Flash Builder installation is not supported. See [bug FB-30479](http://bugs.adobe.com/jira/browse/FB-30479) and [bug FB-31761](http://bugs.adobe.com/jira/browse/FB-31761).

To disable the update sites you can open the &#8220;Available Software Sites&#8221; preferences and, in the next window, remove the repositories you just added.

![/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_4.png](/assets/images/posts/uploads/2011/12/wpid-flash_builder_jboss_4.png)

Please keep in mind that you can&#8217;t update the base Eclipse of a Flash Builder installation!