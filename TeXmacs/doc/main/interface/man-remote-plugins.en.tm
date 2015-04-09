<TeXmacs|1.99.2>

<style|tmdoc>

<\body>
  <tmdoc-title|Remote plug-ins>

  It sometimes happens that certain plug-ins are only installed on a remote
  computer. In many cases, it will still be possible to use such a plug-in
  inside <TeXmacs> over an <hlink|SSH|http://www.openssh.com/> connection.

  In order to make this work, you first have to make sure that SSH is
  installed on both computers and that connecting by SSH to the remote
  machine can be done automatically, without having to type a password. This
  can be done by copying your public identity on the local server to the
  remote server into the file <verbatim|~/.ssh/authorized_keys>; see the
  documentation on SSH for more information.

  As the next step, you have to make sure that <TeXmacs> has been installed
  on both computers. The remote <TeXmacs> installation will mainly be used in
  order to detect which plug-ins can be used on the remote computer.

  When everything has been set up correctly in this way, select
  <menu|Insert|Session|Remote> in order to open the remote plug-in selector.
  Add the name of the remote server by typing its name or IP address and
  clicking on <menu|Add>. After a small pause, the remote server should
  appear in the list together with the remote plug-ins which are supported.
  You may now simply select the plug-in you want to use from the list. Notice
  that remote plug-ins may take a few seconds in order to boot. Please be
  patient while booting is in progress.

  Servers which have been added to the list of remote plug-in servers will be
  remembered at the next time when you start <TeXmacs>. You may use the
  buttons <menu|Remove> and <menu|Update> in order to remove a server from
  the list and to redetermine the list of supported remote plug-ins.

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>