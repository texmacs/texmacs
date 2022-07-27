<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|远程插件>

  It sometimes happens that certain plug-ins are only installed on a remote
  computer. In many cases, it will still be possible to use such a plug-in
  inside <TeXmacs> over an <hlink|SSH|http://www.openssh.com/> connection.

  有的时候，会出现某些插件只安装在了远程计算机上的情况。在大部分情况下，你依然可以通过使用<hlink|
  SSH |http://www.openssh.com/>连接从而在<TeXmacs>中使用这些插件。

  In order to make this work, you first have to make sure that SSH is
  installed on both computers and that connecting by SSH to the remote
  machine can be done automatically, without having to type a password. This
  can be done by copying your public identity on the local server to the
  remote server into the file <verbatim|~/.ssh/authorized_keys>; see the
  documentation on SSH for more information.

  为了做到这一点，你首先要确保两台计算机上都安装了
  SSH，并且可以自动通过 SSH 连接到远程计算机，而无需输入密码。这可以通过将本地服务器上的公共身份复制到远程服务器的
  \ <verbatim|~/.ssh/authorized_keys>文件中来实现；更多信息，请参阅
  SSH 的文档。

  As the next step, you have to make sure that <TeXmacs> has been installed
  on both computers. The remote <TeXmacs> installation will mainly be used in
  order to detect which plug-ins can be used on the remote computer.

  接着，你必须确保两台计算机上都安装了
  <TeXmacs>。在远程计算机上安装 <TeXmacs>
  的目的主要是检测在远程计算机上可以使用哪些插件。

  When everything has been set up correctly in this way, select
  <menu|Insert|Session|Remote> in order to open the remote plug-in selector.
  Add the name of the remote server by typing its name or IP address and
  clicking on <menu|Add>. After a small pause, the remote server should
  appear in the list together with the remote plug-ins which are supported.
  You may now simply select the plug-in you want to use from the list. Notice
  that remote plug-ins may take a few seconds in order to boot. Please be
  patient while booting is in progress.

  以上述方式正确设置全部内容后，使用<menu|Insert|Session|Remote>打开远程插件选择器。输入远程服务器的名称或IP地址，然后点击<menu|Add>，以添加远程服务器的名称。稍等片刻，远程服务器和支持的远程插件就会全部出现在列表中。现在，你只需要从列表中选择你想使用的插件即可。请注意，远程插件需要几秒钟的启动时间，在启动过程中，请耐心等待。

  Servers which have been added to the list of remote plug-in servers will be
  remembered at the next time when you start <TeXmacs>. You may use the
  buttons <menu|Remove> and <menu|Update> in order to remove a server from
  the list and to redetermine the list of supported remote plug-ins.

  已添加到远程插件服务器列表中的服务器会被记住，在你下次启动
  <TeXmacs> 时可以直接使用。你可以使用 <menu|Remove> 和
  <menu|Update> 按钮从列表中删除服务器并更新支持的远程插件列表。

  <tmdoc-copyright|1998\U2022|Joris van der Hoeven|詹旭弘>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>