<TeXmacs|2.1.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese>>

<\body>
  <tmdoc-title|\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>>

  It sometimes happens that certain plug-ins are only installed on a remote
  computer. In many cases, it will still be possible to use such a plug-in
  inside <TeXmacs> over an <hlink|SSH|http://www.openssh.com/> connection.

  \<#6709\>\<#7684\>\<#65F6\>\<#5019\>\<#FF0C\>\<#4F1A\>\<#51FA\>\<#73B0\>\<#67D0\>\<#4E9B\>\<#63D2\>\<#4EF6\>\<#53EA\>\<#5B89\>\<#88C5\>\<#5728\>\<#4E86\>\<#8FDC\>\<#7A0B\>\<#8BA1\>\<#7B97\>\<#673A\>\<#4E0A\>\<#7684\>\<#60C5\>\<#51B5\>\<#3002\>\<#5728\>\<#5927\>\<#90E8\>\<#5206\>\<#60C5\>\<#51B5\>\<#4E0B\>\<#FF0C\>\<#4F60\>\<#4F9D\>\<#7136\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#4F7F\>\<#7528\><hlink|
  SSH |http://www.openssh.com/>\<#8FDE\>\<#63A5\>\<#4ECE\>\<#800C\>\<#5728\><TeXmacs>\<#4E2D\>\<#4F7F\>\<#7528\>\<#8FD9\>\<#4E9B\>\<#63D2\>\<#4EF6\>\<#3002\>

  In order to make this work, you first have to make sure that SSH is
  installed on both computers and that connecting by SSH to the remote
  machine can be done automatically, without having to type a password. This
  can be done by copying your public identity on the local server to the
  remote server into the file <verbatim|~/.ssh/authorized_keys>; see the
  documentation on SSH for more information.

  \<#4E3A\>\<#4E86\>\<#505A\>\<#5230\>\<#8FD9\>\<#4E00\>\<#70B9\>\<#FF0C\>\<#4F60\>\<#9996\>\<#5148\>\<#8981\>\<#786E\>\<#4FDD\>\<#4E24\>\<#53F0\>\<#8BA1\>\<#7B97\>\<#673A\>\<#4E0A\>\<#90FD\>\<#5B89\>\<#88C5\>\<#4E86\>
  SSH\<#FF0C\>\<#5E76\>\<#4E14\>\<#53EF\>\<#4EE5\>\<#81EA\>\<#52A8\>\<#901A\>\<#8FC7\>
  SSH \<#8FDE\>\<#63A5\>\<#5230\>\<#8FDC\>\<#7A0B\>\<#8BA1\>\<#7B97\>\<#673A\>\<#FF0C\>\<#800C\>\<#65E0\>\<#9700\>\<#8F93\>\<#5165\>\<#5BC6\>\<#7801\>\<#3002\>\<#8FD9\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#5C06\>\<#672C\>\<#5730\>\<#670D\>\<#52A1\>\<#5668\>\<#4E0A\>\<#7684\>\<#516C\>\<#5171\>\<#8EAB\>\<#4EFD\>\<#590D\>\<#5236\>\<#5230\>\<#8FDC\>\<#7A0B\>\<#670D\>\<#52A1\>\<#5668\>\<#7684\>
  \ <verbatim|~/.ssh/authorized_keys>\<#6587\>\<#4EF6\>\<#4E2D\>\<#6765\>\<#5B9E\>\<#73B0\>\<#FF1B\>\<#66F4\>\<#591A\>\<#4FE1\>\<#606F\>\<#FF0C\>\<#8BF7\>\<#53C2\>\<#9605\>
  SSH \<#7684\>\<#6587\>\<#6863\>\<#3002\>

  As the next step, you have to make sure that <TeXmacs> has been installed
  on both computers. The remote <TeXmacs> installation will mainly be used in
  order to detect which plug-ins can be used on the remote computer.

  \<#63A5\>\<#7740\>\<#FF0C\>\<#4F60\>\<#5FC5\>\<#987B\>\<#786E\>\<#4FDD\>\<#4E24\>\<#53F0\>\<#8BA1\>\<#7B97\>\<#673A\>\<#4E0A\>\<#90FD\>\<#5B89\>\<#88C5\>\<#4E86\>
  <TeXmacs>\<#3002\>\<#5728\>\<#8FDC\>\<#7A0B\>\<#8BA1\>\<#7B97\>\<#673A\>\<#4E0A\>\<#5B89\>\<#88C5\>
  <TeXmacs> \<#7684\>\<#76EE\>\<#7684\>\<#4E3B\>\<#8981\>\<#662F\>\<#68C0\>\<#6D4B\>\<#5728\>\<#8FDC\>\<#7A0B\>\<#8BA1\>\<#7B97\>\<#673A\>\<#4E0A\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>\<#54EA\>\<#4E9B\>\<#63D2\>\<#4EF6\>\<#3002\>

  When everything has been set up correctly in this way, select
  <menu|Insert|Session|Remote> in order to open the remote plug-in selector.
  Add the name of the remote server by typing its name or IP address and
  clicking on <menu|Add>. After a small pause, the remote server should
  appear in the list together with the remote plug-ins which are supported.
  You may now simply select the plug-in you want to use from the list. Notice
  that remote plug-ins may take a few seconds in order to boot. Please be
  patient while booting is in progress.

  \<#4EE5\>\<#4E0A\>\<#8FF0\>\<#65B9\>\<#5F0F\>\<#6B63\>\<#786E\>\<#8BBE\>\<#7F6E\>\<#5168\>\<#90E8\>\<#5185\>\<#5BB9\>\<#540E\>\<#FF0C\>\<#4F7F\>\<#7528\><menu|Insert|Session|Remote>\<#6253\>\<#5F00\>\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>\<#9009\>\<#62E9\>\<#5668\>\<#3002\>\<#8F93\>\<#5165\>\<#8FDC\>\<#7A0B\>\<#670D\>\<#52A1\>\<#5668\>\<#7684\>\<#540D\>\<#79F0\>\<#6216\>IP\<#5730\>\<#5740\>\<#FF0C\>\<#7136\>\<#540E\>\<#70B9\>\<#51FB\><menu|Add>\<#FF0C\>\<#4EE5\>\<#6DFB\>\<#52A0\>\<#8FDC\>\<#7A0B\>\<#670D\>\<#52A1\>\<#5668\>\<#7684\>\<#540D\>\<#79F0\>\<#3002\>\<#7A0D\>\<#7B49\>\<#7247\>\<#523B\>\<#FF0C\>\<#8FDC\>\<#7A0B\>\<#670D\>\<#52A1\>\<#5668\>\<#548C\>\<#652F\>\<#6301\>\<#7684\>\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>\<#5C31\>\<#4F1A\>\<#4E00\>\<#5207\>\<#51FA\>\<#73B0\>\<#5728\>\<#5217\>\<#8868\>\<#4E2D\>\<#3002\>\<#73B0\>\<#5728\>\<#FF0C\>\<#4F60\>\<#53EA\>\<#9700\>\<#8981\>\<#4ECE\>\<#5217\>\<#8868\>\<#4E2D\>\<#9009\>\<#62E9\>\<#4F60\>\<#60F3\>\<#4F7F\>\<#7528\>\<#7684\>\<#63D2\>\<#4EF6\>\<#5373\>\<#53EF\>\<#3002\>\<#8BF7\>\<#6CE8\>\<#610F\>\<#FF0C\>\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>\<#9700\>\<#8981\>\<#51E0\>\<#79D2\>\<#949F\>\<#7684\>\<#542F\>\<#52A8\>\<#65F6\>\<#95F4\>\<#FF0C\>\<#5728\>\<#542F\>\<#52A8\>\<#8FC7\>\<#7A0B\>\<#4E2D\>\<#FF0C\>\<#8BF7\>\<#8010\>\<#5FC3\>\<#7B49\>\<#5F85\>\<#3002\>

  Servers which have been added to the list of remote plug-in servers will be
  remembered at the next time when you start <TeXmacs>. You may use the
  buttons <menu|Remove> and <menu|Update> in order to remove a server from
  the list and to redetermine the list of supported remote plug-ins.

  \<#5DF2\>\<#6DFB\>\<#52A0\>\<#5230\>\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>\<#670D\>\<#52A1\>\<#5668\>\<#5217\>\<#8868\>\<#4E2D\>\<#7684\>\<#670D\>\<#52A1\>\<#5668\>\<#4F1A\>\<#88AB\>\<#8BB0\>\<#4F4F\>\<#FF0C\>\<#5728\>\<#4F60\>\<#4E0B\>\<#6B21\>\<#542F\>\<#52A8\>
  <TeXmacs> \<#65F6\>\<#53EF\>\<#4EE5\>\<#76F4\>\<#63A5\>\<#4F7F\>\<#7528\>\<#3002\>\<#4F60\>\<#53EF\>\<#4EE5\>\<#4F7F\>\<#7528\>
  <menu|Remove> \<#548C\> <menu|Update> \<#6309\>\<#94AE\>\<#4ECE\>\<#5217\>\<#8868\>\<#4E2D\>\<#5220\>\<#9664\>\<#670D\>\<#52A1\>\<#5668\>\<#5E76\>\<#66F4\>\<#65B0\>\<#652F\>\<#6301\>\<#7684\>\<#8FDC\>\<#7A0B\>\<#63D2\>\<#4EF6\>\<#5217\>\<#8868\>\<#3002\>

  <tmdoc-copyright|2015|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>