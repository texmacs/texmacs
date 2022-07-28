<TeXmacs|2.1.3>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|chinese|python>>

<\body>
  <tmdoc-title|创建会话>

  可以从<menu|Insert|Session>菜单启动会话。因为 <TeXmacs>
  是基于 <name|Scheme> 语言的，所以总是可以使用
  <menu|Insert|Session|Scheme> 来启动一个 <name|Scheme> 会话。在
  <name|Unix> 系统上，通常还可以使用 <menu|Insert|Session|Shell>
  来启动一个 <name|Bash> shell 会话。如果你的系统上安装了其他会话插件，那么它们会出现在<menu|Insert|Session>菜单中的其余项目中。

  会话由一系列输入和输出字段以及它们之间可能的文本组成。当在一个会话的输入字段内按
  <shortcut|(kbd-return)> 时，输入字段内的文本将被求值，结果将会显示在输出字段中。

  When entering a command in a session, the application attempts to execute
  it. Several commands may be launched concurrently in the same document, but
  the output will only be active in the session where the cursor is and at
  the place of the cursor. Therefore, we recommend to use different buffers
  for parallel executions.（这一段不太理解，保留）

  对于不同类型的外部应用程序，可以选择是由不同的会话共享一个进程，还是为每个不同的会话启动一个单独的进程。更准确地说，当你使用<menu|Insert|Session|Other>插入会话时，你可以同时指定\P会话类型\Q（目前的选项名称为Lan）（Shell、Pari、Maxima
  等）和\P会话名称\Q（目前的选项名称为Sec）（默认名称为\Pdefault\Q）。不同名称的会话对应不同的进程，相同名称的会话共享一个共同的进程。

  为了中断给定会话的执行过程，你可以使用<menu|Session|Close
  session>，同时会断开与外部系统的连接。在未连接外部系统的会话输入字段中按
  <shortcut|(kbd-return)> ，外部系统将被自动重新启动。你也可以使用<menu|Session|Interrupt
  execution>来中断命令的执行。但是，可能一些应用程序不支持此功能。

  为了评估所有字段，例如评估所有之前创建的会话，你可以使用<menu|Session|Evaluate|Evaluate
  all>。 类似地，<menu|Session|Evaluate|Evaluate
  above>和<menu|Session|Evaluate|Evaluate
  below>允许你对当前字段上方或下方的所有字段求值。

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