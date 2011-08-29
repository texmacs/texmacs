<TeXmacs|1.0.7.11>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating sessions>

  A session can be started from the <menu|Insert|Session> menu. Since
  <TeXmacs> is based on the <name|Scheme> language, it is always possible to
  start a <name|Scheme> session using <menu|Insert|Session|Scheme>. On
  <name|Unix> systems, it is usually also possible to start <name|Bash> shell
  sessions using <menu|Insert|Session|Shell>. The remainder of the items in
  the <menu|Insert|Session> menu depend on the plug-ins which are installed
  on your system.

  A session consists of a sequence of input and output fields and possible
  text between them. When pressing <shortcut|(kbd-return)> inside an input
  field of a session, the text inside the environment is evaluated and the
  result is displayed in an output field.

  When entering a command in a session, the application attempts to execute
  it. Several commands may be launched concurrently in the same document, but
  the output will only be active in the session where the cursor is and at
  the place of the cursor. Therefore, we recommend to use different buffers
  for parallel executions.

  For each type of extern application, one may choose between sharing a
  single process by different sessions, or launching a separate process for
  each different session. More precisely, when inserting a session using
  <menu|Insert|Session|Other>, you may specify both a ``session type''
  (Shell, Pari, Maxima, <abbr|etc.>) and a ``session name'' (the default name
  is ``default''). Sessions with different names correspond to different
  processes and sessions with the same name share a common process.

  In order to finish the process which underlies a given session, you may use
  <menu|Session|Close session>. When pressing <shortcut|(kbd-return)> in the
  input of a non-connected system, the system will be restarted
  automatically. You may also use <menu|Session|Interrupt execution> in order
  to interrupt the execution of a command. However, several applications do
  not support this feature.

  In order to evaluate all fields of <abbr|e.g.> a previously created
  session, you may use <menu|Session|Evaluate|Evaluate all>. Similarly,
  <menu|Session|Evaluate|Evaluate above> and <menu|Session|Evaluate|Evaluate
  below> allow you to evaluate all field above or below the current field.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>