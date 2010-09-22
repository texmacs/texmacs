<TeXmacs|1.0.5.12>

<style|tmdoc>

<\body>
  <doc-data|<doc-title|Pnambic plugin>>

  <section|Installation>

  Recursively copy the <verbatim|pnambic> directory to
  <code*|~/.TeXmacs/plugins> and do

  <\shell-fragment>
    make
  </shell-fragment>

  \;

  See The Jargon File for the explanation of the name.

  <section|How to debug <TeXmacs> plugins>

  Suppose there is an external program which is called from <TeXmacs> as

  <\shell-fragment>
    extprog --texmacs
  </shell-fragment>

  We want to see all bytes flowing from <TeXmacs> to <code*|extprog> and from
  <code*|extprog> to <TeXmacs> in real time. We write a shell script
  <code*|extprog.sh>:

  <\shell-fragment>
    #!/bin/sh

    tee /tmp/in \| extprog --texmacs \| tee /tmp/out
  </shell-fragment>

  We change <code*|init-extprog.scm> to call this shell wrapper. In fact,
  things are often simpler, and <TeXmacs> calls not <code*|extprog> directly,
  but some shell script <code*|tm_extprog>. It contains a line with
  <code*|exec extprog> somewhere. In such a case, we only need to change this
  single line.

  Then we go to <code*|/tmp> and do

  <\shell-fragment>
    mkfifo in

    mkfifo out
  </shell-fragment>

  Then we do

  <\shell-fragment>
    cat out \| ~/.TeXmacs/plugins/pnambic/bin/p_out
  </shell-fragment>

  This <code*|xterm> window will show us all bytes which <code*|extprog>
  outputs to <TeXmacs>. Some of these bytes are not printatle characters;
  they are shown as hex codes in square brackets, like <code*|[02]> or
  <code*|[05]> (these characters indicate the beginning and the end of a data
  chunk in the <TeXmacs> protocole). The characters <code*|[> and <code*|]>
  are shown as <code*|[[> and <code*|]]>, so that the output is unambiguous.

  Then go to another <code*|xterm> window, go to <code*|/tmp> and say

  <\shell-fragment>
    cat in
  </shell-fragment>

  This window will show all bytes which <code*|extprog> inputs from
  <TeXmacs>. Then start <TeXmacs> and start an <code*|extprog> session. You
  will see all data exchanges, and will be able to find out what goes wrong.

  <section|How to run a <TeXmacs> plugin without <TeXmacs>>

  You can simplify this process. Just run

  <\shell-fragment>
    extprog --texmacs \| ~/.TeXmacs/plugins/pnambic/bin/p_out
  </shell-fragment>

  You see all characters you send to <code*|extprog>; you also see all bytes
  (including non-printable characters) which <code*|extprog> sends back.

  <section|How to run a <TeXmacs> plugin without the plugin>

  Enters <code*|pnambic>: <em|you> will be the plugin. Do the following (the
  order is important!)

  <\enumerate-numeric>
    <item>In an <code*|xterm> window, go to
    <code*|~/.TeXmacs/plugins/pnambic> and say

    <\shell-fragment>
      cat out
    </shell-fragment>

    <item>Start a <code*|Pnambic> session in <TeXmacs>

    <item>In another <code*|xterm> window, go to
    <code*|~/.TeXmacs/plugins/pnambic> and say

    <\shell-fragment>
      bin/p_in in
    </shell-fragment>
  </enumerate-numeric>

  Now you can type in this second window all bytes which the plugin sends to
  <TeXmacs>. Use the same syntax: any byte can be written as two hex digits
  in brackets; if you want to send <code*|[> or <code*|]>, type <code*|[[> or
  <code*|]]>. Note that when you press <shortcut|(kbd-return)> it does <em|not>
  send a newline to <TeXmacs>; to do this, use <code*|[0a]>. Follow the
  <TeXmacs> communication protocol: say something like

  <\shell-fragment>
    [02]verbatim:Hi, this is me, the plugin

    [02]prompt#what? [05][05]
  </shell-fragment>

  In the <TeXmacs> window, you will see the greeting and the prompt. Now you
  can write something after this prompt in the <TeXmacs> window and press
  <shortcut|(kbd-return)>. This input will appear in the first <code*|xterm>
  window (remember? it's where you said <code*|cat out>). Now you pretend
  that you are a CAS, think about this user input, go to the second
  <code*|xterm> window and write the result of your thinking in a form
  suitable for the <TeXmacs> protocol. The user will see it in the <TeXmacs>
  window, will input something new, and so on. When you get tired of this
  game, say something like

  <\shell-fragment>
    [02]verbatim:The end[05]
  </shell-fragment>

  and press <key|C-d>.

  This is a great way to experiment with the <TeXmacs> communication protocal
  and to see how <TeXmacs> reacts when it gets some bytes from a plugin.
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
  </collection>
</initial>