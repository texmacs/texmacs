<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Usage of the Python Plugin>

  Separate sentences by using <key|shift return>, when you are ready to have
  <name|Python> evaluate your input press <key|return>. \ If you would like
  this behaviour reversed, go to <verbatim|Focus-\<gtr\>multiline-session-\<gtr\>option-methods>
  and select multiline-input.

  The plugin has the following features:

  <\enumerate>
    <item>Autocompletion is enabled: Simply press the fist letters of a
    built-in command or a command you have previously defined, and then press
    <key|tab> until you find an option of your choice.

    <item>It can output postscript directly to <TeXmacs> via the function
    <verbatim|ps_out (data)>: (Not tested)

    <\itemize>
      <item>If the <verbatim|data> is a string and has more than one line, it
      will be processed as raw <name|Postscript> data.

      <item>If <verbatim|data> is a string, it's supposed to contain the
      filename of a <name|Postscript> file which will be read ( if the file
      has no extension, the defaults .ps and .eps will be tried.)

      <item>If <verbatim|data> is a file or other object which provides a
      '<verbatim|read>' method, data will be obtained by calling such method.
    </itemize>
  </enumerate>

  <tmdoc-copyright|2004|Adrián Soto>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
  </collection>
</initial>