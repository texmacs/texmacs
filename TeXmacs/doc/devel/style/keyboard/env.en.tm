<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Macros and environment variables>

  The main key-combinations that you should know to write style files are the
  following:

  <\description>
    <item*|<key|inactive =>>creates a new assignment. The first argument is a new
    command name and the second argument an expression.

    <item*|<key|inactive w>>permits to locally change one or more environment
    variables. With statements are of the form
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<rsub|n>\|b\<rangle\>>,
    where the <with|mode|math|x<rsub|i>> are the names of the variables, the
    <with|mode|math|a<rsub|i>> their local values, and <with|mode|math|b> the
    text on which the local environment applies.

    <item*|<key|inactive m>>creates a macro. Arguments to the macro can be inserted
    using the <key|tab>-key.

    <item*|<key|inactive #>>get the value of a macro argument.

    <item*|<key|inactive v>>get the value of an environment variable.

    <item*|<key|inactive e>>expands the macro with zero or more arguments.
  </description>

  More precisely, when evaluating a macro expansion
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> created by
  <key|inactive e>, the following action is undertaken:

  <\itemize>
    <item>If <with|mode|math|a> is not a string nor a macro, then
    <with|mode|math|a> is evaluated once. This results either in a macro name
    or a macro expression <with|mode|math|f>.

    <item>If we obtain a macro name, then we replace <with|mode|math|f> by
    the value of the environment variable <with|mode|math|f>. If, after this,
    <with|mode|math|f> is still not a macro expression, then we return
    <with|mode|math|f>.

    <item>Let <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> be the arguments
    of <with|mode|math|f> and <with|mode|math|b> it's body (superfluous
    arguments are discarded; missing arguments take the empty string as their
    default value). Then we substitute <with|mode|math|x<rsub|i>> for each
    <with|mode|math|y<rsub|i>> in <with|mode|math|b> and return the evaluated
    result.
  </itemize>

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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>