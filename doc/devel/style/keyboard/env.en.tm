<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Macros, functions and environment variables>

  The main key-combinations that you should know to write style files are the
  following:

  <\description>
    <expand|item*|<expand|kbd-gen|=>>creates a new assignment. The first
    argument is a new command name and the second argument an expression.

    <expand|item*|<expand|kbd-gen|w>>permits to locally change one or more
    environment variables. With statements are of the form
    <with|mode|math|\<langle\>x<rsub|1>\|a<rsub|1>\|\<cdots\>\|x<rsub|n>\|a<r\
    sub|n>\|b\<rangle\>>, where the <with|mode|math|x<rsub|i>> are the names
    of the variables, the <with|mode|math|a<rsub|i>> their local values, and
    <with|mode|math|b> the text on which the local environment applies.

    <expand|item*|<expand|kbd-gen|m>>creates a macro. Arguments to the macro
    can be inserted using the <key|tab>-key.

    <expand|item*|<expand|kbd-gen|f>>creates a function. Arguments to the
    macro can be inserted using the <key|tab>-key.

    <expand|item*|<expand|kbd-ia|#>>get the value of a macro argument.

    <expand|item*|<expand|kbd-ia|v>>get the value of an environment variable.

    <expand|item*|<expand|kbd-ia|e>>expands the macro with zero or more
    arguments.

    <expand|item*|<expand|kbd-ia|a>>applies a function to zero or more
    arguments.
  </description>

  More precisely, when evaluating a macro expansion
  <with|mode|math|{a\|x<rsub|1>\|\<cdots\>\|x<rsub|n>}> created by
  <expand|kbd-ia|e>, the following action is undertaken:

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

  Functions are similar to macros, except that the arguments of function
  appliciations are evaluated and they can not be edited in a direct way (you
  first need to deactivate the function application, edit the arguments, and
  reactivate). Also, <with|mode|math|y<rsub|1>,\<ldots\>,y<rsub|n>> are now
  rather considered as local environment variables, which are given
  <with|mode|math|x<rsub|1>,\<ldots\>,x<rsub|n>> as their values. These local
  variables are not remembered when a function returns a function which
  involves these variables.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>
