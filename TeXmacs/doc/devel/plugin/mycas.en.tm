<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Studying the ``mycas'' example>

  The best way to start implementing a new interface with <apply|TeXmacs> is
  to take a look at the sample ``computer algebra system'' <verbatim|mycas>,
  which can be found in the directory <verbatim|$TEXMACS_PATH/misc/mycas>.
  The file <verbatim|mycas.cpp>, which is listed at the end of this section,
  contains a very simple program which can be interfaced with
  <apply|TeXmacs>. In order to test the program, you should compile it using:\ 

  <\verbatim>
    \ \ \ \ g++ mycas.cpp -o mycas
  </verbatim>

  and move the binary <verbatim|mycas> to some location in your path. When
  starting up <apply|TeXmacs>, you should then have a <apply|menu|Mycas>
  entry in the <apply|menu|Insert|Session> menu.

  <section|Studying the source code step by step>

  Let us study the source code of <verbatim|mycas> step by step. First, all
  communication takes place via standard input and output, using pipes. In
  order to make it possible for <apply|TeXmacs> to know when the output from
  your system has finished, all output needs to be encapsulated in blocks,
  using three special control characters:\ 

  <\verbatim>
    \ \ \ \ #define DATA_BEGIN \ \ ((char) 2)<format|next line> \ \ \ #define
    DATA_END \ \ \ \ ((char) 5)<format|next line> \ \ \ #define DATA_ESCAPE
    \ ((char) 27)
  </verbatim>

  The <verbatim|DATA_ESCAPE> character followed by any other character
  <with|mode|math|c> may be used to produce <with|mode|math|c>, even if
  <with|mode|math|c> is one of the three control characters. An illustration
  of how to use <verbatim|DATA_BEGIN> and <verbatim|DATA_END> is given by the
  startup banner:\ 

  <\verbatim>
    \ \ \ \ int<format|next line> \ \ \ main () {<format|next line>
    \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ cout \<less\>\<less\> "Welcome to my test computer
    algebra system for TeXmacs\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "This software comes with no warranty
    whatsoever\\n";<format|next line> \ \ \ \ \ cout \<less\>\<less\> "(c)
    2001 \ by Joris van der Hoeven\\n";<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ fflush (stdout);
  </verbatim>

  The first line of <verbatim|main> says that the startup banner will be
  printed in the ``verbatim'' format. The <verbatim|next_input> function,
  which is called after outputting the banner, is used for printing a prompt
  and will be detailed later. The final <verbatim|DATA_END> closes the
  startup banner block and tells <apply|TeXmacs> that <verbatim|mycas> is
  waiting for input. Don't forget to flush the standard output, so that
  <apply|TeXmacs> will receive the whole message.

  The main loop starts by asking for input from the standard input:\ 

  <\verbatim>
    \ \ \ \ \ \ while (1) {<format|next line> \ \ \ \ \ \ \ char
    buffer[100];<format|next line> \ \ \ \ \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ \ \ \ \ if (strcmp (buffer, "quit") == 0)
    break;
  </verbatim>

  The output which is send back should again be enclosed in a
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block.\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "verbatim:";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> "You
    typed " \<less\>\<less\> buffer \<less\>\<less\> "\\n";
  </verbatim>

  Inside such a block you may recursively send other blocks, which may be
  specified in different formats. For instance, the following code will send
  a <apply|LaTeX> formula:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And now a LaTeX formula:
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "latex:" \<less\>\<less\> "$x^2+y^2=z^2$"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> "\\n";
  </verbatim>

  For certain purposes, it may be useful to directly send output in
  <apply|TeXmacs> format using a <apply|scheme> representation:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "And finally a fraction
    ";<format|next line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "scheme:" \<less\>\<less\> "(frac \\"a\\" \\"b\\")"
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> ".\\n";
  </verbatim>

  In order to finish, we should again output the matching <verbatim|DATA_END>
  and flush the standard output:\ 

  <\verbatim>
    \ \ \ \ \ \ \ \ next_input ();<format|next line> \ \ \ \ \ \ \ cout
    \<less\>\<less\> DATA_END;<format|next line> \ \ \ \ \ \ \ fflush
    (stdout);<format|next line> \ \ \ \ \ }<format|next line>
    \ \ \ \ \ return 0;<format|next line> \ \ \ }
  </verbatim>

  Notice that you should never output more than one
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block. As soon as the first
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> block has been received by
  <apply|TeXmacs>, it is assumed that your system is waiting for input. If
  you want to send several <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks,
  then they should be enclosed in one main block.

  A special ``channel'' is used in order to send the input prompt. Channels
  are specified as special <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks:\ 

  <\verbatim>
    \ \ \ \ static int counter= 0;<format|next line><format|next line>
    \ \ \ void<format|next line> \ \ \ next_input () {<format|next line>
    \ \ \ \ \ counter++;<format|next line> \ \ \ \ \ cout \<less\>\<less\>
    DATA_BEGIN \<less\>\<less\> "channel:prompt" \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ \ \ cout \<less\>\<less\> "Input "
    \<less\>\<less\> counter \<less\>\<less\> "] ";<format|next line> \ \ \ }
  </verbatim>

  Inside the prompt channel, you may again use
  <verbatim|DATA_BEGIN>-<verbatim|DATA_END> blocks in a nested way. This
  allows you for instance to use a formula as a prompt. There are three
  standard channels:

  <\description>
    <expand|item*|<verbatim|output>.>The default channel for normal output.

    <expand|item*|<verbatim|prompt>.>For sending input prompts.

    <expand|item*|<verbatim|input>.>For specifying a default value for the
    next input.
  </description>

  <section|Graphical output>

  It is possible to send postscript graphics as output. Assume for instance
  that you have a picture <verbatim|picture.ps> in your home directory. Then
  inserting the lines:

  <\verbatim>
    \ \ \ \ \ \ \ \ cout \<less\>\<less\> "A little picture:\\n";<format|next
    line> \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\>
    "ps:";<format|next line> \ \ \ \ \ \ \ fflush (stdout);<format|next line>
    \ \ \ \ \ \ \ system ("cat $HOME/picture.ps");<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ \ \ \ \ cout \<less\>\<less\> "\\n";
  </verbatim>

  at the appropriate place in the main loop will display your image in the
  middle of the output.

  <section|The complete listing>

  <\verbatim>
    #include \<less\>stdio.h\<gtr\><format|next line>#include
    \<less\>stdlib.h\<gtr\><format|next line>#include
    \<less\>string.h\<gtr\><format|next line>#include
    \<less\>iostream.h\<gtr\><format|next line><format|next line>#define
    DATA_BEGIN \ \ ((char) 2)<format|next line>#define DATA_END
    \ \ \ \ ((char) 5)<format|next line>#define DATA_ESCAPE \ ((char)
    27)<format|next line><format|next line>static int counter= 0;<format|next
    line><format|next line>void<format|next line>next_input () {<format|next
    line> \ counter++;<format|next line> \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "channel:prompt" \<less\>\<less\> DATA_END;<format|next
    line> \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";<format|next line>}<format|next line><format|next
    line>int<format|next line>main () {<format|next line> \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ cout \<less\>\<less\> "Welcome to my test computer algebra system
    for TeXmacs\\n";<format|next line> \ cout \<less\>\<less\> "This software
    comes with no warranty whatsoever\\n";<format|next line> \ cout
    \<less\>\<less\> "(c) 2001 \ by Joris van der Hoeven\\n";<format|next
    line> \ cout \<less\>\<less\> "------------------------------------------------------\\n";<format|next
    line> \ next_input ();<format|next line> \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ fflush (stdout);<format|next
    line><format|next line> \ while (1) {<format|next line> \ \ \ char
    buffer[100];<format|next line> \ \ \ cin \<gtr\>\<gtr\>
    buffer;<format|next line> \ \ \ if (strcmp (buffer, "quit") == 0)
    break;<format|next line> \ \ \ cout \<less\>\<less\> DATA_BEGIN
    \<less\>\<less\> "verbatim:";<format|next line> \ \ \ cout
    \<less\>\<less\> "You typed " \<less\>\<less\> buffer \<less\>\<less\>
    "\\n";<format|next line><format|next line> \ \ \ cout \<less\>\<less\>
    "And now a LaTeX formula: ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:" \<less\>\<less\>
    "$x^2+y^2=z^2$" \<less\>\<less\> DATA_END;<format|next line> \ \ \ cout
    \<less\>\<less\> "\\n";<format|next line><format|next line> \ \ \ cout
    \<less\>\<less\> "And finally a fraction ";<format|next line> \ \ \ cout
    \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:" \<less\>\<less\>
    "(frac \\"a\\" \\"b\\")" \<less\>\<less\> DATA_END;<format|next line>
    \ \ \ cout \<less\>\<less\> ".\\n";<format|next line><format|next line>
    \ \ \ next_input ();<format|next line> \ \ \ cout \<less\>\<less\>
    DATA_END;<format|next line> \ \ \ fflush (stdout);<format|next line>
    \ }<format|next line> \ return 0;<format|next line>}
  </verbatim>

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Studying the source code step by
      step><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Graphical
      output><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>The complete
      listing><value|toc-dots><pageref|toc-3><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
