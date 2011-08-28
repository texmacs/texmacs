<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Writing your first interface with <apply|TeXmacs>>

  In order to write your first interface to <apply|TeXmacs>, we recommend you
  to follow the following steps:

  <\enumerate>
    <item>Create a <verbatim|--texmacs> option for your program which will be
    used for calling your program from inside <apply|TeXmacs>.

    <item>Modify your output routines in a such a way that the appropriate
    output is send to <apply|TeXmacs> when your program is started with the
    <verbatim|--texmacs> option.

    <item>Create a <verbatim|mycas> script in your path which executes your
    program with the <verbatim|--texmacs> option.
  </enumerate>

  After doing this, your program will be available under the name
  <apply|menu|Mycas> in <apply|menu|Insert|Session>. We will explain later how
  to make your system listed under its own name, how to customize it, and how
  to get the interface incorporated into the main <apply|TeXmacs>
  distribution.

  Usually, step 2 is the most complicated one and the time it will cost you
  depends on how your system was designed. If you designed clean output
  routines (including the routines for displaying error messages), then it
  usually suffices to modify these by mimicking the <verbatim|mycas> example
  and reusing existing <apply|LaTeX> output routines, which most systems
  provide.

  At the moment, we only implemented <apply|LaTeX> as a standard transmission
  format for mathematical formulas, because this is the format which is most
  widely used. In the future, we intend to implement more semantically secure
  formats, and we recommend you to keep in mind the possibility of sending
  your output in tree format.

  Nevertheless, we enriched standard <apply|LaTeX> with the <verbatim|\\*>
  and <verbatim|\\bignone> commands for multiplication and closing big
  operators. This allows us to distinguish between:\ 

  <\verbatim>
    \ \ \ \ a \\* (b + c)
  </verbatim>

  (or <with|mode|math|a> multiplied by <with|mode|math|b+c>) and:

  <\verbatim>
    \ \ \ \ f(x + y)
  </verbatim>

  (or <with|mode|math|f> applied to <with|mode|math|x+y>). Similarly, in:

  <\verbatim>
    \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
  </verbatim>

  the <verbatim|\\bignone> command is used in order to specify the scopes of
  the <verbatim|\\sum> operators.

  It turns out that the systematic use of the <verbatim|\\*> and
  <verbatim|\\bignone> commands, in combination with clean <apply|LaTeX>
  output for the remaining constructs, makes it <with|font shape|italic|a
  priori> possible to associate an appropriate meaning to your output. In
  particular, this usually makes it possible to write additional routines for
  copying and pasting formulae between different systems.

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
    <associate|idx-1|<tuple|3.|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|3.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Mycas>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
