<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Formatted and structured output>

  In the <apply|hyper-link|previous section|interface-pipes.en.tm>, we have
  seen that output from applications is encapsulated in blocks of the form

  <\quotation>
    <expand|framed-fragment|<verbatim|<key|DATA_BEGIN><em|format>:<em|message><key|DATA_END>>>
  </quotation>

  In fact, the <verbatim|<em|message>> may recursively contain blocks of the
  same form. Currently implemented formats include <verbatim|verbatim>,
  <verbatim|latex>, <verbatim|html>, <verbatim|ps>, <verbatim|scheme>. The
  <verbatim|scheme> format is used for sending <TeXmacs> trees in the form of
  <value|scheme> expressions.

  <paragraph|The <verbatim|formula> plugin>

  The <verbatim|formula> plugin demonstrates the use of <LaTeX> as the output
  format. It consists of the files

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|formula/Makefile>

    \ \ \ \ <expand|example-plugin-link|formula/progs/init-formula.scm>

    \ \ \ \ <expand|example-plugin-link|formula/src/formula.cpp>
  </verbatim>

  The body of the main loop of <verbatim|formula.cpp> is given by

  <\expand|cpp-fragment>
    int i, nr;

    cin \<gtr\>\<gtr\> nr;

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$";

    for (i=1; i\<less\>nr; i++)

    \ \ cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}+";

    cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  Similarly, the use of nested output blocks is demonstrated by the
  <verbatim|nested> plugin; see in particular the source file
  <expand|example-plugin-link|nested/src/nested.cpp>.

  <\remark>
    At the moment, we only implemented <apply|LaTeX> as a standard
    transmission format for mathematical formulas, because this is the format
    which is most widely used. In the future, we intend to implement more
    semantically secure formats, and we recommend you to keep in mind the
    possibility of sending your output in tree format.

    Nevertheless, we enriched standard <apply|LaTeX> with the <verbatim|\\*>
    and <verbatim|\\bignone> commands for multiplication and closing big
    operators. This allows us to distinguish between

    <\verbatim>
      \ \ \ \ a \\* (b + c)
    </verbatim>

    (<abbr|i.e.> <with|mode|math|a> multiplied by <with|mode|math|b+c>) and

    <\verbatim>
      \ \ \ \ f(x + y)
    </verbatim>

    (<abbr|i.e.> <with|mode|math|f> applied to <with|mode|math|x+y>).
    Similarly, in

    <\verbatim>
      \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
    </verbatim>

    the <verbatim|\\bignone> command is used in order to specify the scopes
    of the <verbatim|\\sum> operators.

    It turns out that the systematic use of the <verbatim|\\*> and
    <verbatim|\\bignone> commands, in combination with clean <apply|LaTeX>
    output for the remaining constructs, makes it <with|font shape|italic|a
    priori> possible to associate an appropriate meaning to your output. In
    particular, this usually makes it possible to write additional routines
    for copying and pasting formulae between different systems.
  </remark>

  <paragraph|The <verbatim|markup> plugin>

  It is important to remind that structured output can be combined with the
  power of <TeXmacs> as a structured editor. For instance, the
  <verbatim|markup> plugin demonstrates the definition of an additional tag
  <markup|foo>, which is used as an additional primitive in the output of the
  application. More precisely, the <verbatim|markup> plugin consists of the
  following files:

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|markup/Makefile>

    \ \ \ \ <expand|example-plugin-link|markup/packages/session/markup.ts>

    \ \ \ \ <expand|example-plugin-link|markup/progs/init-markup.scm>

    \ \ \ \ <expand|example-plugin-link|markup/src/markup.cpp>
  </verbatim>

  The style package <tmpackage|markup.ts> contains the following definition
  for <markup|foo>:

  <\expand|tm-fragment>
    <with|preamble|true|<with|mode|math|<assign|foo|<func|x|<frac|1|1+<apply|x>>>>>>
  </expand>

  The <markup|foo> tag is used in the following way in the body of the main
  loop of <verbatim|markup.cpp>:

  <\expand|cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$\\\\foo{" \<less\>\<less\> buffer
    \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  Notice that the style package <tmpackage|markup.ts> also defines the
  <markup|markup-output> environment:

  <\expand|tm-fragment>
    <with|preamble|true|<assign|markup-output|<macro|body|<expand|generic-output|<with|paragraph
    mode|center|<arg|body>>>>>>
  </expand>

  This has the effect of centering the output in sessions started using
  <apply|menu|Text|Session|Markup>.

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
    <associate|idx-1|<tuple|1|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|toc-3|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
    <associate|idx-4|<tuple|1|?>>
    <associate|idx-5|<tuple|1|?>>
    <associate|idx-6|<tuple|1|?>>
    <associate|idx-7|<tuple|1|?>>
  </collection>
</references>
