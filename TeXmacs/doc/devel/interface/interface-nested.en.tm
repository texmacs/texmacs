<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Formatted and structured output>

  In the <hlink|previous section|interface-pipes.en.tm>, we have seen that
  output from applications is encapsulated in blocks of the form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  In fact, the <verbatim|<em|message>> may recursively contain blocks of the
  same form. Currently implemented formats include <verbatim|verbatim>,
  <verbatim|latex>, <verbatim|html>, <verbatim|ps>, <verbatim|scheme>. The
  <verbatim|scheme> format is used for sending <TeXmacs> trees in the form of
  <scheme> expressions.

  <paragraph*|The <verbatim|formula> plug-in>

  The <verbatim|formula> plug-in demonstrates the use of <LaTeX> as the
  output format. It consists of the files

  <\verbatim>
    \ \ \ \ <example-plugin-link|formula/Makefile>

    \ \ \ \ <example-plugin-link|formula/progs/init-formula.scm>

    \ \ \ \ <example-plugin-link|formula/src/formula.cpp>
  </verbatim>

  The body of the main loop of <verbatim|formula.cpp> is given by

  <\cpp-code>
    int i, nr;

    cin \<gtr\>\<gtr\> nr;

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$";

    for (i=1; i\<less\>nr; i++)

    \ \ cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}+";

    cout \<less\>\<less\> "x_{" \<less\>\<less\> i \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  Similarly, the use of nested output blocks is demonstrated by the
  <verbatim|nested> plug-in; see in particular the source file
  <example-plugin-link|nested/src/nested.cpp>.

  <\remark>
    At the moment, we only implemented <LaTeX> as a standard transmission
    format for mathematical formulas, because this is the format which is
    most widely used. In the future, we intend to implement more semantically
    secure formats, and we recommend you to keep in mind the possibility of
    sending your output in tree format.

    Nevertheless, we enriched standard <LaTeX> with the <verbatim|\\*> and
    <verbatim|\\bignone> commands for multiplication and closing big
    operators. This allows us to distinguish between

    <\verbatim>
      \ \ \ \ a \\* (b + c)
    </verbatim>

    (<abbr|i.e.> <math|a> multiplied by <math|b+c>) and

    <\verbatim>
      \ \ \ \ f(x + y)
    </verbatim>

    (<abbr|i.e.> <math|f> applied to <math|x+y>). Similarly, in

    <\verbatim>
      \ \ \ \ \\sum_{i=1}^m a_i \\bignone + \\sum_{j=1}^n b_j \\bignone
    </verbatim>

    the <verbatim|\\bignone> command is used in order to specify the scopes
    of the <verbatim|\\sum> operators.

    It turns out that the systematic use of the <verbatim|\\*> and
    <verbatim|\\bignone> commands, in combination with clean <LaTeX> output
    for the remaining constructs, makes it <with|font-shape|italic|a priori>
    possible to associate an appropriate meaning to your output. In
    particular, this usually makes it possible to write additional routines
    for copying and pasting formulae between different systems.
  </remark>

  <paragraph*|The <verbatim|markup> plug-in>

  It is important to remind that structured output can be combined with the
  power of <TeXmacs> as a structured editor. For instance, the
  <verbatim|markup> plug-in demonstrates the definition of an additional tag
  <markup|foo>, which is used as an additional primitive in the output of the
  application. More precisely, the <verbatim|markup> plug-in consists of the
  following files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|markup/Makefile>

    \ \ \ \ <example-plugin-link|markup/packages/session/markup.ts>

    \ \ \ \ <example-plugin-link|markup/progs/init-markup.scm>

    \ \ \ \ <example-plugin-link|markup/src/markup.cpp>
  </verbatim>

  The style package <tmpackage|markup.ts> contains the following definition
  for <markup|foo>:

  <\tm-fragment>
    <inactive*|<math|<assign|foo|<macro|x|<frac|1|1+<arg|x>>>>>>
  </tm-fragment>

  The <markup|foo> tag is used in the following way in the body of the main
  loop of <verbatim|markup.cpp>:

  <\cpp-code>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "latex:";

    cout \<less\>\<less\> "$\\\\foo{" \<less\>\<less\> buffer
    \<less\>\<less\> "}$";

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  Notice that the style package <tmpackage|markup.ts> also defines the
  <markup|markup-output> environment:

  <\tm-fragment>
    <inactive*|<assign|markup-output|<macro|body|<generic-output|<with|par-mode|center|<arg|body>>>>>>
  </tm-fragment>

  This has the effect of centering the output in sessions started using
  <menu|Insert|Session|Markup>.

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