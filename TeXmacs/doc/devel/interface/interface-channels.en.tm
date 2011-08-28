<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Output channels, prompts and default input>

  Besides blocks of the form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  the <TeXmacs> meta-format also allows you to use blocks of the form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|channel>#<em|message><render-key|DATA_END>>>
  </quotation>

  Here <verbatim|<em|channel>> specifies an ``output channel'' to which the
  body <verbatim|<em|message>> has to be sent. The default output channel is
  <verbatim|output>, but we also provide channels <verbatim|prompt> and
  <verbatim|input> for specifying the prompt and a default input for the next
  input in a session. Default inputs may be useful for instance be useful for
  demo modes of computer algebra systems. In the future, we also plan to
  support <verbatim|error> and <verbatim|status> channels.

  <paragraph*|The <verbatim|prompt> plug-in>

  The <verbatim|prompt> plug-in shows how to use prompts. It consists of the
  files

  <\verbatim>
    \ \ \ \ <example-plugin-link|prompt/Makefile>

    \ \ \ \ <example-plugin-link|prompt/progs/init-prompt.scm>

    \ \ \ \ <example-plugin-link|prompt/src/prompt.cpp>
  </verbatim>

  The routine for displaying the next prompt is given by

  <\cpp-fragment>
    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "prompt#";

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-fragment>

  This routine is both used for displaying the startup banner

  <\cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "A LaTeX -\<gtr\> TeXmacs converter";

    next_input ();

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

  and in the body of the main loop

  <\cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    next_input ();

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-fragment>

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
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|sfactor|4>
  </collection>
</initial>