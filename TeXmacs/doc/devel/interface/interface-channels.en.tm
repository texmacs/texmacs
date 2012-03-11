<TeXmacs|1.0.7.14>

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

  <\cpp-code>
    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "prompt#";

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-code>

  This routine is both used for displaying the startup banner

  <\cpp-code>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "A LaTeX -\<gtr\> TeXmacs converter";

    next_input ();

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  and in the body of the main loop

  <\cpp-code>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> DATA_BEGIN;

    cout \<less\>\<less\> "latex:$" \<less\>\<less\> buffer \<less\>\<less\>
    "$";

    cout \<less\>\<less\> DATA_END;

    next_input ();

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

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