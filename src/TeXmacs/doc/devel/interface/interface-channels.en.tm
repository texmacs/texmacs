<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Output channels, prompts and default input>

  Besides blocks of the form

  <\quotation>
    <expand|framed-fragment|<verbatim|<key|DATA_BEGIN><em|format>:<em|message><key|DATA_END>>>
  </quotation>

  the <TeXmacs> meta-format also allows you to use blocks of the form

  <\quotation>
    <expand|framed-fragment|<verbatim|<key|DATA_BEGIN><em|channel>#<em|message><key|DATA_END>>>
  </quotation>

  Here <verbatim|<em|channel>> specifies an ``output channel'' to which the
  body <verbatim|<em|message>> has to be sent. The default output channel is
  <verbatim|output>, but we also provide channels <verbatim|prompt> and
  <verbatim|input> for specifying the prompt and a default input for the next
  input in a session. Default inputs may be useful for instance be useful for
  demo modes of computer algebra systems. In the future, we also plan to
  support <verbatim|error> and <verbatim|status> channels.

  <paragraph|The <verbatim|prompt> plugin>

  The <verbatim|prompt> plugin shows how to use prompts. It consists of the
  files

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|prompt/Makefile>

    \ \ \ \ <expand|example-plugin-link|prompt/progs/init-prompt.scm>

    \ \ \ \ <expand|example-plugin-link|prompt/src/prompt.cpp>
  </verbatim>

  The routine for displaying the next prompt is given by

  <\expand|cpp-fragment>
    void

    next_input () {

    \ \ counter++;

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "prompt#";

    \ \ cout \<less\>\<less\> "Input " \<less\>\<less\> counter
    \<less\>\<less\> "] ";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </expand>

  This routine is both used for dislaying the startup banner

  <\expand|cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "A LaTeX -\<gtr\> TeXmacs converter";

    next_input ();

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  and in the body of the main loop

  <\expand|cpp-fragment>
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
  </expand>

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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>
