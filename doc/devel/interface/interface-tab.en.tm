<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Tab-completion>

  By default, <TeXmacs> looks into your document for possible
  tab-completions. Inside sessions for your application, you might wish to
  customize this behaviour, so as to complete built-in commands. In order to
  do this, you have to specify the configuration option

  <\expand|scheme-fragment>
    (:tab-completion #t)
  </expand>

  in your <verbatim|init-<em|myplugin>.scm> file, so that <TeXmacs> will send
  special tab-completion requests to your application whenever you press
  <key|<expand|key-tab>> inside a session. These commands are of the form

  <\quotation>
    <\expand|framed-fragment>
      <\verbatim>
        <key|DATA_COMMAND>(complete <em|input-string>
        <em|cursor-position>)<key|<expand|key-return>>
      </verbatim>
    </expand>
  </quotation>

  Here <verbatim|DATA_COMMAND> stands for the special character
  <verbatim|'\\20'> (ASCII 16). The <verbatim|<em|input-string>> is the
  complete string in which the <key|<expand|key-tab>> occurred and the
  <verbatim|<em|cursor-position>> is an integer which specifies the position
  of the cursor when you pressed <key|<expand|key-tab>>. <TeXmacs> expects
  your application to return a tuple with all possible tab-completions of the
  form

  <\quotation>
    <\expand|framed-fragment>
      <verbatim|<key|DATA_BEGIN>scheme:(tuple <em|root> <em|completion-1>
      ><with|mode|math|\<cdots\>><verbatim| <em|completion-n>)><key|DATA_END>
    </expand>
  </quotation>

  Here <verbatim|<em|root>> corresponds to a substring before the cursor for
  which completions could be found. The strings <verbatim|<em|completion-1>>
  until <verbatim|<em|completion-n>> are the list of completions as they
  might be inserted at the current cursor position. If no completions could
  be found, then you may also return the empty string.

  <\remark>
    In principle, the tab-completion mechanism should still work in
    mathematical input mode. In that case, the <verbatim|<em|input-string>>
    will correspond to the serialization of the <TeXmacs> input.
  </remark>

  <\remark>
    The way <TeXmacs> sends commands to your application can be customized in
    a similar way as for the input: we provide a <verbatim|:commander>
    configuration option for this, which works in a similar way as the
    <verbatim|:serializer> option.
  </remark>

  <paragraph|The <verbatim|complete> plugin>

  A very rudimentary example of how the tab-completion mechanism works is
  given by the <verbatim|complete> plugin, which consists of the following
  files:

  <\verbatim>
    \ \ \ \ <expand|example-plugin-link|complete/Makefile>

    \ \ \ \ <expand|example-plugin-link|complete/progs/init-complete.scm>

    \ \ \ \ <expand|example-plugin-link|complete/src/complete.cpp>
  </verbatim>

  The startup banner in <verbatim|complete.cpp> takes care of part of the
  configuration:

  <\expand|cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    format_plugin ();

    cout \<less\>\<less\> "We know how to complete 'h'";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  Here <expand|cpp-code|format_plugin> is given by

  <\expand|cpp-fragment>
    void

    format_plugin () {

    \ \ // The configuration of a plugin can be completed at startup time.

    \ \ // This may be interesting for adding tab-completion a posteriori.

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "command:";

    \ \ cout \<less\>\<less\> "(plugin-configure complete (:tab-completion
    #t))";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </expand>

  In the main loop, we first deal with regular input:

  <\expand|cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    if (buffer[0] != DATA_COMMAND) {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </expand>

  We next treat the case when a tab-completion command is sent to the
  application:

  <\expand|cpp-fragment>
    else {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:";

    \ \ cout \<less\>\<less\> "(tuple \\"h\\" \\"ello\\" \\"i there\\"
    \\"ola\\" \\"opsakee\\")";

    \ \ cout \<less\>\<less\> DATA_END;

    }

    fflush (stdout);
  </expand>

  As you notice, the actual command is ignored, so our example is really very
  rudimentary.

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
    <associate|toc-1|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|The <with|font
      family|<quote|tt>|language|<quote|verbatim>|complete>
      plugin<value|toc-dots><pageref|toc-1>>
    </associate>
  </collection>
</auxiliary>
