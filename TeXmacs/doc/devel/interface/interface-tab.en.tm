<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Tab-completion>

  By default, <TeXmacs> looks into your document for possible
  tab-completions. Inside sessions for your application, you might wish to
  customize this behaviour, so as to complete built-in commands. In order to
  do this, you have to specify the configuration option

  <\scm-code>
    (:tab-completion #t)
  </scm-code>

  in your <verbatim|init-<em|myplugin>.scm> file, so that <TeXmacs> will send
  special tab-completion requests to your application whenever you press
  <key|tab> inside a session. These commands are of the form

  <\quotation>
    <\framed-fragment>
      <\verbatim>
        <render-key|DATA_COMMAND>(complete <em|input-string>
        <em|cursor-position>)<shortcut|(kbd-return)>
      </verbatim>
    </framed-fragment>
  </quotation>

  Here <verbatim|DATA_COMMAND> stands for the special character
  <verbatim|'\\20'> (ASCII 16). The <verbatim|<em|input-string>> is the
  complete string in which the <key|tab> occurred and the
  <verbatim|<em|cursor-position>> is an integer which specifies the position
  of the cursor when you pressed <key|tab>. <TeXmacs> expects your
  application to return a tuple with all possible tab-completions of the form

  <\quotation>
    <\framed-fragment>
      <verbatim|<render-key|DATA_BEGIN>scheme:(tuple <em|root>
      <em|completion-1> ><math|\<cdots\>><verbatim|
      <em|completion-n>)><render-key|DATA_END>
    </framed-fragment>
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
    a similar way as for the input: we provide a <scm|:commander>
    configuration option for this, which works in a similar way as the
    <scm|:serializer> option.
  </remark>

  <paragraph*|The <verbatim|complete> plug-in>

  A very rudimentary example of how the tab-completion mechanism works is
  given by the <verbatim|complete> plug-in, which consists of the following
  files:

  <\verbatim>
    \ \ \ \ <example-plugin-link|complete/Makefile>

    \ \ \ \ <example-plugin-link|complete/progs/init-complete.scm>

    \ \ \ \ <example-plugin-link|complete/src/complete.cpp>
  </verbatim>

  The startup banner in <verbatim|complete.cpp> takes care of part of the
  configuration:

  <\cpp-code>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    format_plugin ();

    cout \<less\>\<less\> "We know how to complete 'h'";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </cpp-code>

  Here <cpp|format_plugin> is given by

  <\cpp-code>
    void

    format_plugin () {

    \ \ // The configuration of a plugin can be completed at startup time.

    \ \ // This may be interesting for adding tab-completion a posteriori.

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "command:";

    \ \ cout \<less\>\<less\> "(plugin-configure complete (:tab-completion
    #t))";

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-code>

  In the main loop, we first deal with regular input:

  <\cpp-code>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');

    if (buffer[0] != DATA_COMMAND) {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    \ \ cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    \ \ cout \<less\>\<less\> DATA_END;

    }
  </cpp-code>

  We next treat the case when a tab-completion command is sent to the
  application:

  <\cpp-code>
    else {

    \ \ cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "scheme:";

    \ \ cout \<less\>\<less\> "(tuple \\"h\\" \\"ello\\" \\"i there\\"
    \\"ola\\" \\"opsakee\\")";

    \ \ cout \<less\>\<less\> DATA_END;

    }

    cout.flush ();
  </cpp-code>

  As you notice, the actual command is ignored, so our example is really very
  rudimentary.

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