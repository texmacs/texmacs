<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Basic input/output using pipes>

  The configuration and the compilation of the <verbatim|minimal> plug-in is
  <hlink|described|../plugin/plugin-binary.en.tm> in the chapter about
  plug-ins. We will now study the source file
  <example-plugin-link|minimal/src/minimal.cpp>. Essentially, the
  <verbatim|main> routine is given by

  <\cpp-code>
    int

    main () {

    \ \ <em|display-startup-banner>

    \ \ while (true) {

    \ \ \ \ <em|read-input>

    \ \ \ \ <em|display-output>

    \ \ }

    \ \ return 0;

    }
  </cpp-code>

  By default, <TeXmacs> just send a <verbatim|'\\n'>-terminated string to the
  application as the input. Consequently, the code for
  <verbatim|<em|read-input>> is given by

  <\cpp-code>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');
  </cpp-code>

  The output part is more complicated, since <TeXmacs> needs to have a secure
  way for knowing whether the output has finished. This is accomplished by
  encapsulating each piece of output (in our case both the display banner and
  the interactive output) inside a block of the form

  <\quotation>
    <framed-fragment|<verbatim|<render-key|DATA_BEGIN><em|format>:<em|message><render-key|DATA_END>>>
  </quotation>

  Here <verbatim|DATA_BEGIN> and <verbatim|DATA_END> stand for special
  control characters:

  <\cpp-code>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </cpp-code>

  The <verbatim|DATA_ESCAPE> is used for producing the <verbatim|DATA_BEGIN>
  and <verbatim|DATA_END> characters in the <verbatim|<em|message>> using the
  rewriting rules

  <\quotation>
    <\framed-fragment>
      <\with|font-family|tt>
        <tabular|<tformat|<table|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_BEGIN>>|<cell|<math|\<longrightarrow\>>>|<cell|<render-key|DATA_BEGIN>>>|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_END>>|<cell|<math|\<longrightarrow\>>>|<cell|<render-key|DATA_END>>>|<row|<cell|<render-key|DATA_ESCAPE><space|0.6spc><render-key|DATA_ESCAPE>>|<cell|<math|\<longrightarrow\>>>|<cell|<render-key|DATA_ESCAPE>>>>>>
      </with>
    </framed-fragment>
  </quotation>

  The <verbatim|<em|format>> specifies the format of the
  <verbatim|<em|message>>. For instance, in our example, the code of
  <verbatim|<em|display-startup-banner>> is given by

  <\cpp-code>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "Hi there!";

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  Similarly, the code of <verbatim|<em|display-output>> is given by

  <\cpp-code>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    cout.flush ();
  </cpp-code>

  <\remark>
    For synchronization purposes, <TeXmacs> will assume that the output is
    finished as soon as it encounters the <render-key|DATA_END> which closes
    the initial <render-key|DATA_BEGIN>. So all output has to be inside one
    <em|single> outer <render-key|DATA_BEGIN>-<render-key|DATA_END> block: if
    you send more blocks, then <TeXmacs> will retake control before reading
    all your output. It <em|is> possible to nest
    <render-key|DATA_BEGIN>-<render-key|DATA_END> blocks though, as we will
    see below.
  </remark>

  <\remark>
    In our example, the <c++> code for the application is included in the
    plug-in. In the case when you are writing a <TeXmacs> interface for an
    existing application <verbatim|<em|myapp>>, the convention is to create a
    <verbatim|--texmacs> option for this program. Then it is no longer
    necessary to have <verbatim|<em|myapp>/src> and <verbatim|<em|myapp>/bin>
    directories for your plug-in and it suffices to configure the plug-in by
    putting something like the following in
    <verbatim|<em|myapp>/progs/init-<em|myapp>.scm>:

    <\scm-code>
      (plugin-configure <em|myapp>

      \ \ (:require (url-exists-in-path? "<em|myapp>"))

      \ \ (:launch "<em|myapp> --texmacs")

      \ \ (:session "<em|Myapp>"))
    </scm-code>

    In the case when you do not have the possibility to modify the source
    code of <verbatim|<em|myapp>>, you typically have to write an
    input/output filter <verbatim|tm_<em|myapp>> for performing the
    appropriate rewritings. By looking at the standard plug-ins distributed
    with <TeXmacs> in

    <\verbatim>
      \ \ \ \ $TEXMACS_PATH/plugins
    </verbatim>

    you can find several examples of how this can be done.
  </remark>

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