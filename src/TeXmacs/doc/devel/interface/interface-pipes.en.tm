<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Basic input/output using pipes>

  The configuration and the compilation of the <verbatim|minimal> plugin is
  <apply|hyper-link|described|../plugin/plugin-binary.en.tm> in the chapter
  about plugins. We will now study the source file
  <expand|example-plugin-link|minimal/src/minimal.cpp>. Essentially, the
  <verbatim|main> routine is given by

  <\expand|cpp-fragment>
    int

    main () {

    \ \ <em|display-startup-banner>

    \ \ while (true) {

    \ \ \ \ <em|read-input>

    \ \ \ \ <em|display-output>

    \ \ }

    \ \ return 0;

    }
  </expand>

  By default, <TeXmacs> just send a <verbatim|'\\n'>-terminated string to the
  application as the input. Consequently, the code for
  <verbatim|<em|read-input>> is given by

  <\expand|cpp-fragment>
    char buffer[100];

    cin.getline (buffer, 100, '\\n');
  </expand>

  The output part is more complicated, since <TeXmacs> needs to have a secure
  way for knowing whether the output has finished. This is accomplished by
  encapsulating each piece of output (in our case both the display banner and
  the interactive output) inside a block of the form

  <\quotation>
    <expand|framed-fragment|<verbatim|<key|DATA_BEGIN><em|format>:<em|message><key|DATA_END>>>
  </quotation>

  Here <verbatim|DATA_BEGIN> and <verbatim|DATA_END> stand for special
  control characters:

  <\expand|cpp-fragment>
    #define DATA_BEGIN \ \ ((char) 2)

    #define DATA_END \ \ \ \ ((char) 5)

    #define DATA_ESCAPE \ ((char) 27)
  </expand>

  The <verbatim|DATA_ESCAPE> is used for producing the <verbatim|DATA_BEGIN>
  and <verbatim|DATA_END> characters in the <verbatim|<em|message>> using the
  rewriting rules

  <\quotation>
    <\expand|framed-fragment>
      <\with|font family|tt>
        <tabular|<tformat|<table|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_BEGIN>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_BEGIN>>>|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_END>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_END>>>|<row|<cell|<key|DATA_ESCAPE><space|0.6spc><key|DATA_ESCAPE>>|<cell|<with|mode|math|\<longrightarrow\>>>|<cell|<key|DATA_ESCAPE>>>>>>
      </with>
    </expand>
  </quotation>

  The <verbatim|<em|format>> specifies the format of the
  <verbatim|<em|message>>. For instance, in our example, the code of
  <verbatim|<em|display-startup-banner>> is given by

  <\expand|cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "Hi there!";

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  Similarly, the code of <verbatim|<em|display-output>> is given by

  <\expand|cpp-fragment>
    cout \<less\>\<less\> DATA_BEGIN \<less\>\<less\> "verbatim:";

    cout \<less\>\<less\> "You typed " \<less\>\<less\> buffer;

    cout \<less\>\<less\> DATA_END;

    fflush (stdout);
  </expand>

  <\remark>
    In our example, the <value|cpp> code for the application is included in
    the plugin. In the case when you are writing a <TeXmacs> interface for an
    existing application <verbatim|<em|myapp>>, the convention is to create a
    <verbatim|--texmacs> option for this program. Then it is no longer
    necessary to have <verbatim|<em|myapp>/src> and <verbatim|<em|myapp>/bin>
    directories for your plugin and it suffices to configure the plugin by
    putting something like the following in
    <verbatim|<em|myapp>/progs/init-<em|myapp>.scm>:

    <\expand|scheme-fragment>
      (plugin-configure <em|myapp>

      \ \ (:require (url-exists-in-path? "<em|myapp>"))

      \ \ (:launch "<em|myapp> --texmacs")

      \ \ (:session "<em|Myapp>"))
    </expand>

    In the case when you do not have the possibility to modify the source
    code of <verbatim|<em|myapp>>, you typically have to write an
    input/output filter <verbatim|tm_<em|myapp>> for performing the
    appropriate rewritings. By looking at the standard plugins distributed
    with <TeXmacs> in

    <\verbatim>
      \ \ \ \ $TEXMACS_PATH/plugins
    </verbatim>

    you can find several examples of how this can be done.
  </remark>

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
