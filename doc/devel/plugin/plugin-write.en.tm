<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Writing your own plugins>

  In order to write a plugin <verbatim|<em|myplugin>>, you should start by
  creating a directory

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>
  </verbatim>

  where to put all your files (recall that <verbatim|$TEXMACS_HOME_PATH>
  defaults to <verbatim|$HOME/.TeXmacs>). In addition, you may create the
  following subdirectories (when needed):

  <\expand|description-dash>
    <expand|item*|<verbatim|bin>>For binary files.

    <expand|item*|<verbatim|doc>>For documentation (not yet supported).

    <expand|item*|<verbatim|langs>>For language related files, such as
    dictionaries (not yet supported).

    <expand|item*|<verbatim|lib>>For libraries.

    <expand|item*|<verbatim|packages>>For style packages.

    <expand|item*|<verbatim|progs>>For <value|scheme> programs.

    <expand|item*|<verbatim|src>>For source files.

    <expand|item*|<verbatim|styles>>For style files.
  </expand>

  As a general rule, files which are present in these subdirectories will be
  automatically recognized by <TeXmacs> at startup. For instance, if you
  provide a <verbatim|bin> subdirectory, then

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/bin
  </verbatim>

  will be automatically added to the <verbatim|PATH> environment variable at
  startup. Notice that the subdirectory structure of a plugin is very similar
  to the subdirectory structure of <verbatim|$TEXMACS_PATH>.

  <\example>
    The easiest type of plugin only consists of data files, such as a
    collection of style files and packages. In order to create such a plugin,
    it suffices to create directories

    <\verbatim>
      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/styles

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/packages
    </verbatim>

    and to put your style files and packages in the last two directories.
    After rebooting <TeXmacs>, your style files and packages will
    automatically appear in the <apply|menu|Document|Style> and
    <apply|menu|Document|Use package> menus.
  </example>

  For more complex plugins, such as plugins with additional <value|scheme> or
  <value|cpp> code, one usually has to provide a <value|scheme> configuration
  file

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/progs/init-<em|myplugin>.scm
  </verbatim>

  This configuration file should contain an instruction of the following form

  <\expand|scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </expand>

  Here the <verbatim|<em|configuration-options>> describe the principal
  actions which have to be undertaken at startup, including sanity checks for
  the plugin. In the next sections, we will describe some simple examples of
  plugins and their configuration. Many other examples can be found in the
  directories

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins

    \ \ \ \ $TEXMACS_PATH/plugins
  </verbatim>

  Some of these are <apply|hyper-link|described|../interface/interface.en.tm>
  in more detail in the chapter about writing new interfaces.

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
    <associate|idx-2|<tuple|1|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Style>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Document>|<with|font
      family|<quote|ss>|Use package>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
