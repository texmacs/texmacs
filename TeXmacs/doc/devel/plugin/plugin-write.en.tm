<TeXmacs|2.1.2>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Writing your own plug-ins>

  In order to write a plug-in <verbatim|<em|myplugin>>, you should start by
  creating a directory

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>
  </verbatim>

  where to put all your files (recall that <verbatim|$TEXMACS_HOME_PATH>
  defaults to <verbatim|$HOME/.TeXmacs>). In addition, you may create the
  following subdirectories (when needed):

  <\description-dash>
    <item*|<verbatim|bin>>For binary files.

    <item*|<verbatim|doc>>For documentation.

    <item*|<verbatim|langs>>For language related files, such as dictionaries
    (not yet supported).

    <item*|<verbatim|lib>>For libraries.

    <item*|<verbatim|packages>>For style packages.

    <item*|<verbatim|progs>>For <scheme> programs.

    <item*|<verbatim|src>>For source files.

    <item*|<verbatim|styles>>For style files.
  </description-dash>

  As a general rule, files which are present in these subdirectories will be
  automatically recognized by <TeXmacs> at startup. For instance, if you
  provide a <verbatim|bin> subdirectory, then

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/bin
  </verbatim>

  will be automatically added to the <verbatim|PATH> environment variable at
  startup. Notice that the subdirectory structure of a plug-in is very
  similar to the subdirectory structure of <verbatim|$TEXMACS_PATH>.

  Similarly, plugin documentation is intended to be automatically added to
  the <menu|Help|Plug-ins> submenu. For this to automation to work, the
  <verbatim|myplugin/doc/> directory should contain at least two files

  <\verbatim>
    \ \ \ \ myplugin.en.tm

    \ \ \ \ myplugin-abstract.en.tm
  </verbatim>

  The first file is the main entry point to the plugin's documentation and
  should follow <hlink|the general conventions for structuring <TeXmacs>
  documentation|../../about/contribute/documentation/traversal.en.tm>. The
  <verbatim|-abstract> file provides a short description of the plugin's
  functionality.

  <\example>
    The easiest type of plug-in only consists of data files, such as a
    collection of style files and packages. In order to create such a
    plug-in, it suffices to create directories

    <\verbatim>
      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/styles

      \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/packages
    </verbatim>

    and to put your style files and packages in the last two directories.
    After restarting <TeXmacs>, your style files and packages will
    automatically appear in the <menu|Document|Style> and <menu|Document|Use
    package> menus.
  </example>

  For more complex plug-ins, such as plug-ins with additional <scheme> or
  <c++> code, one usually has to provide a <scheme> configuration file

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/<em|myplugin>/progs/init-<em|myplugin>.scm
  </verbatim>

  This configuration file should contain an instruction of the following form

  <\scm-code>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scm-code>

  Here the <verbatim|<em|configuration-options>> describe the principal
  actions which have to be undertaken at startup, including sanity checks for
  the plug-in. In the next sections, we will describe some simple examples of
  plug-ins and their configuration. Many other examples can be found in the
  directories

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/examples/plugins

    \ \ \ \ $TEXMACS_PATH/plugins
  </verbatim>

  Some of these are <hlink|described|../interface/interface.en.tm> in more
  detail in the chapter about writing new interfaces.

  <tmdoc-copyright|1998\U2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>