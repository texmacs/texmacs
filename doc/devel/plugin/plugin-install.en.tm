<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Installing and using a plug-in>

  From the user's point of view, a plug-in <verbatim|<em|myplugin>> will
  usually be distributed on some web-site as a binary tarball with the name

  <\verbatim>
    \ \ \ \ <em|myplugin>-<em|version>-<em|architecture>.tar.gz
  </verbatim>

  If you installed <TeXmacs> yourself in the directory
  <verbatim|$TEXMACS_PATH>, then you should unpack this tarball in the
  directory <verbatim|$TEXMACS_PATH/plugins>, using

  <\verbatim>
    \ \ \ \ tar -zxvf <em|myplugin>-<em|version>-<em|architecture>.tar.gz
  </verbatim>

  This will create a <verbatim|<em|myplugin>> subdirectory in
  <verbatim|$TEXMACS_PATH/plugins>. As soon as you restart <TeXmacs>, the
  plug-in should be automatically recognized. Please read the documentation
  which comes with your plug-in in order to learn using it.

  <\remark>
    If you did not install <TeXmacs> yourself, or if you do not have write
    access to <verbatim|$TEXMACS_PATH>, then you may also unpack the tarball
    in <verbatim|$TEXMACS_HOME_PATH/plugins>. Here we recall that
    <verbatim|$TEXMACS_HOME_PATH> defaults to <verbatim|$HOME/.TeXmacs>. When
    starting <TeXmacs>, your plug-in should again be automatically
    recognized.
  </remark>

  <\remark>
    If the plug-in is distributed as a source tarball like
    <verbatim|<em|myplugin>-<em|version>-src.tar.gz>, then you should first
    compile the source code before relaunching <TeXmacs>. Depending on the
    plug-in (read the instructions), this is usually done using

    <\verbatim>
      \ \ \ \ cd <em|myplugin>; make
    </verbatim>

    or

    <\verbatim>
      \ \ \ \ cd <em|myplugin>; ./configure; make
    </verbatim>
  </remark>

  <\remark>
    In order to upgrade a plug-in, just remove the old version in
    <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>
    using

    <\verbatim>
      \ \ \ \ rm -rf <em|myplugin>
    </verbatim>

    and reinstall as explained above.
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