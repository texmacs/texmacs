<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Installing and using a plugin>

  From the user's point of view, a plugin <verbatim|<em|myplugin>> will
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
  plugin should be automatically recognized. Please read the documentation
  which comes with your plugin in order to learn using it.

  <\remark>
    If you did not install <TeXmacs> yourself, or if you do not have write
    access to <verbatim|$TEXMACS_PATH>, then you may also unpack the tarball
    in <verbatim|$TEXMACS_HOME_PATH/plugins>. Here we recall that
    <verbatim|$TEXMACS_HOME_PATH> defaults to <verbatim|$HOME/.TeXmacs>. When
    starting <TeXmacs>, your plugin should again be automatically recognized.
  </remark>

  <\remark>
    If the plugin is distributed as a source tarball like
    <verbatim|<em|myplugin>-<em|version>-src.tar.gz>, then you should first
    compile the source code before relaunching <TeXmacs>. Depending on the
    plugin (read the instructions), this is usually done using

    <\verbatim>
      \ \ \ \ cd <em|myplugin>; make
    </verbatim>

    or

    <\verbatim>
      \ \ \ \ cd <em|myplugin>; ./configure; make
    </verbatim>
  </remark>

  <\remark>
    In order to upgrade a plugin, just remove the old version in
    <verbatim|$TEXMACS_PATH/plugins> or <verbatim|$TEXMACS_HOME_PATH/plugins>
    using

    <\verbatim>
      \ \ \ \ rm -rf <em|myplugin>
    </verbatim>

    and reinstall as explained above.
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
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
  </collection>
</references>
