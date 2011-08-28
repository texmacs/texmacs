<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Supporting your system inside <apply|TeXmacs>>

  Assume that you have successfully written a first interface with
  <apply|TeXmacs> as explained in the previous section. Then it is time now
  to include support for your system in the standard <apply|TeXmacs>
  distribution, after which further improvements can be made.

  From version 1.0.1.5 on, it has become quite easy to adapt your interface
  in such a way that it can be directly integrated into <TeXmacs>. The idea
  is to create a directory:

  <\verbatim>
    \ \ \ \ $TEXMACS_HOME_PATH/plugins/myplugin
  </verbatim>

  where <verbatim|myplugin> is the name of your plugin. Here we recall that
  <verbatim|$TEXMACS_HOME_PATH> is bound to <verbatim|~/.TeXmacs>, by
  default. In the directory <verbatim|$TEXMACS_PATH/plugins> you can find all
  standard plugins, which are shipped with your <TeXmacs> distribution. These
  provide good examples which you may imitate.

  The above <verbatim|myplugin> directory should contain a similar
  subdirectory structure as <verbatim|$TEXMACS_PATH> itself, but you may omit
  directories which you do not actually use. In any case, you need to provide
  a file <verbatim|progs/init-myplugin.scm> which describes how to initialize
  your plugin. Usually, this file contains just one <name|Scheme> instruction
  of the following form:

  <\verbatim>
    \ \ \ \ (plugin-configure myplugin<format|next line> \ \ \ \ \ (:require
    (file-in-path "myplugin"))<format|next line> \ \ \ \ \ (:launch
    "shell-cmd")<format|next line> \ \ \ \ \ (:format "input-format"
    "output-format")<format|next line> \ \ \ \ \ (:session "Myplugin"))
  </verbatim>

  The first instruction is a predicate which checks whether your plugin can
  be used on a particular system. Usually, it tests whether a certain program
  is available in the path. The remainder of the instructions is only
  executed if the requirement is fulfilled. The <verbatim|:launch>
  instruction specifies that your plugin should be launched using
  <verbatim|shell-cmd>. The command <verbatim|shell-cmd> is usually of the
  form <verbatim|myplugin --texmacs>. The <verbatim|:format> instruction
  specifies which formats are used for the input and output. Usually, the
  <verbatim|input-format> is <verbatim|verbatim> and the
  <verbatim|output-format> is <verbatim|generic>. Other available formats are
  <verbatim|scheme>, <verbatim|latex>, <verbatim|html> and <verbatim|ps>. The
  <verbatim|:session> instruction makes shell sessions available for your
  plugin from the menu <apply|menu|Insert|Session|Myplugin>.

  If everything works well, and you wish to make it possible for others to
  use your system inside the official <apply|TeXmacs> distribution, then
  contact me at <verbatim|vdhoeven@texmacs.org>.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Myplugin>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
