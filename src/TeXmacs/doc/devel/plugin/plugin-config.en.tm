<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Summary of the configuration options for plugins>

  As explained before, the <value|scheme> configuration file
  <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm> of a plugin with name
  <verbatim|<em|plugin>> should contain an instruction of the type

  <\expand|scheme-fragment>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </expand>

  Here follows a list of the available <verbatim|<em|configuration-options>>:

  <\expand|description-dash>
    <expand|item*|<verbatim|<with|font series|medium|(:require
    <em|condition>)>>>This option specifies a sanity
    <verbatim|<em|condition>> which needs to be satisfied by the plugin.
    Usually, it is checked that certain binaries or libraries are present on
    your system. If the condition fails, then <TeXmacs> will continue as
    whether your plugin did not exist. In that case, further configuration is
    aborted. The <verbatim|:require> option usually occurs first in the list
    of configuration options.

    <expand|item*|<verbatim|<with|font series|medium|(:version
    <em|version-cmd>)>>>This option specifies a <value|scheme> expression
    <verbatim|<em|version-cmd>> which evaluates to the version of the plugin.

    <expand|item*|<verbatim|<with|font series|medium|(:setup <em|cmd>)>>>This
    command is only executed when the version of the plugin changed from one
    execution of <TeXmacs> to another one. This occurs mainly when installing
    new versions of <TeXmacs> or helper applications.

    <expand|item*|<verbatim|<with|font series|medium|(:initialize
    <em|cmd>)>>>This option executes the <value|scheme> expression
    <verbatim|<em|cmd>>. It usually occurs just after the <verbatim|:require>
    option, so that the plugin will only be configured if the plugin really
    exists. For large plugins, it is important to keep the file
    <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm> small, because it
    will be rerun each time you start <TeXmacs>. In order to reduce the boot
    time, most <value|scheme> commands of the plugin therefore occur in
    separate modules, some of which may be loaded by the initialization
    command.

    <expand|item*|<verbatim|<with|font series|medium|(:launch
    <em|shell-cmd>)>>>This option specifies that the plugin is able to
    evaluate expressions over a pipe, using a helper application which is
    launched using the shell-command <verbatim|<em|shell-cmd>>.

    <expand|item*|<verbatim|<with|font series|medium|(:link <em|lib-name>
    <em|export-struct> <em|options>)>>>This option is similar to
    <verbatim|:launch>, except that the extern application is now linked
    dynamically. For more information, see the section about
    <apply|hyper-link|dynamic linking|../interface/interface-dynlibs.en.tm>.

    <expand|item*|<verbatim|<with|font series|medium|(:session
    <em|menu-name>)>>>This option indicates that the plugin supports an
    evaluator for interactive shell sessions. An item
    <verbatim|<em|menu-item>> will be inserted to the
    <apply|menu|Text|Session> menu in order to launch such sessions.

    <expand|item*|<verbatim|<with|font series|medium|(:serializer
    ,<em|fun-name>)>>>If the plugin can be used as an evaluator, then this
    option specifies the <value|scheme> function <verbatim|<em|fun-name>>
    which is used in order to transform <TeXmacs> trees to strings.

    <expand|item*|<verbatim|<with|font series|medium|(:commander
    ,<em|fun-name>)>>>This command is similar to the <verbatim|:serializer>
    option except that it is used to transform special commands to strings.

    <expand|item*|<verbatim|<with|font series|medium|(:tab-completion
    #t)>>>This command indicates that the plugin supports tab-completion.

    <expand|item*|<verbatim|<with|font series|medium|(:test-input-done
    #t)>>>This command indicates that the plugin provides a routine for
    testing whether the input is complete.
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
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|1|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
