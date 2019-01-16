<TeXmacs|1.99.8>

<style|<tuple|tmdoc|english|old-spacing>>

<\body>
  <tmdoc-title|Summary of the configuration options for plug-ins>

  As explained before, the <scheme> configuration file
  <verbatim|<em|myplugin>/progs/init-<em|myplugin>.scm> of a plug-in with
  name <verbatim|<em|plugin>> should contain an instruction of the type

  <\scm-code>
    (plugin-configure <em|myplugin>

    \ \ <em|configuration-options>)
  </scm-code>

  Here follows a list of the available <verbatim|<em|configuration-options>>:

  <\description-dash>
    <item*|<verbatim|<with|font-series|medium|(:winpath <em|package-path>
    <em|inner-bin-path>)>>>Specify where to search for the plug-in under
    windows. The <verbatim|<em|package-path>> is the usual place where the
    plug-in is installed. The <verbatim|<em|inner-bin-path>> is the place
    where to look for the binary executable corresponding to the plug-in,
    relative to the <verbatim|<em|package-path>>.

    <item*|<verbatim|<with|font-series|medium|(:winpath <em|package-path>
    <em|inner-bin-path>)>>>Analogous to <verbatim|:winpath>, but under
    <name|MacOS>.

    <item*|<verbatim|<with|font-series|medium|(:require
    <em|condition>)>>>This option specifies a sanity
    <verbatim|<em|condition>> which needs to be satisfied by the plug-in.
    Usually, it is checked that certain binaries or libraries are present on
    your system. If the condition fails, then <TeXmacs> will continue as
    whether your plug-in did not exist. In that case, further configuration
    is aborted. The <verbatim|:require> option usually occurs first in the
    list of configuration options.

    <item*|<verbatim|<with|font-series|medium|(:versions
    <em|version-cmd>)>>>This option specifies a <scheme> expression
    <verbatim|<em|version-cmd>> which evaluates to a list of available
    versions of the plug-in.

    <item*|<verbatim|<with|font-series|medium|(:setup <em|cmd>)>>>This
    command is only executed when the version of the plug-in changed from one
    execution of <TeXmacs> to another one. This occurs mainly when installing
    new versions of <TeXmacs> or helper applications.

    <item*|<verbatim|<with|font-series|medium|(:launch <em|shell-cmd>)>>>This
    option specifies that the plug-in is able to evaluate expressions over a
    pipe, using a helper application which is launched using the
    shell-command <verbatim|<em|shell-cmd>>.

    <item*|<verbatim|<with|font-series|medium|(:link <em|lib-name>
    <em|export-struct> <em|options>)>>>This option is similar to
    <verbatim|:launch>, except that the extern application is now linked
    dynamically. For more information, see the section about <hlink|dynamic
    linking|../interface/interface-dynlibs.en.tm>.

    <item*|<verbatim|<with|font-series|medium|(:session
    <em|menu-name>)>>>This option indicates that the plug-in supports an
    evaluator for interactive shell sessions. An item
    <verbatim|<em|menu-item>> will be inserted to the <menu|Insert|Session>
    menu in order to launch such sessions.

    <item*|<verbatim|<with|font-series|medium|(:serializer
    ,<em|fun-name>)>>>If the plug-in can be used as an evaluator, then this
    option specifies the <scheme> function <verbatim|<em|fun-name>> which is
    used in order to transform <TeXmacs> trees to strings.

    <item*|<verbatim|<with|font-series|medium|(:commander
    ,<em|fun-name>)>>>This command is similar to the <verbatim|:serializer>
    option except that it is used to transform special commands to strings.

    <item*|<verbatim|<with|font-series|medium|(:tab-completion #t)>>>This
    command indicates that the plug-in supports tab-completion.

    <item*|<verbatim|<with|font-series|medium|(:test-input-done #t)>>>This
    command indicates that the plug-in provides a routine for testing whether
    the input is complete.
  </description-dash>

  It should be noticed that the configuration of the plug-in
  <verbatim|<em|myplugin>> automatically creates a few predicates:

  <\description>
    <item*|<with|font-series|medium|<verbatim|supports-<em|myplugin>?>>>Test
    whether the plug-in is fully operational (all requirements are met).

    <item*|<with|font-series|medium|<verbatim|in-<em|myplugin>?>>>Test
    whether <verbatim|<em|myplugin>> is the current programming language.

    <item*|<with|font-series|medium|<verbatim|<em|myplugin>-scripts?>>>Test
    whether <verbatim|<em|myplugin>> is the current scripting language.
  </description>

  <tmdoc-copyright|1998--2013|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>