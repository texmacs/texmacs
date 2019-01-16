<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|The module system and lazy definitions>

  As explained above, each <scheme> file inside <TeXmacs> or one of its
  plug-ins corresponds to a <em|<TeXmacs> module>. The individual <TeXmacs>
  modules are usually grouped together into an internal or external module,
  which corresponds to a directory on your hard disk.

  Any <TeXmacs> module should start with an instruction of the form

  <\scm-code>
    (texmacs-module <em|name>

    \ \ (:use <em|submodule-1> ... <em|submodule-n>))
  </scm-code>

  The <verbatim|<em|name>> of the module is a list which corresponds to the
  location of the corresponding file. More precisely, <TeXmacs> searches for
  its modules in the path <verbatim|$GUILE_LOAD_PATH>, which defaults to the
  standard <name|Guile> load path, combined with
  <verbatim|$TEXMACS_PATH/progs> and all <verbatim|progs> subdirectories in
  the plug-ins. For instance, the module <verbatim|(math math-edit)>
  corresponds to the file

  <verbatim| \ \ \ $TEXMACS_PATH/progs/edit/math-edit.scm>

  The user should explicitly specify all submodules on which the module
  depends, except those modules which are loaded by default, <abbr|i.e.> all
  language extensions and utilities in the directories

  <verbatim| \ \ \ $TEXMACS_PATH/progs/kernel<new-line>
  \ \ \ $TEXMACS_PATH/progs/utils/library>

  All symbols which are defined inside the module using <scm|define> or
  <scm|define-macro> are only visible within the module itself. In order to
  make the symbol publicly visible you should use <scm|tm-define> or
  <scm|tm-define-macro>. Currently, because of implementation details for the
  <hlink|contextual overloading system|overview-overloading.en.tm>, as soon
  as a symbol is declared to be public, it becomes visible inside all other
  modules. However, you should not rely on this: in the future, explicit
  importation with <scm|:use> might become<nbsp>necessary.

  Because the number of <TeXmacs> modules and plug-ins keeps on growing, it
  is inefficient to load all modules when booting. Instead, initialization
  files are assumed to declare the provided functionality in a <em|lazy> way:
  whenever the functionality is explicitly needed, <TeXmacs> is triggered to
  load the corresponding modules (if this was not already done). In addition,
  <TeXmacs> may load some of these modules during spare time, when the
  computer is waiting for user input. Indeed, this helps increasing the
  reactivity of <TeXmacs> at the first use of the functionality.

  For instance, assume that you defined a large new editing function
  <scm|foo-action> inside the module <scm|(foo-edit)>. Then your
  initialization file <verbatim|init-foo.scm> would typically contain a line

  <\scm-code>
    (lazy-define (foo-edit) foo-action)
  </scm-code>

  Similarly, lazy keyboard shortcuts and menus for <verbatim|foo> might be
  defined using

  <\scm-code>
    (lazy-keyboard (foo-kbd) in-foo-mode?)

    (lazy-menu (foo-menu) foo-menu)
  </scm-code>

  For more concrete examples, we recommend the user to take a look at the
  standard initialization file <hlink|<verbatim|init-texmacs.scm>|$TEXMACS_PATH/progs/init-texmacs.scm>.

  <label|redefinitions>On the negative side, the mechanism for lazy loading
  has the important consequence that you can no longer make assumptions on
  when a particular module is loaded. For instance, when you attempt to
  redefine a keyboard shortcut in your personal initialization file, it may
  happen that the standard definition is loaded after your \Predefinition\Q.
  In that case, your redefinition remains without consequence.

  For this reason, <TeXmacs> also provides the instruction <scm|import-from>
  to force a particular module to be loaded. Similarly, the commands
  <scm|lazy-keyboard-force>, <scm|lazy-plugin-force>, <abbr|etc.> may be used
  to force all lazy keyboard definitions <abbr|resp.> plug-ins to be loaded.
  In other words, the use of laziness forces to make implicit dependencies
  between modules more explicit.

  In the case when you want to redefine keyboard shortcuts, the
  <hlink|contextual overloading system|overview-overloading.en.tm> gives you
  an even more fine-grained control. For instance, assume that the keyboard
  shortcut <key|x x x> has been defined twice, both in general and in math
  mode. After calling <scm|lazy-keyboard-force> and overriding the general
  definition of the shortcut, the special definition will still take
  precedence in math mode. Alternatively, you may redefine the keyboard
  shortcut using

  <\scm-code>
    (kbd-map

    \ \ (:mode prevail?)

    \ \ ("x x x" <em|action>))
  </scm-code>

  This redefinition will prevail both in general and in math mode.

  <tmdoc-copyright|2005|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>