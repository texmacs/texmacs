<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Defining new environments>

  The <tmdtd|env-base> <abbr|d.t.d.> contains high-level markup which can be
  used by the user to define new numbered environments for theorems, remarks,
  exercises and figures:

  <\explain|<explain-macro|new-theorem|env-name|display-name>>
    This meta-macro is used for defining new theorem-like environments. The
    first argument <src-arg|env-name> specifies the name for the environment
    (like ``experiment'') and <src-arg|display-name> the corresponding text
    (like ``Experiment''). When defining a new theorem-like environment like
    <markup|experiment>, an unnumbered variant <markup|experiment*> is
    automatically defined as well.
  </explain>

  <\explain|<explain-macro|new-remark|env-name|display-name>>
    Similar as <markup|new-theorem>, but for remarks.
  </explain>

  <\explain|<explain-macro|new-exercise|env-name|display-name>>
    Similar as <markup|new-theorem>, but for exercises.
  </explain>

  <\explain|<explain-macro|new-exercise|env-name|display-name>>
    Similar as <markup|new-theorem>, but for figures. When defining a new
    type of figure, like ``picture'', the <markup|new-figure> macro defines
    both the inline environment <markup|small-picture> and the
    block-environment <markup|big-picture>, as well as the unnumbered
    variants <markup|small-picture*> and <markup|big-picture*>.
  </explain>

  The theorem-like and remark-like environments belong to a common
  counter-group <verbatim|theorem-env>. By default, we use American-style
  numbering (one common counter for all environments). When selecting the
  package <tmpackage|number-europe>, each environment uses its own counter.
  All exercises and figures use their own counter-group.

  More generally, the <verbatim|std-env> counter-group regroups the counters
  for all standard <TeXmacs> environments. Typically, all counters in this
  group are prefixed in a similar way (for instance by the number of the
  chapter). Figure <reference|std-env-fig> shows how the hierarchical
  organization of this counter group.

  <\big-figure|<tree|<verbatim|std-env>|<tree|<verbatim|theorem-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|theorem>>>|<row|<cell|<verbatim|proposition>>>|<row|<cell|<verbatim|remark>>>|<row|<cell|<with|mode|math|\<vdots\>>>>>>>>|<tree|<verbatim|exercise-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|exercise>>>|<row|<cell|<verbatim|problem>>>>>>>|<tree|<verbatim|figure-env>|<tabular*|<tformat|<table|<row|<cell|<verbatim|figure>>>|<row|<cell|<verbatim|table>>>>>>>|<verbatim|equation>|<verbatim|footnote>>>
    <label|std-env-fig>Organization of the counters for the standard
    <TeXmacs> environments.
  </big-figure>

  In addition to the standard theorem-like, remark-like, exercise-like and
  figure-like environments, other numbered textual environments may be
  defined using the <markup|new-env> macro. These environments may be based
  on arbitrary counter-groups:

  <\explain|<explain-macro|new-env|group|env|env-name|display-name>>
    The first argument is the name of the counter <src-arg|group> to which
    the new environment belongs. The second argument <src-arg|env> is the
    name of a binary macro for rendering the environment. The arguments of
    the rendering macro are a name (like ``Theorem 3.14'') and its body. The
    remaining arguments are similar as for <markup|new-theorem>. For
    instance, in the standard style-sheets, <markup|new-theorem> is defined
    by

    <\tm-fragment>
      <\inactive*>
        <assign|new-theorem|<macro|env|name|<new-env|<arg|env>|<arg|name>|theorem-env|render-theorem>>>
      </inactive*>
    </tm-fragment>
  </explain>

  We recall that you may add new counters or counter-groups to the
  <verbatim|theorem-env> counter-group using the <markup|new-counter-group>
  and <markup|add-to-counter-group> macros, as described in the section about
  counters.

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