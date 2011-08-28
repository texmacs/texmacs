<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Counters and counter groups>

  In <TeXmacs>, all automatic numbering of theorems, sections, <abbr|etc.> is
  done using ``counters''. Such counters may be individual counters (like
  <src-var|equation-nr>) or belong to a group of similar counters (like in
  the case of <src-var|theorem-nr>). <TeXmacs> allows for the customization
  of counters on an individual or groupwise basis. Typically, you may
  redefine the rendering of a counter (and let it appear as roman numerals,
  for instance), or undertake special action when increasing the counter
  (such as resetting a subcounter).

  New individual counters are defined using the following meta-macro:

  <\explain|<explain-macro|new-counter|x>>
    Defines a new counter with name <src-arg|x>. The counter is stored in the
    numerical environment variable <no-break><src-var|x-nr> and in addition,
    the following macros are defined:

    <\explain|<explain-macro|the-<em|x>>>
      Retrieve the counter such as it should be displayed on the screen.
    </explain>

    <\explain|<explain-macro|reset-<em|x>>>
      Reset the counter to <with|mode|math|0>.
    </explain>

    <\explain|<explain-macro|inc-<em|x>>>
      Increase the counter. This macro may also be customized by the user so
      as to reset other counters (even though this is not the way things are
      done in the standard style files).
    </explain>

    <\explain|<explain-macro|next-<em|x>>>
      Increase the counter, display the counter and set the current label.
    </explain>

    For the purpose of customization, the <markup|new-counter> macro also
    defines the following macros:

    <\explain|<explain-macro|display-<em|x>|nr>>
      This is the macro which is used for transforming the numerical value of
      the counter into the value which is displayed on the screen.
    </explain>

    <\explain|<explain-macro|counter-<em|x>|x>>
      This internal macro is used in order to retrieve the name of the
      environment variable which contains the counter. By default, this macro
      returns ``nr-x'', but it may be redefined if the counter belongs to a
      group.
    </explain>
  </explain>

  As noticed in the introduction, <TeXmacs> uses <em|counter groups> in order
  to make it possible to treat similar counters in a uniform way. For
  instance the counter group <verbatim|theorem-env> regroups the counters
  <verbatim|theorem>, <verbatim|proposition>, <verbatim|lemma>, <abbr|etc.>.
  New counter groups and are defined using:

  <\explain|<explain-macro|new-counter-group|g>>
    Create a new counter group with name <src-arg|g>. This results in the
    creation of the following macros:

    <\explain>
      <explain-macro|display-in-<em|g>|x|nr>

      <explain-macro|counter-in-<em|g>|x>
    <|explain>
      These macros are similar to the macros <markup|display-<em|x>> and
      <markup|counter-<em|x>> from above, but relative to the counter group.
      The name <src-arg|x> of the counter in consideration is passed as an
      argument.
    </explain>
  </explain>

  New counters can be added to the group using:

  <\explain|<explain-macro|add-to-counter-group|x|g>>
    Defines a new counter <src-arg|x> and add it to the counter group
    <src-arg|g>. For counters in groups, the macros <markup|display-<em|x>>
    and <markup|counter-<em|x>> are replaced with the corresponding macros
    <markup|display-in-<em|g>> and <markup|counter-<no-break>in-<no-break><em|g>>
    for their groups. Nevertheless, two new macros
    <markup|ind-display-<em|x>> and <markup|ind-counter-<em|x>> are defined
    which may take over the roles of <markup|display-<em|x>> and
    <markup|counter-<em|x>> in the case when the group consists of individual
    counters.
  </explain>

  At any moment, you may decide whether the counters of a group share a
  common group counter, or whether they all use their individual counters.
  This feature is used for instance in order to switch between American style
  numbering and European style numbering:

  <\explain|<explain-macro|group-common-counter|g>>
    Use a common counter for the group (which is stored in the environment
    variable <src-var|g-nr>).
  </explain>

  <\explain|<explain-macro|group-individual-counters|g>>
    Use an individual counter for each member of the group (this is the
    default).
  </explain>

  We notice that group counters may recursively belong to super-groups. For
  instance, the following declarations are from <verbatim|env-base.ts>:

  <\tm-fragment>
    <\inactive*>
      <new-counter-group|std-env>
    </inactive*>

    <inactive*|<new-counter-group|theorem-env>>

    <inactive*|<add-to-counter-group|theorem-env|std-env>>

    <inactive*|<group-common-counter|theorem-env>>
  </tm-fragment>

  <tmdoc-copyright|1998--2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|par-width|150mm>
  </collection>
</initial>