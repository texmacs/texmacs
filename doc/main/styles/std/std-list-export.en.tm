<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <tmdoc-title|Using list environments>

  The standard <TeXmacs> lists are defined in <tmdtd|std-list>. The
  unnumbered lists environments are:

  <\explain|<explain-macro|itemize|body>>
    The tag before each item depends on the nesting depth.
  </explain>

  <\explain|<explain-macro|itemize-minus|body>>
    Uses <with|mode|math|-> for the tag.
  </explain>

  <\explain|<explain-macro|itemize-dot|body>>
    Uses <with|mode|math|\<bullet\>> for the tag.
  </explain>

  <\explain|<explain-macro|itemize-arrow|body>>
    Uses <with|mode|math|\<rightarrow\>> for the tag.
  </explain>

  The following environments can be used for producing numbered lists:

  <\explain|<explain-macro|enumerate|body>>
    The kind of number before each item depends on the nesting depth.
  </explain>

  <\explain|<explain-macro|enumerate-numeric|body>>
    Number the items by 1, 2, 3, <abbr|etc.>
  </explain>

  <\explain|<explain-macro|enumerate-roman|body>>
    Number the items by i, ii, iii, <abbr|etc.>
  </explain>

  <\explain|<explain-macro|enumerate-Roman|body>>
    Number the items by I, II, III, <abbr|etc.>
  </explain>

  <\explain|<explain-macro|enumerate-alpha|body>>
    Number the items by a), b), c), <abbr|etc.>
  </explain>

  <\explain|<explain-macro|enumerate-Alpha|body>>
    Number the items by A), B), C), <abbr|etc.>
  </explain>

  The following environments can be used for descriptive lists:

  <\explain|<explain-macro|description|body>>
    The environment for default descriptive lists (usually
    <markup|description-compact>).
  </explain>

  <\explain|<explain-macro|description-compact|body>>
    Align the left hand sides of the items in the list and put their
    descriptions shortly behind it.
  </explain>

  <\explain|<explain-macro|description-dash|body>>
    Similar to <markup|description-compact>, but use a  to separate each
    item from its description.
  </explain>

  <\explain|<explain-macro|description-align|body>>
    Align the left hand sides of the descriptions, while aligning the items
    to the right.
  </explain>

  <\explain|<explain-macro|description-long|body>>
    Put the items and their descriptions on distinct lines.
  </explain>

  New items in a list are indicated through the <markup|item> tag or the
  <markup|item*> tag in the case of descriptions. The <markup|item> tag takes
  no arguments and the <markup|item*> tag one argument. When using the
  experimental <tmdtd|structured-list> package, these tags may take an
  optional body argument. In the future, all list items should become
  structured.

  By default, items in sublists are numbered in the same way as usual lists.
  Each list environment <markup|<em|list>> admits a variant
  <markup|<em|list>*> whose items are prefixed by the last item in the parent
  list. Of course, this feature can be used recursively.

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