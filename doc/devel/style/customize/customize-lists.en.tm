<TeXmacs|1.0.3.5>

<style|tmdoc>

<\body>
  <tmdoc-title|Customizing list environments>

  Lists are made up of two principal ingredients: the outer list environment
  and the inner items. List environments may either be customized by
  customizing or redefining the rendering macros for these environments, or
  defining additional list environments which match the same abstract
  interface.

  The rendering of the outer list environment is controlled by the
  <markup|render-list> macro which takes the body of the list as its
  argument. For instance, consider the following redefinition of
  <markup|render-list>:

  <\tm-fragment>
    <inactive*|<assign|render-list|<macro|body|<style-with|src-compact|none|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>>>
  </tm-fragment>

  This redefinition affects the rendering of all list environments (itemize,
  enumerate, <abbr|etc.>) by reducing the right margin with a length of
  <verbatim|3fn>:

  <\with|render-list|<macro|body|<surround|<no-page-break*><vspace*|0.5fn>|<right-flush><vspace|0.5fn><no-indent*>|<with|par-left|<plus|<value|par-left>|3fn>|par-right|<plus|<value|par-right>|3fn>|<arg|body>>>>>
    <\itemize>
      <item>This text, which has been made so long that it does not fit on a
      single line, is indented on the right hand side by <verbatim|3fn>.

      <\enumerate>
        <item>This text is indented by an additional <verbatim|3fn> on the
        right hand side, since it occurs inside a second list environment.
      </enumerate>

      <item>Once again: this text, which has been made so long that it does
      not fit on a single line, is indented on the right hand side by
      <verbatim|3fn>.
    </itemize>
  </with>

  In a similar way, you may customize the rendering of list items by
  redefining the macros <markup|aligned-item> and <markup|compact-item>.
  These macros both take one argument with the text of the item and render it
  either in a right-aligned way (such that subsequent text is left aligned)
  or in a left-aligned way (such that subsequent text may not be aligned).
  For instance, consider the following redefinition of <markup|aligned-item>:

  <\tm-fragment>
    <inactive*|<assign|aligned-item|<macro|x|<style-with|src-compact|none|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>>>
  </tm-fragment>

  Then items inside all list environments with compact items will appear in
  red:

  <\with|aligned-item|<macro|x|<vspace*|0.5fn><with|par-first|-3fn|<yes-indent>><resize|<with|color|red|<arg|x>>|r-2.5fn||r+0.5fn|>>>
    <\itemize>
      <item>This list and aligned descriptions have red items.

      <\description-aligned>
        <item*|C1>First condition.

        <item*|C2>Second condition.
      </description-aligned>

      <item>The items of compact description lists are rendered using
      <markup|compact-item>.

      <\description-compact>
        <item*|Gnus and gnats>Nice beasts.

        <item*|Micros and softies>Evil beings.
      </description-compact>
    </itemize>
  </with>

  <\remark>
    The macros <markup|aligned-item> and <markup|compact-item> are required
    to produce inline content, so that they may be used in order to surround
    blocks. In particular, several other internal macros
    (<markup|aligned-space-item>, <markup|long-compact-strong-dot-item>,
    <abbr|etc.>) are based on <markup|aligned-item> and
    <markup|compact-item>, and used for the rendering of the different types
    of lists (<markup|itemize-arrow>, <markup|description-long>,
    <abbr|etc.>). In the future, we also plan to extend <markup|item> and
    <markup|item*> with a compulsory <src-arg|body> argument. When
    customizing the list environments, it is important to keep that in mind,
    so as to make your style-sheets upward compatible.
  </remark>

  The <tmdtd|std-list> <abbr|d.t.d.> also provides a macro <markup|new-list>
  to define new lists. Its syntax is <explain-macro|new-list|name|item-render|item-transform>,
  where <src-arg|name> is the name of the new list environment,
  <src-arg|item-render> an (inline) macro for rendering the item and
  <src-arg|item-transform> an additional transformation which is applied on
  the item text. For instance, the <markup|enumerate-roman> environment is
  defined by

  <\tm-fragment>
    <\inactive*>
      <new-list|enumerate-roman|<value|aligned-dot-item>|<macro|x|<number|<arg|x>|roman>>>
    </inactive*>
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