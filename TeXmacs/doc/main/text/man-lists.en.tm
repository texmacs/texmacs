<TeXmacs|1.99.13>

<style|<tuple|tmdoc|english|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|Lists>

  Using <menu|Insert|Itemize> you may start an unnumbered list. You may
  either select a particular tag like <math|\<bullet\>> (bullets),
  <math|<op|->> (dashes) or <math|<op|\<rightarrow\>>> (arrows) to indicate
  entries in the list or the default tag. Lists may be <em|nested> inside
  other tags, like in the following list:

  <\itemize>
    <item>First item.

    <item>Now comes the sublist:

    <\itemize>
      <item>A subitem.

      <item>Another one.
    </itemize>

    <item>A final item.
  </itemize>

  The default tag is rendered in a different way depending on the level of
  nesting. At the outermost level, we used the <math|\<bullet\>> tag, at the
  second level <math|<op|\<circ\>>>, and so on. When you are inside a list,
  notice that pressing <shortcut|(kbd-return)> automatically starts a new
  item. If you need items which are several paragraphs long, then you may
  always use <shortcut|(kbd-shift-return)> in order to start a new paragraph.

  Enumerate environments, which are started using <menu|Insert|Enumerate>,
  behave in a similar way as itemize, except that the items are numbered.
  Here follows an example of an enumeration which was started using
  <menu|Insert|Enumerate|Roman>:

  <\enumerate-Roman>
    <item>A first item.

    <item>A second one.

    <item>And a last one.
  </enumerate-Roman>

  The last type of lists are descriptive lists. They are started using
  <menu|Insert|Description> and allow you to describe a list of concepts:

  <\description>
    <item*|Gnu>A hairy but gentle beast.

    <item*|Gnat>Only lives in a zoo.
  </description>

  <tmdoc-copyright|1998\U2020|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>