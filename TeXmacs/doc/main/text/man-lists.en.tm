<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Lists>

  Using <apply|menu|Insert|Itemize> you may start an unnumbered list. You may
  either select a particular tag like <with|mode|math|\<bullet\>> (bullets),
  <with|mode|math|<op|->> (dashes) or <with|mode|math|<op|\<rightarrow\>>>
  (arrows) to indicate entries in the list or the default tag. Lists may be
  <em|nested> inside other tags, like in the following list:

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
  nesting. At the outermost level, we used the <with|mode|math|\<bullet\>>
  tag, at the second level <with|mode|math|<op|\<circ\>>>, and so on. When
  you are inside a list, notice that pressing <shortcut|(kbd-return)>
  automatically starts a new item. If you need items which are several
  paragraphs long, then you may always use <shortcut|(kbd-shift-return)> in
  order to start a new paragraph.

  Enumerate environments, which are started using
  <apply|menu|Insert|Enumerate>, behave in a similar way as itemize, except
  that the items are numbered. Here follows an example of an enumeration
  which was started using <apply|menu|Insert|Enumerate|Roman>:

  <\expand|enumerate-Roman>
    <item>A first item.

    <item>A second one.

    <item>And a last one.
  </expand>

  The last type of lists are descriptive lists. They are started using
  <apply|menu|Insert|Description> and allow you to describe a list of concepts:

  <\description>
    <expand|item*|Gnu.>A hairy but gentle beast.

    <expand|item*|Gnat.>Only lives in a zoo.
  </description>

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
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|III.|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Itemize>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Enumerate>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Enumerate>|<with|font
      family|<quote|ss>|Roman>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Description>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
