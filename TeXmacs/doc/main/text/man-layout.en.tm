<TeXmacs|1.0.0.5>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Layout issues>

  As a general rule, <apply|TeXmacs> takes care of the layout of your text.
  Therefore, although we did not want to forbid this possibility, we do not
  encourage you to typeset your document visually. For instance, you should
  not insert spaces or blank lines as substitutes for horizontal and vertical
  spaces between words and lines; instead, additional space should be
  inserted explicitly using <submenu|Insert|space>. This will make your text
  more robust in the sense that you will not have to reconsider the layout
  when performing some minor changes, which affect line or page breaking, or
  major changes, such as changing the document style.

  Several types of explicit spacing commands have been implemented. First of
  all, you can insert rigid spaces of given widths and heights. Horizontal
  spaces do not have a height and are either stretchable or not. The length
  of a stretchable spaces depends on the way a paragraph is hyphenated.
  Furthermore, it is possible to insert tabular spaces. Vertical spaces may
  be inserted either at the start or the end of a paragraph: the additional
  vertical space between two paragraphs is the maximum of the vertical space
  after the first one and the vertical space before the second one (contrary
  to <apply|TeX>, this prevents from superfluous space between two
  consecutive theorems).

  As to the paragraph layout, the user may specify the paragraph style
  (justified, left ragged, centered or right ragged), the paragraph margins
  and the left (resp. right) indentation of the first (resp. last) line of a
  paragraph. The user also controls the spaces between paragraphs and
  successive lines in paragraphs.

  You can specify the page layout in the <submenu|Document|page> menu. First
  of all, you can specify the way pages are displayed on the screen: when
  selecting ``paper'' as page type in <subsubmenu|Document|page|type>, you
  explicitly see the page breaks. By default, the page type is ``papyrus'',
  which avoids page breaking during the preparation of your document. The
  ``automatic'' page type assumes that your paper size is exactly the size of
  your window. The page margins and text width are specified in
  <subsubmenu|Document|page|layout>. Often, it is convenient to reduce the
  page margins for usage on the screen; this can be done in
  <subsubmenu|Document|page|screen layout>.

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
    <associate|idx-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
  </collection>
</references>
