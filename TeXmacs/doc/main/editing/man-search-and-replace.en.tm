<TeXmacs|1.99.1>

<style|<tuple|tmdoc|english>>

<\body>
  <tmdoc-title|Search and replace>

  You can start searching text by pressing <key|C-s> or <menu|Edit|Search>.
  Doing this, a new special ``search toolbar'' will appear below the main
  text, just above the footer. When typing text in the search field of the
  toolbar, all occurrences of this text will be highlighted in the main
  document. Moreover, one ``principal'' occurrence will be highlighted in red
  and you may navigate through all occurrences using <key|pageup> and
  <key|pagedown> (or <key|up> and <key|down>, or <key|return>). Using
  <key|home> and <key|end>, you may jump to the first and last occurrences
  respectively. As soon as you press the escape key <key|escape>, the search
  toolbar will be closed, searching stops and focus returns to the main
  document.

  During a search, only text in the same mode and the same language will be
  found, as those which are active at the position where you started your
  search. In other words, when searching an <math|x> in math-mode, you will
  not find any x's in the ordinary text. As a current limitation, the search
  string can only contain ordinary text and no math-symbols or more
  complicated structured text.

  In order to replace text, you should use <menu|Edit|Replace> or press
  <key|C-=>. This will cause a special ``replace toolbar'' to appear below
  the main text, just above the footer. You are prompted for a string which
  is to be replaced and the string by which to replace. Again, you may use
  the <key|pageup> and <key|pagedown> keys in order to navigate through the
  occurrences of the search string. When pressing <key|tab> or <key|return>
  in the search field, focus will be moved to the replace field. You may
  still use the <key|pageup> and <key|pagedown> keys in order to navigate
  through the occurrences of the search string. In addition, pressing
  <key|return> will replace the principal occurrence of the search string by
  the replace string. Using <key|S-return>, you may undo the last
  replacement. You may replace all remaining occurrences by pressing
  <key|C-return>. Like in the case of searching, the query-replace command is
  mode and language sensitive.

  The search and replace toolbars are quite rudimentary in the sense that
  they only allow for searching and replacing plain text. By pressing the
  <icon|tm_expand_tool.xpm> icon on either of these toolbars, you may expand
  the toolbar into a full blown widget with larger search and replace fields
  which may contain arbitrary markup. Searching and replacing can be done
  using more or less the same keyboard shortcuts as in the case of the
  toolbars, but you may now search and replace arbitrary content.

  When searching non textual content, the conditions for having a hit are
  somewhat released. For instance, assume that you are just starting a new
  search with an empty search field. Then typing<nbsp><key|F6> inserts the
  <markup|strong> tag with no text inside yet. Instead of looking only for
  ``strong empty strings'', <TeXmacs> will rather look for all strong markup
  in your document. If you next enter the letter ``a'', then <TeXmacs> will
  look for all strong text which contains the letter ``a''. In a<nbsp>similar
  way, when searching for the formula <math|<frac|x|>>, <TeXmacs> will
  highlight all fractions in which the numerator contains the variable
  <math|x>. Yet another example: a search for <math|<frac||<sqrt|x>>> will
  highlight all formulas in which the denominator contains a square root
  which contains the variable <math|x>. For instance, the fraction
  <math|<frac|a+b|c+<sqrt|x+y>>> will be highlighted, but not
  <math|<frac|<sqrt|x+y>|a+<sqrt|y>>>.

  When using the structured text and replace widgets, <TeXmacs> also
  implements a few additional special tags for enhancing structured
  searching. First of all, it can happen that you would like to search for
  certain content <em|inside> a special context. For instance, you might want
  to search for all occurrences of the letter ``a'' inside a <markup|strong>
  tag. When searching for <strong|a>, as above, <TeXmacs> will highlight all
  <markup|strong> tags which contain the letter ``a''. In order to highlight
  the letters ``a'' themselves, you should first enter the <markup|strong>
  tag inside an empty search field using <key|F6>. You next enter a special
  <markup|select-region> tag using <key|std ?>, and finally insert the letter
  ``a'' inside this tag. Other special markup which can be used inside search
  fields are the ``wildcards'' <math|x>, <math|y> and <math|z> which are
  entered using <key|std 1>, <key|std 2> and <key|std 3>.

  As soon as you start using the structured text and replace widgets instead
  of the toolbars, this will be remembered as a user preference: any
  subsequent searches or replacements will again use the widgets. In order to
  switch back to the less intrusive toolbar methods for searching and
  replacing, you should press the <icon|tm_compress_tool.xpm> icon.

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>