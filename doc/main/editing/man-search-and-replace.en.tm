<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Search and replace>

  You can start searching text by pressing <key|C-s> or
  <apply|menu|Edit|Search>. During a search, the ``search string'' is
  displayed at the left hand side of the footer. Each character you type is
  appended to this search string and the next occurrence of it is surrounded
  by a red box. When pressing <key|C-s> a second time during a search, the
  next occurrence is being searched. A beep indicates that no more
  occurrences were found in the document; pressing <key|C-s> will continue
  the search at the beginning of your document. You may press
  <key|<key-backspace>> in order to undo key presses during a
  search.

  Usually, text is being searched for in a forward manner, starting from the
  current cursor position. You may also search backwards, using <key|C-r>.
  During a search, only text in the same mode and the same language will be
  found, as those which are active at the position where you started your
  search. In other words, when searching an <with|mode|math|x> in math-mode,
  you will not find any x's in the ordinary text. As a current limitation,
  the search string can only contain ordinary text and no math-symbols or
  more complicated structured text.

  A query replace is started by pressing <key|C-=> or
  <apply|menu|Edit|Replace>. You are prompted for a string which is to be
  replaced and the string by which to replace. At each occurrence of the
  string to be replaced you are prompted and you have to choose between
  replacing the string (y), not replacing it (n) and replace this and all
  further occurrences (a). Like in the case of searching, the query-replace
  command is mode and language sensitive.

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
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Search>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Edit>|<with|font
      family|<quote|ss>|Replace>>|<pageref|idx-2>>
    </associate>
  </collection>
</auxiliary>
