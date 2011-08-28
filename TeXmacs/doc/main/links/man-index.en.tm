<TeXmacs|1.0.1.12>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Generating an index>

  For the generation of an index, you first have to put index entries in your
  document using <apply|menu|Insert|Link|Index entry>. At a second stage, you
  must put your cursor at the place where you want your index to be generated
  and click on <apply|menu|Insert|Automatic|Index>. The index is than generated
  in a similar way as the table of contents.

  In the <apply|menu|Insert|Link|Index entry> menu, you find several types of
  index entries. The simplest are ``main'', ``sub'', ``subsub'', which are
  macros with one, two and three arguments respectively. Entries of the form
  ``sub'' and ``subsub'' may be used to subordinate index entries with
  respect to other ones.

  A complex index entry takes four arguments. The first one is a key how the
  entry has to be sorted and it must be a ``tuple'' (created using
  <key|inactive \<less\>>) whose first component is the main category, the
  second a subcategory, etc. The second argument of a complex index entry is
  either blank or ``strong'', in which case the page number of your entry
  will appear in a bold typeface. The third argument is usually blank, but if
  you create two index entries with the same non-blank third argument, then
  this will create a ``range'' of page numbers. The fourth argument, which is
  again a tuple, is the entry itself.

  It is also possible to create an index line without a page number using
  ``interject'' in <apply|menu|Insert|Link|Index entry>. The first argument
  of this macro is a key for how to sort the index line. The second argument
  contains the actual text. This construct may be useful for creating
  different sections ``A'', ``B'', etc. in your index.

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
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Index
      entry>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Automatic>|<with|font
      family|<quote|ss>|Index>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Index
      entry>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insert>|<with|font
      family|<quote|ss>|Link>|<with|font family|<quote|ss>|Index
      entry>>|<pageref|idx-4>>
    </associate>
  </collection>
</auxiliary>
