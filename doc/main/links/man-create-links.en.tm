<TeXmacs|1.0.0.8>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Creating labels, links and references>

  You may create a new inactive label using <expand|kbd-gen|!> or
  <subsubmenu|Insert|link|label> and a reference to this label using
  <expand|kbd-gen|?> or <subsubmenu|Insert|link|reference>. Be careful to put
  the label at a point where its number will be correct. When labeling
  sections, the recommended place is just after the section name. When
  labeling equations, the recommended place is at the start inside the
  equation.

  It is possible to create hyperlinks to other documents using
  <expand|kbd-ia|\<gtr\>> or <subsubmenu|Insert|link|hyperlink>. The first
  field of the hyperlink is the associated text, which is displayed in blue
  when activated. The second field contains the name of a document, which may
  be on the web. As is usual for hyperlinks, a link of the form
  <verbatim|#<with|font shape|italic|label>> points to a label in the same
  document and a link of the form <verbatim|<with|font
  shape|italic|url>#<with|font shape|italic|label>> points to a label in the
  document located at <verbatim|<with|font shape|italic|url>>.

  In a similar fashion, an action may be associated to a piece of text or
  graphics using <expand|kbd-ia|*> or <subsubmenu|Insert|link|action>. The
  second field now contains a Guile/Scheme script command, which is executed
  whenever you double click on the text, after its activation. For security
  reasons, such scripts are not always accepted. By default, you are prompted
  for acceptation; this default behaviour may be changed in
  <submenu|Options|security>. Notice that the Guile/Scheme command\ 

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  evaluates <verbatim|shell-command> as a shell command.

  Finally, you may directly include other documents inside a given document
  using <expand|kbd-ia|i> or <subsubmenu|Insert|link|include>. This allows
  you for instance to include the listing of a program in your text in such a
  way that your modifications in your program are automatically refelcted in
  your text.

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
