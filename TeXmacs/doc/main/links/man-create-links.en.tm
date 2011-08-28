<TeXmacs|1.0.5.6>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating labels, links and references>

  You may create a new inactive label using <shortcut|(make-label)> or
  <menu|Insert|Link|Label> and a reference to this label using <shortcut|(make 'reference)> or
  <menu|Insert|Link|Reference>. After typing the name of the label or
  reference, remember to hit <shortcut|(kbd-return)> in order to
  <hlink|activate|../text/keyboard/man-hybrid.en.tm> it. You may also type
  the first characters of the name of a reference and use the <key|tab>
  key in order to automatically complete it.

  You should be careful to put the label at a point where its number will be
  correct. When labeling sections, the recommended place is just after the
  sectional tag. When labeling single equations (created using
  <menu|Insert|Mathematics|Equation>), the recommended place is at the start
  inside the equation. When labeling multiple equations (created using
  <menu|Insert|Mathematics|Equations>), you must put the labels just behind
  the equation numbers. Recall that you may use <shortcut|(numbered-toggle (focus-tree))> in order to
  transform an unnumbered environment or equation into a numbered one, and
  vice versa.

  It is possible to create hyperlinks to other documents using
  <key|inactive \<gtr\>> or <menu|Insert|Link|Hyperlink>. The first field of the
  hyperlink is the associated text, which is displayed in blue when
  activated. The second field contains the name of a document, which may be
  on the web. As is usual for hyperlinks, a link of the form
  <verbatim|#<with|font-shape|italic|label>> points to a label in the same
  document and a link of the form <verbatim|<with|font-shape|italic|url>#<with|font-shape|italic|label>>
  points to a label in the document located at
  <verbatim|<with|font-shape|italic|url>>.

  In a similar fashion, an action may be associated to a piece of text or
  graphics using <key|inactive *> or <menu|Insert|Link|Action>. The second field
  now contains a Guile/Scheme script command, which is executed whenever you
  double click on the text, after its activation. For security reasons, such
  scripts are not always accepted. By default, you are prompted for
  acceptation; this default behaviour may be changed in
  <menu|Options|Security>. Notice that the Guile/Scheme command\ 

  <\verbatim>
    \ \ \ \ (system "shell-command")
  </verbatim>

  evaluates <verbatim|shell-command> as a shell command.

  Finally, you may directly include other documents inside a given document
  using <key|inactive i> or <menu|Insert|Link|Include>. This allows you for
  instance to include the listing of a program in your text in such a way
  that your modifications in your program are automatically reflected in your
  text.

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
  </collection>
</initial>