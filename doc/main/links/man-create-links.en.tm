<TeXmacs|1.0.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Creating labels, links and references>

  You may create a new inactive label using <kbd-gen|!> or
  <menu|Insert|Link|Label> and a reference to this label using <kbd-gen|?> or
  <menu|Insert|Link|Reference>. Be careful to put the label at a point where
  its number will be correct. When labeling sections, the recommended place
  is just after the section name. When labeling equations, the recommended
  place is at the start inside the equation.

  It is possible to create hyperlinks to other documents using
  <kbd-ia|\<gtr\>> or <menu|Insert|Link|Hyperlink>. The first field of the
  hyperlink is the associated text, which is displayed in blue when
  activated. The second field contains the name of a document, which may be
  on the web. As is usual for hyperlinks, a link of the form
  <verbatim|#<with|font-shape|italic|label>> points to a label in the same
  document and a link of the form <verbatim|<with|font-shape|italic|url>#<with|font-shape|italic|label>>
  points to a label in the document located at
  <verbatim|<with|font-shape|italic|url>>.

  In a similar fashion, an action may be associated to a piece of text or
  graphics using <kbd-ia|*> or <menu|Insert|Link|Action>. The second field
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
  using <kbd-ia|i> or <menu|Insert|Link|Include>. This allows you for
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
    <associate|page-even|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-left|25mm>
    <associate|sfactor|4>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|page-right|30mm>
    <associate|par-width|150mm>
    <associate|page-odd|30mm>
    <associate|page-bot|30mm>
    <associate|language|english>
    <associate|page-reduce-top|15mm>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Label>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Reference>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Hyperlink>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Action>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Options>|<with|font-family|<quote|ss>|Security>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Insert>|<with|font-family|<quote|ss>|Link>|<with|font-family|<quote|ss>|Include>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>