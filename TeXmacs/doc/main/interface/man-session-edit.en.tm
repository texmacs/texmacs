<TeXmacs|1.0.1.21>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Editing sessions>

  Inside input fields of sessions, the cursor keys have a special meaning:
  when moving upwards or downwards, you will move to previous or subsequent
  input fields. When moving to the left or to the right, you will never leave
  the input field; you should rather use the mouse for this.

  Some facilities for editing input, output and text fields are available in
  the <apply|menu|Session|Insert fields> and <apply|menu|Session|Remove
  fields> menus. Most operations directly apply to matching input/output
  fields. Optionally, an additional explanatory text field can be associated
  to an input field using <apply|menu|Session|Insert fields|Insert text
  field>. Keyboard shortcuts for inserting fields are <key|A-<expand|key-up>>
  (insert above) and <key|A-<expand|key-down>>. Keyboard shortcuts for
  removing matching text/input/output fields are
  <key|A-<expand|key-backspace>> (remove backwards) and
  <key|A-<expand|key-delete>> (remove current fields).

  It is possible to create ``subsessions'' using <apply|menu|Session|Insert
  fields|Fold input field> or <key|A-<expand|key-right>>. In that case, the
  current text/input/output field becomes the body of an unfolded subsession.
  Such a subsession consists of an explanatory text together with a sequence
  of text/input/output fields. Subsessions can be folded and unfolded using
  <key|M-A-<expand|key-up>> <abbr|resp.> <key|M-A-<expand|key-down>>.
  Subsessions have a nice rendering on the screen when using the
  <tmpackage|varsession> package in <apply|menu|Document|Use
  package|Program>.

  Other useful editing operations for text/input/output fields are
  <apply|menu|Session|Remove fields|Remove all output fields>, which is
  useful for creating a demo sessions which will be executed later on, and
  <apply|menu|Session|Split session>, which can be used for splitting a
  session into parts for inclusion into a paper.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Insert fields>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Remove fields>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Insert fields>|<with|font family|<quote|ss>|Insert
      text field>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Text>|<with|font
      family|<quote|ss>|Session>|<with|font
      family|<quote|ss>|Other>>|<pageref|idx-5>>
    </associate>
  </collection>
</auxiliary>
