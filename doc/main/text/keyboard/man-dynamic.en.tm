<TeXmacs|1.0.1.11>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Dynamic objects>

  Certain more complex objects can have several <em|states> during the
  editing process. Examples of such <em|dynamic objects> are labels and
  references, because the appearance of the reference depends on a
  dynamically determined number. Many other examples of dynamic markup can be
  found in the documentation about <apply|hyper-link|writing style
  files|../../../devel/style/keyboard/style-kbd.en.tm>.

  When entering a dynamic object like a label using <expand|kbd-gen|!>, the
  default state is <em|inactive>. This inactive state enables you to type the
  information which is relevant to the dynamic object, such as the name of
  the label in our case. Certain dynamic objects take an arbitrary number of
  parameters, and new ones can be inserted using <key|<key-variant>>.

  When you finished typing the relevant information for your dynamic object,
  you may type <key|<key-return>> in order to <em|activate> the
  object. An active dynamic object may be deactivated by placing your cursor
  just behind the object and hitting <key|<key-backspace>>.

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
