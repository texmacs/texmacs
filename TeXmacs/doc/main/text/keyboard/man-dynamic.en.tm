<TeXmacs|1.0.2.9>

<style|tmdoc>

<\body>
  <tmdoc-title|Dynamic objects>

  Certain more complex objects can have several <em|states> during the
  editing process. Examples of such <em|dynamic objects> are labels and
  references, because the appearance of the reference depends on a
  dynamically determined number. Many other examples of dynamic markup can be
  found in the documentation about <hyper-link|writing style
  files|../../../devel/style/keyboard/style-kbd.en.tm>.

  When entering a dynamic object like a label using <shortcut|(make-label)>, the default
  state is <em|inactive>. This inactive state enables you to type the
  information which is relevant to the dynamic object, such as the name of
  the label in our case. Certain dynamic objects take an arbitrary number of
  parameters, and new ones can be inserted using <key|var>.

  <\big-figure>
    <with|color|blue|<with|mode|math|\<langle\>>label<with|mode|math|\|>>pythagoras<with|color|blue|<with|mode|math|\<rangle\>>>
  </big-figure|Inactive label>

  When you finished typing the relevant information for your dynamic object,
  you may type <shortcut|(kbd-return)> in order to <em|activate> the object. An
  active dynamic object may be deactivated by placing your cursor just behind
  the object and hitting <key|backspace>.

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
    <associate|idx-1|<tuple|1|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|gly-1|<tuple|1|?>>
    <associate|gly-2|<tuple|2|?>>
    <associate|idx-2|<tuple|1|?>>
    <associate|gly-3|<tuple|3|?>>
  </collection>
</references>