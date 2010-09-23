<TeXmacs|1.0.7.6>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|tmdoc-keyboard|1.0>

    <\src-purpose>
      Macros for keyboard shortcuts in the <TeXmacs> documentation.
    </src-purpose>

    <src-copyright|2001--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(doc tmdoc-markup)>

  \;

  <assign|render-key|<macro|x|<active*|<move|<small|<with|font-family|tt|<with|ornament-color|#e0e0e0|ornament-sunny-color|#f0f0f0|ornament-shadow-color|#c0c0c0|ornament-hpadding|2ln|ornament-vpadding|2ln|ornament-border|2ln|<ornament|<vcorrect|<arg|x>>>>>>||0.075ex>>>>

  <assign|key|<macro|x|<extern|tmdoc-key|<arg|x>>>>

  <assign|key*|<macro|x|<extern|tmdoc-key*|<arg|x>>>>

  <assign|shortcut|<macro|cmd|<extern|tmdoc-shortcut|<arg|cmd>>>>

  <assign|prefix|<macro|x|<extern|tmdoc-key|<arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
    <associate|sfactor|3>
  </collection>
</initial>