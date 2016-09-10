<TeXmacs|1.99.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|icourse|1.0>

    <\src-purpose>
      Facilities for interactive courses
    </src-purpose>

    <src-copyright|2016|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(dynamic calc-markup)>

  <\active*>
    <\src-comment>
      Style parameters
    </src-comment>
  </active*>

  <assign|calc-input-color|#4040A0>

  <assign|calc-check-color|#40A040>

  <\active*>
    <\src-comment>
      Customizations
    </src-comment>
  </active*>

  <assign|calc-inert|<macro|ref|in|<with|color|<value|calc-input-color>|<arg|in>>>>

  <assign|calc-input|<macro|ref|in|out|<with|color|<value|calc-input-color>|<arg|in>>>>

  <assign|calc-output|<macro|ref|in|out|<arg|out>>>

  <assign|calc-ref|<macro|ref|<with|color|<value|calc-ref-color>|<arg|ref>>>>

  <\active*>
    <\src-comment>
      Automatically generated exercises and checking of user answers
    </src-comment>
  </active*>

  <assign|calc-generate|<macro|ref|cmd|out|<arg|out>>>

  <assign|calc-generate-command|<macro|ref|cmd|out|<with|color|<value|calc-input-color>|<arg|cmd>>>>

  <assign|calc-empty|<macro|x|\<ldots\><arg|x>\<ldots\>>>

  <assign|calc-correct|<macro|x|<arg|x>>>

  <assign|calc-incorrect|<macro|x|<with|color|red|<arg|x>>>>

  <assign|calc-answer|<macro|ref|cmd|answer|expected|<if|<or|<equal|<arg|answer>|<arg|expected>>|<equal|<math|<arg|answer>>|<arg|expected>>>|<calc-correct|<arg|answer>>|<if|<equal|<arg|answer>|>|<calc-empty|<arg|answer>>|<calc-incorrect|<arg|answer>>>>>>

  <assign|calc-answer-command|<macro|ref|cmd|answer|expected|<with|color|<value|calc-input-color>|<arg|cmd>>>>

  <assign|calc-suggest|<macro|ref|in|out|<with|color|<value|calc-input-color>|<arg|in>>>>

  <assign|calc-check|<macro|ref|pred|answer|status|sub|<with|mode|<value|mode>|<extern|calc-check-callback|<quote-arg|ref>>><if|<or|<equal|<arg|status>|true>|<equal|<arg|status>|<math|true>>>|<calc-correct|<arg|answer>>|<if|<equal|<arg|answer>|>|<calc-empty|<arg|answer>>|<calc-incorrect|<arg|answer>>>>>>

  <assign|calc-check-predicate|<macro|ref|pred|answer|status|sub|<with|color|<value|calc-check-color>|<arg|pred>>>>

  <assign|calc-check-command|<macro|ref|pred|answer|status|sub|<arg|sub>>>

  <drd-props|calc-check|syntax|<macro|ref|pred|answer|status|sub|<arg|answer>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>