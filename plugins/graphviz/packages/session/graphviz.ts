<TeXmacs|1.99.12>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|graphviz|1.0>

    <\src-purpose>
      Markup for Graphviz sessions.
    </src-purpose>

    <src-copyright|2020|Darcy Shen>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|doc>

  <assign|graphviz-input|<\macro|prompt|body>
    <generic-input|<arg|prompt>|<with|prog-language|dot|<arg|body>>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>