<TeXmacs|1.99.9>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|no-page-numbers|1.0|no-page-numbers|1.0>

    <\src-purpose>
      Remove headers and footers
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|no-page-numbers|true>

  <provide|normal-page-head-sep|<value|page-head-sep>>

  <provide|normal-page-foot-sep|<value|page-foot-sep>>

  <provide|normal-page-odd-header|<quote-value|page-odd-header>>

  <provide|normal-page-even-header|<quote-value|page-even-header>>

  <provide|normal-page-odd-footer|<quote-value|page-odd-footer>>

  <provide|normal-page-even-footer|<quote-value|page-even-footer>>

  \;

  <assign|page-head-sep|2mm>

  <assign|page-foot-sep|2mm>

  <set-header|>

  <set-footer|>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>