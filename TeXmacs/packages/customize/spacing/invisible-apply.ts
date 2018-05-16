<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|invisible-apply|1.0|invisible-apply|1.0>

    <\src-purpose>
      Suppress spacing around compact-style function application as in
      <math|f x><compound|math|>.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <if|<equal|<get-label|<value|spacing-policy>>|tuple>||<assign|spacing-policy|<tuple|<value|spacing-policy>>>>

  <assign|spacing-policy|<merge|<tuple|short-apply|0spc>|<value|spacing-policy>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>