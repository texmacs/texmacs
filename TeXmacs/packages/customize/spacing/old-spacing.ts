<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|old-spacing|1.0|old-spacing|1.0>

    <\src-purpose>
      Mimick spacing of TeXmacs version 1.99.6 and below.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <style-with|src-compact|none|<if|<equal|<get-label|<value|spacing-policy>>|tuple>|<assign|spacing-policy|<merge|<range|<value|spacing-policy>|0|<minus|<length|<value|spacing-policy>>|1>>|<tuple|old>>>|<assign|spacing-policy|old>>>

  \;

  <assign|par-kerning-reduce|0>

  <assign|par-kerning-contract|0>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>