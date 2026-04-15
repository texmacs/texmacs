<TeXmacs|2.1.5>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|xwide-spacing|1.0|xwide-spacing|1.0>

    <\src-purpose>
      Extra wide spacing of logical connectors and infix relations.
    </src-purpose>

    <src-copyright|2026|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <style-with|src-compact|none|<if|<equal|<get-label|<value|spacing-policy>>|tuple>|<assign|spacing-policy|<merge|<range|<value|spacing-policy>|0|<minus|<length|<value|spacing-policy>>|1>>|<tuple|operator|<tuple|0.5spc|1spc|1spc>|wideop|<tuple|0.5spc|0.75spc|2spc>|wide>>>|<assign|spacing-policy|<tuple|operator|<tuple|0.5spc|1spc|1spc>|wideop|<tuple|0.5spc|1spc|2spc>|wide>>>>

  \;

  <assign|par-kerning-reduce|auto>

  <assign|par-kerning-contract|auto>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>