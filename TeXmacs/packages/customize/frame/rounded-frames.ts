<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|rounded-frames|1.0>

    <\src-purpose>
      A style package for rounded frames
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|ornament-shape|rounded>

  <if|<unequal|<value|ornament-color>|>|<assign|ornament-border|0ln>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>