<TeXmacs|1.0.7.15>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|graphical-macros|1.0>

    <\src-purpose>
      Extra macros for graphics.
    </src-purpose>

    <src-copyright|2012|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(graphics graphics-markup)>

  <\active*>
    <\src-comment>
      Basic macros.
    </src-comment>
  </active*>

  <assign|rectangle|<macro|p1|p2|<extern|rectangle|<arg|p1>|<arg|p2>>>>

  <assign|circle|<macro|p1|p2|<extern|circle|<arg|p1>|<arg|p2>>>>

  <\active*>
    <\src-comment>
      Electrical.
    </src-comment>
  </active*>

  <assign|condensator|<macro|p1|p2|p3|<extern|condensator|<arg|p1>|<arg|p2>|<arg|p3>>>>

  <assign|diode|<macro|p1|p2|p3|<extern|diode|<arg|p1>|<arg|p2>|<arg|p3>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>