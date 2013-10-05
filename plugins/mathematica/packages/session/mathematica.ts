<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mathematica|1.0>

    <\src-purpose>
      Markup for Mathematica sessions.
    </src-purpose>

    <src-copyright|2005|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|mathematica-output|<\macro|body>
    <\padded>
      <generic-output*|<arg|body>>
    </padded>
  </macro>>

  <assign|Mvariable|<macro|name|<arg|name>>>

  <assign|Mfunction|<macro|name|<arg|name>>>

  <assign|Muserfunction|<macro|name|<arg|name>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>