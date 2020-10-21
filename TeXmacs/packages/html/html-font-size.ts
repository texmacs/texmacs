<TeXmacs|1.99.13>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|html-font-size|1.0>

    <\src-purpose>
      Some styles use fixed font sizes, which is not suitable for Html
      exports. With this package, relative settings are used for the Html
      export.
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  \;

  <active*|<\src-comment>
    Don't use absolute sizes for Html export
  </src-comment>>

  <assign|tmhtml-really-tiny|<macro|body|<with|font-size|0.4|<arg|body>>>>

  <assign|tmhtml-very-tiny|<macro|body|<with|font-size|0.45|<arg|body>>>>

  <assign|tmhtml-tiny|<macro|body|<with|font-size|0.5|<arg|body>>>>

  <assign|tmhtml-really-small|<macro|body|<with|font-size|0.6|<arg|body>>>>

  <assign|tmhtml-very-small|<macro|body|<with|font-size|0.7|<arg|body>>>>

  <assign|tmhtml-smaller|<macro|body|<with|font-size|0.8|<arg|body>>>>

  <assign|tmhtml-small|<macro|body|<with|font-size|0.9|<arg|body>>>>

  <assign|tmhtml-flat-size|<macro|body|<with|font-size|0.9|<arg|body>>>>

  <assign|tmhtml-normal-size|<macro|body|<with|font-size|1|<arg|body>>>>

  <assign|tmhtml-sharp-size|<macro|body|<with|font-size|1.1|<arg|body>>>>

  <assign|tmhtml-large|<macro|body|<with|font-size|1.2|<arg|body>>>>

  <assign|tmhtml-larger|<macro|body|<with|font-size|1.4|<arg|body>>>>

  <assign|tmhtml-very-large|<macro|body|<with|font-size|1.7|<arg|body>>>>

  <assign|tmhtml-really-large|<macro|body|<with|font-size|1.8|<arg|body>>>>

  <assign|tmhtml-huge|<macro|body|<with|font-size|2.0|<arg|body>>>>

  <assign|tmhtml-very-huge|<macro|body|<with|font-size|2.2|<arg|body>>>>

  <assign|tmhtml-really-huge|<macro|body|<with|font-size|2.5|<arg|body>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>