<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|axiom|1.0>

    <\src-purpose>
      Markup for Axiom sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|axiom-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<resize|<arg|prompt>|||1.75fn||>|<arg|body>>>>>

  <assign|axiom-output|<macro|body|<style-with|src-compact|none|<surround|<vspace*|1fn>|<vspace|1fn>|<with|par-left|<plus|<value|par-left>|1.75fn>|<generic-output*|<arg|body>>>>>>>

  \;

  <assign|leqno|<macro|<htab|5mm>>>

  <assign|axiomtype|<macro|type|<vspace*|0.5fn><hflush><with|color|brown|Type:
  <arg|type>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>