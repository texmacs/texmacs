<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|mupad|1.0>

    <\src-purpose>
      Markup for MuPAD sessions.
    </src-purpose>

    <src-copyright|2002--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <assign|mupad-output|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<with|par-left|<plus|<value|par-left>|1.5fn>|<with|par-mode|left|par-sep|0.45fn|math-display|true|<arg|body>>>>>>>

  <assign|mupad-input|<macro|prompt|body|<style-with|src-compact|none|<generic-input|<resize|<space|0.5fn><arg|prompt>|0fn||1.5fn||>|<arg|body>>>>>

  <assign|mupad-output|<macro|body|<style-with|src-compact|none|<surround|<vspace*|0.75fn>|<vspace|0.75fn>|<with|par-left|<plus|<value|par-left>|1.5fn>|<generic-output*|<with|par-sep|0.45fn|<arg|body>>>>>>>>

  \;

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