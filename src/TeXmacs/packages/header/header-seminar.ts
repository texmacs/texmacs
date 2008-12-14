<TeXmacs|1.0.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|header-seminar|1.0|header|1.0>

    <\src-purpose>
      Headers for the seminar style.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public
      license, version 3 or later|$TEXMACS_PATH/LICENSE>.
      It comes WITHOUT ANY WARRANTY WHATSOEVER.
      You should have received a copy of the license which the software.
      If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|page-odd-footer|<style-with|src-compact|none|<htab|5mm><with|magnification|1|<quote|<page-the-page>>><htab|5mm>>>

  <assign|page-even-footer|<style-with|src-compact|none|<htab|5mm><with|magnification|1|<quote|<page-the-page>>><htab|5mm>>>

  \;

  <assign|start-page|<macro|s|>>

  <assign|odd-page-text|<macro|s|>>

  <assign|even-page-text|<macro|s|>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>