<TeXmacs|1.0.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|letter|1.0>

      <\src-purpose>
        The letter style.
      </src-purpose>

      <\src-copyright|1998--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public
        license, version 3 or later|$TEXMACS_PATH/LICENSE>.
        It comes WITHOUT ANY WARRANTY WHATSOEVER.
        You should have received a copy of the license which the software.
        If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|std|env|title-generic|header-letter|section-generic>

  \;

  <assign|par-first|0fn>

  <assign|par-par-sep|0.6666fn>

  \;

  <assign|code|<\macro|body>
    <\padded-normal|0.5fn|1fn>
      <with|font-family|tt|language|verbatim|par-par-sep|0fn|<arg|body>>
    </padded-normal>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>