<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|pine-theme|1.0|pine-theme|1.0>

    <\src-purpose>
      Pine theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|reddish-theme>

  <copy-theme|pine|reddish>

  <assign|pine|<macro|x|<with-pine|<with|ornament-color|<pattern|pine.png|*3/5|*3/5|#e0b050>|strong-color|#0c3000|math-color|#500000|ornament-sunny-color|#ffe8c0|ornament-shadow-color|brown|<ornament|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Background
    </src-comment>
  </active*>

  <assign|pine-bg-color|<pattern|pine.png|*3/5|*3/5|#e0b050>>

  <assign|pine-monochrome-bg-color|#e0b050>

  <\active*>
    <\src-comment>
      Standard ornaments
    </src-comment>
  </active*>

  <assign|pine-ornament-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>

  <assign|pine-ornament-extra-color|<pattern|wood-dark.png|*3/5|*3/5|#40200C>>

  <assign|pine-ornament-sunny-color|#c06024>

  <assign|pine-ornament-shadow-color|#40200C>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <assign|pine-title-bar-color|<pattern|wood-dark.png|*3/5|*3/5|#40200C>>

  <assign|pine-title-color|<macro|#f8f8f4>>

  <\active*>
    <\src-comment>
      Colors of standard tags (strong, math, etc.)
    </src-comment>
  </active*>

  <assign|pine-strong-color|#0c3000>

  <assign|pine-math-color|#050000>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <assign|pine-input-color|<pattern|paper-ridged-light.png|*3/5|*3/5|#f4eee8>>

  <assign|pine-fold-bar-color|<pattern|wood-dark.png|*3/5|*3/5|#40200C>>

  <assign|pine-fold-title-color|<pattern|wood-medium.png|*3/5|*3/5|#804018>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>