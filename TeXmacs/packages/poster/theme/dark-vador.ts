<TeXmacs|1.99.6>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-vador|1.0|poster-theme|1.0>

    <\src-purpose>
      Dark vador theme for posters.
    </src-purpose>

    <src-copyright|2018|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Global parameters.
    </src-comment>
  </active*>

  <assign|bg-color|black>

  <assign|color|white>

  <\active*>
    <\src-comment>
      Title blocks.
    </src-comment>
  </active*>

  <assign|xtitle-render-title|<macro|name|<with|font-series|bold|emboss-color|#c0c060|emboss-dx|-0.5ln|emboss-dy|-0.5ln|<emboss|<really-huge|<arg|name>>>>>>

  <assign|title-body-bg-color|#603030>

  <assign|title-body-color|#fff0c0>

  <assign|title-shape|angular>

  <assign|title-sunny-color|#301818>

  <assign|title-shadow-color|#201010>

  <assign|title-border|3ln>

  <\active*>
    <\src-comment>
      Plain blocks.
    </src-comment>
  </active*>

  <assign|plain-title-bg-color|none>

  <assign|plain-title-color|#e0c0c0>

  <assign|plain-body-bg-color|none>

  <assign|plain-body-color|#e0e0e0>

  <assign|plain-shape|angular>

  <\active*>
    <\src-comment>
      Framed blocks.
    </src-comment>
  </active*>

  <assign|framed-shape|angular>

  <assign|framed-title-style|top center>

  <assign|framed-title-bg-color|dark red>

  <assign|framed-title-color|#e0e0e0>

  <assign|framed-body-bg-color|#e0e0e0>

  <assign|framed-body-color|#200000>

  <assign|framed-sunny-color|#804040>

  <assign|framed-shadow-color|#402020>

  <\active*>
    <\src-comment>
      Alternate framed blocks.
    </src-comment>
  </active*>

  <assign|alternate-shape|angular>

  <assign|alternate-title-style|top left>

  <assign|alternate-title-bg-color|#602000>

  <assign|alternate-title-color|#e0d0c0>

  <assign|alternate-body-bg-color|#e0e0c0>

  <assign|alternate-body-color|#400000>

  <assign|alternate-sunny-color|#606030>

  <assign|alternate-shadow-color|#303018>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>