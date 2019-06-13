<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-theme|1.0|base-theme|1.0>

    <\src-purpose>
      Common base for most themes.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Main colors
    </src-comment>
  </active*>

  <new-theme|colors|bg-color|color|math-color|strong-color>

  <new-theme|no-patterns|monochrome-bg-color>

  <\active*>
    <\src-comment>
      Ornaments
    </src-comment>
  </active*>

  <new-theme|ornaments|ornament-color|ornament-extra-color|ornament-sunny-color|ornament-shadow-color|ornament-style|ornament-border|ornament-hpadding|ornament-vpadding>

  <\active*>
    <\src-comment>
      Title blocks
    </src-comment>
  </active*>

  <new-theme|titles|title-shape|title-bar-color|title-color>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <new-theme|sessions|input-color|fold-bar-color|fold-title-color>

  <\active*>
    <\src-comment>
      Poster title blocks
    </src-comment>
  </active*>

  <assign|title-body-bg-color|white>

  <assign|title-body-color|black>

  <assign|title-shape|classic>

  <assign|title-border|1ln>

  <assign|title-sunny-color|#e0c0c0>

  <assign|title-shadow-color|#f0e0e0>

  <new-theme|poster-title|title-body-bg-color|title-body-color|title-shape|title-border|title-sunny-color|title-shadow-color>

  <\active*>
    <\src-comment>
      Plain poster blocks.
    </src-comment>
  </active*>

  <assign|plain-shape|classic>

  <assign|plain-title-style|classic>

  <assign|plain-title-bg-color|none>

  <assign|plain-title-color|black>

  <assign|plain-body-bg-color|none>

  <assign|plain-body-color|black>

  <assign|plain-body-math-color|dark red>

  <assign|plain-body-strong-color|dark blue>

  <assign|plain-sunny-color|none>

  <assign|plain-shadow-color|none>

  <new-theme|poster-plain|plain-shape|plain-title-style|plain-title-bg-color|plain-title-color|plain-body-bg-color|plain-body-color|plain-math-color|plain-strong-color|plain-sunny-color|plain-shadow-color>

  <\active*>
    <\src-comment>
      Framed poster blocks.
    </src-comment>
  </active*>

  <assign|framed-shape|classic>

  <assign|framed-title-style|classic>

  <assign|framed-title-bg-color|dark blue>

  <assign|framed-title-color|pastel yellow>

  <assign|framed-body-bg-color|white>

  <assign|framed-body-color|dark brown>

  <assign|framed-body-math-color|dark red>

  <assign|framed-body-strong-color|dark blue>

  <assign|framed-sunny-color|#e0c0c0>

  <assign|framed-shadow-color|#f0e0e0>

  <new-theme|poster-framed|framed-shape|framed-title-style|framed-title-bg-color|framed-title-color|framed-body-bg-color|framed-body-color|framed-math-color|framed-strong-color|framed-sunny-color|framed-shadow-color>

  <\active*>
    <\src-comment>
      Alternate poster blocks.
    </src-comment>
  </active*>

  <assign|alternate-shape|classic>

  <assign|alternate-title-style|classic>

  <assign|alternate-title-bg-color|dark red>

  <assign|alternate-title-color|white>

  <assign|alternate-body-bg-color|#fff8e0>

  <assign|alternate-body-color|black>

  <assign|alternate-body-math-color|dark red>

  <assign|alternate-body-strong-color|dark blue>

  <assign|alternate-sunny-color|#e0c0c0>

  <assign|alternate-shadow-color|#f0e0e0>

  <new-theme|poster-alternate|alternate-shape|alternate-title-style|alternate-title-bg-color|alternate-title-color|alternate-body-bg-color|alternate-body-color|alternate-math-color|alternate-strong-color|alternate-sunny-color|alternate-shadow-color>

  <\active*>
    <\src-comment>
      Combine everything
    </src-comment>
  </active*>

  <copy-theme|base|colors|no-patterns|ornaments|titles|sessions|poster-title|poster-plain|poster-framed|poster-alternate>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>