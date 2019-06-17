<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-combo|1.0|base-combo|1.0>

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

  <use-package|base-deco>

  <\active*>
    <\src-comment>
      Title blocks
    </src-comment>
  </active*>

  <new-theme|titles|title-bar-color|title-color|title-sunny-color|title-shadow-color|title-shape|title-border|title-vpadding|title-hor-padding|title-ver-padding|title-font-size>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <new-theme|sessions|input-color|fold-bar-color|fold-title-color>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <new-theme|poster|title-block|framed-block|framed-block*|alternate-block|alternate-block*>

  <new-theme|poster-ornament-shape|title-shape|framed-shape|alternate-shape>

  <\active*>
    <\src-comment>
      Combine everything
    </src-comment>
  </active*>

  <copy-theme|base|colors|deco|titles|sessions|poster|poster-ornament-shape>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>