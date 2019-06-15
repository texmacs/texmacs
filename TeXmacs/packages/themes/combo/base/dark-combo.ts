<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-combo|1.0|dark-combo|1.0>

    <\src-purpose>
      Dark theme.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|base-combo>

  <copy-theme|dark|base>

  <copy-theme|dark-ornament|rich-ornament>

  <\active*>
    <\src-comment>
      Major colors
    </src-comment>
  </active*>

  <assign|dark-ornament-bg-color|black>

  <assign|dark-ornament-monochrome-bg-color|black>

  <assign|dark-ornament-color|white>

  <assign|dark-ornament-math-color|white>

  <assign|dark-ornament-strong-color|white>

  <select-theme|dark|dark-ornament>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>