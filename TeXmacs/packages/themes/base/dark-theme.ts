<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-theme|1.0|dark-theme|1.0>

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

  <use-package|base-theme>

  <copy-theme|dark|base>

  <\active*>
    <\src-comment>
      Major colors
    </src-comment>
  </active*>

  <assign|dark-bg-color|black>

  <assign|dark-monochrome-bg-color|black>

  <assign|dark-color|white>

  <assign|dark-math-color|white>

  <assign|dark-strong-color|white>

  <\active*>
    <\src-comment>
      Standard ornaments
    </src-comment>
  </active*>

  <assign|dark-ornament-border|2ln>

  <assign|dark-ornament-hpadding|1spc>

  <assign|dark-ornament-vpadding|1spc>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>