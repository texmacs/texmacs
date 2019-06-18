<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|dark-vador-combo|1.0|dark-vador-combo|1.0>

    <\src-purpose>
      Dark vador theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|dark-combo|vador-deco>

  <copy-theme|dark-vador|dark>

  <select-theme|dark-vador|plain-vador>

  <select-theme|dark-vador|vador-background>

  <\active*>
    <\src-comment>
      Title bars
    </src-comment>
  </active*>

  <select-title-deco|dark-vador|title-vador>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|dark-vador|input-vador>

  <select-fold-title-deco|dark-vador|fold-vador>

  <select-fold-bar-deco|dark-vador|fold-vador>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|dark-vador-title-block|<value|title-vador-block>>

  <assign|dark-vador-framed-block|<value|framed-vador-block>>

  <assign|dark-vador-framed-block*|<value|framed-vador-titled-block>>

  <assign|dark-vador-alternate-block|<value|alternate-vador-block>>

  <assign|dark-vador-alternate-block*|<value|alternate-vador-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>