<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|parchment-combo|1.0|parchement-combo|1.0>

    <\src-purpose>
      Parchment theme for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|light-combo|parchment-deco>

  <copy-theme|parchment|light>

  <select-theme|parchment|light-parchment-deco>

  <select-theme|parchment|parchment-scene>

  <\active*>
    <\src-comment>
      Titles
    </src-comment>
  </active*>

  <select-title-deco*|parchment|xdark-parchment-deco>

  <assign|parchment-title-border|0.5ln>

  <\active*>
    <\src-comment>
      Sessions
    </src-comment>
  </active*>

  <select-input-deco|parchment|light-parchment-deco>

  <select-fold-title-deco|parchment|dark-parchment-deco>

  <select-fold-bar-deco|parchment|dark-parchment-deco>

  <\active*>
    <\src-comment>
      Posters
    </src-comment>
  </active*>

  <assign|parchment-title-block|<value|xdark-parchment-deco-block>>

  <assign|parchment-framed-block|<value|blue-parchment-deco-block>>

  <assign|parchment-framed-block*|<value|blue-parchment-deco-titled-block>>

  <assign|parchment-alternate-block|<value|red-parchment-deco-block>>

  <assign|parchment-alternate-block*|<value|red-parchment-deco-titled-block>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>