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
      Main types of themes
    </src-comment>
  </active*>

  <new-theme|titles|title-bar-color|title-color|title-sunny-color|title-shadow-color|title-shape|title-border|title-vpadding|title-hor-padding|title-ver-padding|title-font-size>

  <new-theme|session|input-color|input-vpadding|input-border|input-border-color|fold-title-color|fold-bar-color|fold-bar-border|fold-bar-border-color>

  <new-theme|session-skin|with-input-deco|with-fold-title-deco|with-fold-bar-deco>

  <new-theme|poster|title-block|framed-block|framed-block*|alternate-block|alternate-block*>

  <new-theme|poster-ornament-shape|title-shape|framed-shape|alternate-shape>

  <copy-theme|base|colors|deco|titles|session|session-skin|poster|poster-ornament-shape>

  <\active*>
    <\src-comment>
      Determine title theme from decoration
    </src-comment>
  </active*>

  <assign|select-title-deco|<macro|combo|deco|<quasi|<style-with|src-compact|none|<complete-deco|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-title-bar-color>>|<value|<unquote|<merge|<arg|deco>|-ornament-title-bg-color>>>><assign|<unquote|<merge|<arg|combo>|-title-color>>|<value|<unquote|<merge|<arg|deco>|-ornament-title-color>>>><assign|<unquote|<merge|<arg|combo>|-title-sunny-color>>|<value|<unquote|<merge|<arg|deco>|-ornament-sunny-color>>>><assign|<unquote|<merge|<arg|combo>|-title-shadow-color>>|<value|<unquote|<merge|<arg|deco>|-ornament-shadow-color>>>><assign|<unquote|<merge|<arg|combo>|-title-shape>>|<value|<unquote|<merge|<arg|deco>|-ornament-shape>>>>>>>>

  <assign|select-title-deco*|<macro|combo|deco|<quasi|<style-with|src-compact|none|<select-title-deco|<unquote|<arg|combo>>|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-title-border>>|<value|<unquote|<merge|<arg|deco>|-ornament-border>>>>>>>>

  <assign|select-title-deco**|<macro|combo|deco|<quasi|<style-with|src-compact|none|<select-title-deco*|<unquote|<arg|combo>>|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-title-vpadding>>|<value|<unquote|<merge|<arg|deco>|-ornament-vpadding>>>><assign|<unquote|<merge|<arg|combo>|-title-hor-padding>>|<value|<unquote|<merge|<arg|deco>|-ornament-hpadding>>>><assign|<unquote|<merge|<arg|combo>|-title-ver-padding>>|<value|<unquote|<merge|<arg|deco>|-ornament-vpadding>>>>>>>>

  <\active*>
    <\src-comment>
      Determine session themes from decoration
    </src-comment>
  </active*>

  <assign|select-input-deco|<macro|combo|deco|<quasi|<style-with|src-compact|none|<complete-deco|<unquote|<arg|deco>>><copy-theme|<unquote|<merge|<arg|combo>|-input-deco>>|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-with-input-deco>>|<value|<unquote|<merge|with-|<arg|combo>|-input-deco>>>>>>>>

  <assign|select-fold-title-deco|<macro|combo|deco|<quasi|<style-with|src-compact|none|<complete-deco|<unquote|<arg|deco>>><copy-theme|<unquote|<merge|<arg|combo>|-fold-title-deco>>|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-with-fold-title-deco>>|<value|<unquote|<merge|with-|<arg|combo>|-fold-title-deco>>>>>>>>

  <assign|select-fold-bar-deco|<macro|combo|deco|<quasi|<style-with|src-compact|none|<complete-deco|<unquote|<arg|deco>>><copy-theme|<unquote|<merge|<arg|combo>|-fold-bar-deco>>|<unquote|<arg|deco>>><assign|<unquote|<merge|<arg|combo>|-with-fold-bar-deco>>|<value|<unquote|<merge|with-|<arg|combo>|-fold-bar-deco>>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>