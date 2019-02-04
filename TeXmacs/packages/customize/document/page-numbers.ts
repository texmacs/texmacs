<TeXmacs|1.99.8>

<style|<tuple|source|std-pattern>>

<\body>
  <active*|<\src-title>
    <src-package-dtd|page-numbers|1.0|page-numbers|1.0>

    <\src-purpose>
      Revert to usual headers and footers for styles that suppress them by
      default
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <assign|no-page-numbers|false>

  <if|<unequal|<value|normal-page-head-sep>|<uninit>>|<assign|page-head-sep|<value|normal-page-head-sep>>>

  <if|<unequal|<value|normal-page-foot-sep>|<uninit>>|<assign|page-foot-sep|<value|normal-page-foot-sep>>>

  <if|<unequal|<value|normal-page-odd-header>|<uninit>>|<assign|page-odd-header|<value|normal-page-odd-header>>>

  <if|<unequal|<value|normal-page-even-header>|<uninit>>|<assign|page-even-header|<value|normal-page-even-header>>>

  <if|<unequal|<value|normal-page-odd-footer>|<uninit>>|<assign|page-odd-footer|<value|normal-page-odd-footer>>>

  <if|<unequal|<value|normal-page-even-footer>|<uninit>>|<assign|page-even-footer|<value|normal-page-even-footer>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>