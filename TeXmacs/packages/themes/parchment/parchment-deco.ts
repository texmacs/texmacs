<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|parchment-deco|1.0|parchment-deco|1.0>

    <\src-purpose>
      Parchment decorations for presentations and posters.
    </src-purpose>

    <src-copyright|2013--2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-package|transparent-deco|parchment-scene>

  <\active*>
    <\src-comment>
      Transparent parchment decoration
    </src-comment>
  </active*>

  <copy-theme|transparent-parchment-deco|transparent-light-deco>

  <new-deco|transparent-parchment-deco>

  <assign|transparent-parchment-deco-ornament-render-title|<value|with-transparent-parchment-title-scene>>

  <assign|transparent-parchment-deco-ornament-render-body|<value|with-transparent-parchment-scene>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>