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

  <\active*>
    <\src-comment>
      Dark translucent parchment decoration
    </src-comment>
  </active*>

  <copy-theme|dark-parchment-deco|transparent-dark-deco>

  <new-deco|dark-parchment-deco>

  <assign|dark-parchment-deco-ornament-render-title|<value|with-dark-parchment-scene>>

  <assign|dark-parchment-deco-ornament-render-body|<value|with-dark-parchment-scene>>

  <assign|dark-parchment-deco-ornament-sunny-color|#fff4>

  <assign|dark-parchment-deco-ornament-shadow-color|#8404>

  <\active*>
    <\src-comment>
      Extra dark translucent parchment decoration
    </src-comment>
  </active*>

  <copy-theme|xdark-parchment-deco|transparent-dark-deco>

  <new-deco|xdark-parchment-deco>

  <assign|xdark-parchment-deco-ornament-render-title|<value|with-xdark-parchment-scene>>

  <assign|xdark-parchment-deco-ornament-render-body|<value|with-xdark-parchment-scene>>

  <assign|xdark-parchment-deco-ornament-sunny-color|#fff4>

  <assign|xdark-parchment-deco-ornament-shadow-color|#8404>

  <\active*>
    <\src-comment>
      Light translucent parchment decoration
    </src-comment>
  </active*>

  <copy-theme|light-parchment-deco|transparent-light-deco>

  <new-deco|light-parchment-deco>

  <assign|light-parchment-deco-ornament-render-title|<value|with-dark-parchment-scene>>

  <assign|light-parchment-deco-ornament-render-body|<value|with-light-parchment-scene>>

  <assign|light-parchment-deco-ornament-sunny-color|#fff4>

  <assign|light-parchment-deco-ornament-shadow-color|#8404>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>