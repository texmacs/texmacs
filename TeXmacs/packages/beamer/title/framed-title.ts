<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|framed-title|1.0>

    <\src-purpose>
      A style package for framed titles
    </src-purpose>

    <src-copyright|2013|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Customized title
    </src-comment>
  </active*>

  <assign|tit|<macro|body|<with|ornament-color|<value|title-bar-color>|<ornament|<title-left><htab|5mm><move|<with|font-series|bold|math-font-series|bold|<large|<with|color|<value|title-color>|math-color|<value|title-color>|<arg|body>>>>|0fn|0.333fn><htab|5mm><title-right>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>