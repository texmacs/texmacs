<TeXmacs|1.99.9>

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

  <assign|title-theme|framed-title>

  <assign|tit|<macro|body|<with|color|<title-color>|font-size|<if|<value|title-old-style>|1.189|<value|title-font-size>>|math-color|<title-color>|strong-color|<title-color>|ornament-color|<title-bar-color>|ornament-vpadding|<plus|<value|ornament-vpadding>|<value|title-vpadding>>|<ornament|<title-left|<arg|body>><htab|5mm><with|font-series|bold|math-font-series|bold|<space|0em|-0.6ex|1.6ex><arg|body>><htab|5mm><title-right|<arg|body>><assign|gpag-length|<macro|<minus|1pag|3fn>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>