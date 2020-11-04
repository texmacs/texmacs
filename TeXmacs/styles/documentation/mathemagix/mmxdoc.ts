<TeXmacs|1.99.2>

<style|<tuple|source|english>>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|mmxdoc|1.0>

      <\src-purpose>
        Style for the documentation of Mathemagix.
      </src-purpose>

      <\src-copyright|2003--2004>
        Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|tmdoc|tmdoc-web|framed-session>

  <\active*>
    <\src-comment>
      Html customization
    </src-comment>
  </active*>

  <assign|html-css|https://www.texmacs.org/css/mmxdoc.css>

  <\active*>
    <\src-comment>
      Fragments of mathemagix code.
    </src-comment>
  </active*>

  <assign|mmxweb-main-links|<macro|<style-with|src-compact|none|<tmweb-list|<tmweb-link|Home|index>|<tmweb-link|Download|download>|<tmweb-link|Language|language>|<tmweb-link|Packages|package_list>|<tmweb-link|Develop|develop>|<tmweb-link|Contact|contact>|<tmweb-link|Jobs|jobs>|<hlink|<with|color|brown|<localize|Search>>|http://www.mathemagix.org/search>>>>>

  <\active*>
    <\src-comment>
      Special logical types.
    </src-comment>
  </active*>

  <assign|type|<macro|x|<with|mode|text|font-family|ss|<arg|x>>>>

  <assign|andt|<active*|<with|mode|math|<space|0.6spc><wide*|\<wedge\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|ort|<active*|<with|mode|math|<space|0.6spc><wide*|\<vee\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|implt|<active*|<with|mode|math|<space|0.6spc><wide*|\<Rightarrow\><rsub|>|\<wide-bar\>><space|0.6spc>>>>

  <assign|forallt|<active*|<with|mode|math|<wide*|\<forall\><rsub|>|\<wide-bar\>><space|0.2spc>>>>

  <assign|existst|<active*|<with|mode|math|<wide*|\<exists\><rsub|>|\<wide-bar\>><space|0.2spc>>>>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|aldor-fragment|<value|scheme-fragment>>

  <assign|caml-fragment|<value|scheme-fragment>>

  <assign|todo|<macro|x|<with|color|red|To do: <arg|x>>>>

  <assign|mmx-link|<macro|name|<slink|<arg|name>>>><active*|<src-short-comment|TODO:
  find correct path>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>