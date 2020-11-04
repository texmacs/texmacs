<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|mmxmanual|1.0>

      <\src-purpose>
        Style for Mathemagix manuals.
      </src-purpose>

      <\src-copyright|2003--2012>
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

  <use-package|tmmanual|tmdoc-web|poorman-doxygen|mathemagix|framed-session>

  <\active*>
    <\src-comment>
      Html customization
    </src-comment>
  </active*>

  <assign|html-css|https://www.texmacs.org/css/mmxdoc.css>

  <\active*>
    <\src-comment>
      Miscellaneous.
    </src-comment>
  </active*>

  <assign|aldor-fragment|<value|scheme-fragment>>

  <assign|caml-fragment|<value|scheme-fragment>>

  <assign|todo|<macro|x|<with|color|red|To do: <arg|x>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>