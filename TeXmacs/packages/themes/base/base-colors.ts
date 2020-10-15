<TeXmacs|1.99.13>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package-dtd|base-colors|1.0|base-colors|1.0>

    <\src-purpose>
      Common base for coloring schemes.
    </src-purpose>

    <src-copyright|2019|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Standard colors
    </src-comment>
  </active*>

  <new-theme|colors|bg-color|monochrome-bg-color|color|math-color|strong-color|heading-color|locus-color|visited-color>

  <\active*>
    <\src-comment>
      Uniform coloring
    </src-comment>
  </active*>

  <assign|make-uniform|<macro|theme|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|theme>|-math-color>>|<value|<unquote|<merge|<arg|theme>|-color>>>><assign|<unquote|<merge|<arg|theme>|-strong-color>>|<value|<unquote|<merge|<arg|theme>|-color>>>><assign|<unquote|<merge|<arg|theme>|-heading-color>>|<value|<unquote|<merge|<arg|theme>|-color>>>><assign|<unquote|<merge|<arg|theme>|-locus-color>>|<value|<unquote|<merge|<arg|theme>|-color>>>><assign|<unquote|<merge|<arg|theme>|-visited-color>>|<value|<unquote|<merge|<arg|theme>|-color>>>>>>>>

  <assign|assign-uniform|<macro|theme|val|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|theme>|-color>>|<unquote|<arg|val>>><make-uniform|<unquote|<arg|theme>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|sfactor|7>
  </collection>
</initial>