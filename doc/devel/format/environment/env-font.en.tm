<TeXmacs|1.0.3.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Specifying the current font>

  In this section, we describe the environment variables which control the
  rendering of fonts. Several parameters may be defined independently for
  each mode (the font name, variant, series and shape), whereas other
  parameters are uniform for all modes. Font properties may be controlled
  globally for the whole document in <menu|Document|Font> and locally for
  document fragments in <menu|Format|Font>.

  From an abstract point of view, a <em|font> is defined to be a graphically
  consistent way of rendering strings. Fonts are usually made up from glyphs
  like ``x'', ``ffi'', ``<with|mode|math|\<alpha\>>'',
  ``<with|mode|math|<op|<big|sum>><big|.>>'', <abbr|etc.> When rendering a
  string, the string is decomposed into glyphs so as to take into account
  ligatures (like fi, fl, ff, ffi, ffl). Next, the individual glyphs are
  positioned while taking into account kerning information (in ``xo'' the
  ``o'' character is slightly shifted to the left so as to take profit out of
  the hole in the ``x''). In the case of mathematical fonts, <TeXmacs> also
  provides a coherent rendering for resizable characters, like the large
  brackets in

  <\equation*>
    <left|(|0><left|(|1><left|(|2><right|)|2><right|)|1><right|)|0>.
  </equation*>

  Similarly, a <em|font family> is a family of fonts with different
  characteristics (like font weight, slant, <abbr|etc.>), but with a globally
  consistent rendering. One also says that the fonts in a font family ``mix
  well together''. For instance, the standard computer modern roman font and
  its <with|font-series|bold|bold> and <with|font-shape|italic|italic>
  variants mix well together, but the computer modern roman font and the
  <with|font|avant-garde|Avant Garde> font do not.

  <\remark>
    For the future, it is planned to replace the font variant and font shape
    variables by a larger range of properties to individually control the
    slant, serifs, small-caps, and so on. It is also planned to
    systematically use Unicode fonts with possible additional glyphs for
    mathematics. This should automatically enable the use of Cyrillic
    characters inside Russian text and similarly for other languages.
  </remark>

  <\explain>
    <var-val|font|roman>

    <var-val|math-font|roman>

    <var-val|prog-font|roman><explain-synopsis|font name>
  <|explain>
    These variables control the main name of the font, also called the
    <em|font family>. For instance:

    <\tm-fragment>
      <with|font|roman|Computer modern roman>, <with|font|chancery|Chancery>,
      <with|font|palatino|Palatino>
    </tm-fragment>

    Similarly, <TeXmacs> supports various mathematical fonts:

    <\tm-fragment>
      Roman: <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>

      Adobe: <with|mode|math|<with|math-font|adobe|a<rsup|2>+b<rsup|2>=c<rsup|2>>>

      <with|mode|math|a<rsup|2>+b<rsup|2>=c<rsup|2>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-family|rm>

    <var-val|math-font-family|mr>

    <var-val|prog-font-family|tt><explain-synopsis|font variant>
  <|explain>
    This variable selects a variant of the major font, like a sans serif
    font, a typewriter font, and so on. As explained above, variants of a
    given font are designed to mix well together. Physically speaking, many
    fonts do not come with all possible variants (sans serif, typewriter,
    <abbr|etc.>), in which case <TeXmacs> tries to fall back on a suitable
    alternative font.

    Typical variants for text fonts are <verbatim|rm> (roman), <verbatim|tt>
    (typewriter) and <verbatim|ss> (sans serif):

    <\tm-fragment>
      roman, <with|font-family|tt|typewriter> and <with|font-family|ss|sans
      serif>
    </tm-fragment>

    In maths mode, a distinction is made between the mathematical variants
    <verbatim|mr> (roman), <verbatim|mt> (typewriter) and <verbatim|ms> (sans
    serif) and textual variants <verbatim|rm> (roman), <verbatim|bf> (bold),
    etc. In the first case, variables and operators are usually rendered in a
    different slant, contrary to the second case:

    <\tm-fragment>
      <verbatim|ms>: <with|mode|math|<with|math-font-family|ms|sin (x+y)=sin
      x*sin y+cos x*cos y>>

      <verbatim|ss>: <with|mode|math|<with|math-font-family|ss|sin (x+y)=sin
      x*sin y+cos x*cos y>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-series|medium>

    <var-val|math-font-series|medium>

    <var-val|prog-font-series|medium><explain-synopsis|font weight>
  <|explain>
    The font series determines the weight of the font. Most fonts only
    provide <verbatim|regular> and <verbatim|bold> font weights. Some fonts
    also provide <verbatim|light> as a possible value.

    <\tm-fragment>
      <with|font-series|light|light>, medium, <with|font-series|bold|bold>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-shape|right>

    <var-val|math-font-shape|normal>

    <var-val|prog-font-shape|right><explain-synopsis|font shape>
  <|explain>
    Please document.
  </explain>

  <\explain>
    <var-val|font-base-size|10><explain-synopsis|font base size>
  <|explain>
    Please document.
  </explain>

  <\explain>
    <var-val|font-size|1><explain-synopsis|font size>
  <|explain>
    Please document.
  </explain>

  <\explain>
    <var-val|dpi|600><explain-synopsis|fonts rendering quality>
  <|explain>
    Please document.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>