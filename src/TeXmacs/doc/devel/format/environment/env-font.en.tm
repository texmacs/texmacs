<TeXmacs|2.1.4>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths|libertine-font>>

<\body>
  <tmdoc-title|Specifying the current font>

  In this section, we describe the environment variables which control the
  rendering of fonts. Font properties may be controlled globally for the
  whole document in <menu|Document|Font> and locally for document fragments
  in <menu|Format|Font>.

  From an abstract point of view, a <em|font> is defined to be a graphically
  consistent way of rendering strings. Fonts are usually made up from glyphs
  like \Px\Q, \Pffi\Q, \P<math|\<alpha\>>\Q, \P<math|<op|<big|sum>>>\Q,
  <abbr|etc.> When rendering a string, the string is decomposed into glyphs
  so as to take into account ligatures (like fi, fl, ff, ffi, ffl). Next, the
  individual glyphs are positioned while taking into account kerning
  information (in \Pxo\Q the \Po\Q character is slightly shifted to the left
  so as to take profit out of the hole in the \Px\Q). In the case of
  mathematical fonts, <TeXmacs> also provides a coherent rendering for
  resizable characters, like the large brackets in

  <\equation*>
    <around*|(|<around*|(|<around*|(||)><rsup|<rsup|\<nosymbol\>>>|)><rsup|<rsup|<rsup|\<nosymbol\>>>>|)>.
  </equation*>

  Similarly, a <em|font family> is a family of fonts with different
  characteristics (like font weight, slant, <abbr|etc.>), but with a globally
  consistent rendering. One also says that the fonts in a font family \Pmix
  well together\Q. For instance, the standard computer modern roman font and
  its <with|font-series|bold|bold> and <with|font-shape|italic|italic>
  variants mix well together, but the computer modern roman font and the
  <with|font|avant-garde|Avant Garde> font do not.

  <\remark>
    In versions of <TeXmacs> prior to 1.99.1, the fonts for the mathematical
    and programming modes could be controlled independently using the
    environment variables <src-var|math-font>, <src-var|math-font-family>,
    <src-var|math-font-series>, <src-var|math-font-shape>,
    <src-var|prog-font>, <src-var|prog-font-family>,
    <src-var|prog-font-series>, <src-var|prog-font-shape>. In more recent
    versions of <TeXmacs>, the environment variables <src-var|font>,
    <src-var|font-family>, <src-var|font-series> and <src-var|font-shape>
    directly control the font for all modes.
  </remark>

  <\explain>
    <var-val|font|roman><explain-synopsis|font name>
  <|explain>
    These variables control the main name of the font, also called the
    <em|font family>. For instance:

    <\tm-fragment>
      <with|font|roman|Computer modern roman>, <with|font|Linux
      Libertine|Linux Libertine>, <with|font|chancery|Chancery>,
      <with|font|palatino|Palatino>
    </tm-fragment>

    Similarly, <TeXmacs> supports various mathematical fonts:

    <\tm-fragment>
      Roman: <math|a<rsup|2>+b<rsup|2>=c<rsup|2>>

      Adobe: <math|<with|math-font|adobe|a<rsup|2>+b<rsup|2>=c<rsup|2>>>

      New roman: <math|<with|math-font|ENR|a<rsup|2>+b<rsup|2>=c<rsup|2>>>

      Concrete: <math|<with|math-font|concrete|a<rsup|2>+b<rsup|2>=c<rsup|2>>>
    </tm-fragment>

    Most fonts only implement a subset of all Unicode glyphs. Sometimes, the
    user might wish to combine several fonts to cover a larger subset. For
    instance, when specifying <verbatim|roman,IPAMincho> or
    <verbatim|cjk=IPAMincho,roman> as the <src-var|font> name, ordinary text
    and mathematics will be typeset using the default <verbatim|roman> font,
    whereas Chinese text will use the <verbatim|IPAMincho> font. Similarly,
    when specifying <verbatim|math=Stix,roman> as the <src-var|font> name,
    ordinary text will be typeset using the default <verbatim|roman> font,
    but mathematical formulas using the <verbatim|Stix> font.
  </explain>

  <\explain>
    <var-val|font-family|rm><explain-synopsis|font variant>
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

      Sans serif formula: <with|font-family|ss|<math|sin
      <around*|(|x+y|)>=sin x*cos y+cos x*sin y>>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-series|medium><explain-synopsis|font weight>
  <|explain>
    The font series determines the weight of the font. Most fonts only
    provide <verbatim|regular> and <verbatim|bold> font weights. Some fonts
    also provide <verbatim|light> as a possible value.

    <\tm-fragment>
      medium, <with|font-series|bold|bold>
    </tm-fragment>
  </explain>

  <\explain>
    <var-val|font-shape|right><explain-synopsis|font shape>
  <|explain>
    The font shape determines other characters of a font, like its slant,
    whether we use small capitals, whether it is condensed, and so on. For
    instance,

    <\tm-fragment>
      <with|font-shape|right|upright>, <with|font-shape|slanted|slanted>,
      <with|font-shape|italic|italic>, <with|font-shape|left-slanted|left
      slanted>, <with|font-shape|small-caps|Small Capitals>,
      <with|font-shape|proportional|<with|font-family|tt|proportional
      typewriter>>, <with|font-shape|condensed|<with|font-series|bold|bold
      condensed>>, <with|font-shape|flat|<with|font-family|ss|flat sans
      serif>>, <with|font-shape|long|long>
    </tm-fragment>
  </explain>

  <\explain>
    <label|font-base-size><var-val|font-base-size|10><explain-synopsis|font
    base size>
  <|explain>
    The base font size is specified in <hlink|<verbatim|pt>
    units|../basics/lengths.en.tm> and is usually invariant throughout the
    document. Usually, the base font size is <verbatim|9pt>, <verbatim|10pt>,
    <verbatim|11pt> or <verbatim|12pt>. Other font sizes are usually obtained
    by changing the <hlink|<src-var|magnification>|env-general.en.tm#magnification>
    or the relative <hlink|font-size|#font-size>.

    <\tm-fragment>
      <with|font-base-size|9|9pt>, <with|font-base-size|10|10pt>,
      <with|font-base-size|11|11pt>, <with|font-base-size|12|12pt>
    </tm-fragment>
  </explain>

  <\explain>
    <label|font-size><var-val|font-size|1><explain-synopsis|font size>
  <|explain>
    The real font size is obtained by multiplying the
    <src-var|font-base-size> by the <src-var|font-size> multiplier. The
    following standard font sizes are available from <menu|Format|Size>:

    <big-table|<descriptive-table|<tformat|<cwith|4|5|1|4|cell-bsep|0.25fn>|<cwith|4|5|1|4|cell-tsep|0.25fn>|<table|<row|<cell|size>|<cell|multiplier>|<cell|size>|<cell|multiplier>>|<row|<cell|<with|font-size|0.59|Tiny>>|<cell|0.59>|<cell|<with|font-size|0.71|Very
    small>>|<cell|0.71>>|<row|<cell|<with|font-size|0.84|Small>>|<cell|0.84>|<cell|<with|font-size|1|Normal>>|<cell|1>>|<row|<cell|<with|font-size|1.19|Large>>|<cell|1.19>|<cell|<with|font-size|1.41|Very
    large>>|<cell|1.41>>|<row|<cell|<with|font-size|1.68|Huge>>|<cell|1.68>|<cell|<with|font-size|2|Really
    huge>>|<cell|2>>>>>|Standard font sizes.>

    From a mathematical point of view, the multipliers are in a geometric
    progression with factor <no-break><math|<sqrt|2|4>>. Notice that the font
    size is also affected by the <hlink|index
    level|env-math.en.tm#math-level>.
  </explain>

  <\explain>
    <var-val|dpi|600><explain-synopsis|fonts rendering quality>
  <|explain>
    The rendering quality of raster fonts (also called Type 3 fonts), such as
    the fonts generated by the <name|Metafont> program is controlled through
    its discretization precision in dots per inch. Nowadays, most laser
    printers offer a printing quality of at least <verbatim|600dpi>, which is
    also the default <src-var|dpi> setting for <TeXmacs>. For really high
    quality printing, professionals usually use a precision of
    <verbatim|1200dpi>. The <src-var|dpi> is usually set once and for all for
    the whole document.
  </explain>

  <tmdoc-copyright|2004|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<initial|<\collection>
</collection>>