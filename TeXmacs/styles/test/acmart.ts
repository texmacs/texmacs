<TeXmacs|1.99.6>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmart|1.0>

      <\src-purpose>
        New ACM base style.
      </src-purpose>

      <\src-copyright|2018>
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

  <use-package|std|env|title-generic|header-article|section-base>

  <active*|<\src-comment>
    TeX-like lengths.
  </src-comment>>

  <assign|tex-len|<macro|default|increase|decrease|<style-with|src-compact|none|<tmlen|<minus|<arg|default>|<arg|decrease>>|<arg|default>|<plus|<arg|default>|<arg|increase>>>>>>

  <active*|<src-comment|Global layout parameters>>

  <assign|font|Linux Libertine>

  <assign|page-width|6.75in>

  <assign|page-height|10in>

  <assign|page-type|user>

  \;

  <assign|page-odd|46pt>

  <assign|page-even|46pt>

  <assign|page-right|46pt>

  <assign|page-top|<plus|58pt|13pt|12pt>>

  <assign|page-bot|<plus|44pt|2pc|-5pt>>

  \;

  <assign|page-head-sep|13pt>

  <assign|page-foot-sep|2pc>

  \;

  <assign|par-first|<macro|10pt>>

  <active*|<src-comment|Font sizes>>

  <assign|tiny|<macro|x|<with|font-base-size|5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-base-size|7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-base-size|9|par-sep|1pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-base-size|10|par-sep|1pt|<arg|x>>>>

  <assign|large|<macro|x|<with|font-base-size|12|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-base-size|14|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-base-size|17|par-sep|5pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-base-size|20|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-base-size|25|par-sep|5pt|<arg|x>>>>

  <active*|<src-comment|Sectional macros>>

  <assign|sectional-sep|<macro|<space|4spc>>>

  <assign|section-font|<macro|name|<normal-size|<with|font-family|ss|<change-case|<arg|name>|UPCASE>>>>>

  <assign|subsection-font|<macro|name|<normal-size|<with|font-family|ss|<arg|name>>>>>

  <assign|subsubsection-font|<macro|name|<normal-size|<with|font-family|ss|<arg|name>>>>>

  <assign|paragraph-font|<macro|name|<normal-size|<arg|name>>>>

  <assign|subparagraph-font|<macro|name|<normal-size|<arg|name>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tmlen|0.75bls|0.55bls|0.95bls>><section-font|<arg|name>><vspace|0.25bls>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|<tmlen|0.75bls|0.55bls|0.55bls>><subsection-font|<arg|name>><vspace|0.25bls>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.5bls|0.3bls|0.7bls>><subsubsection-font|<arg|name>><vspace|3.5pt>>>>>

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.5bls|0.3bls|0.7bls>><paragraph-font|<arg|name>><space|1em>>>>>

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-normal-italic|<vspace*|<tmlen|0.25bls|0.15bls|0.35bls>><subparagraph-font|<arg|name>><space|1em>>>>>

  <\active*>
    <\src-comment>
      Rendering of theorem-like environments and exercises.
    </src-comment>
  </active*>

  <assign|render-enunciation|<\macro|which|body>
    <\padded*>
      <surround|<yes-indent><arg|which>|<yes-indent*>|<arg|body>>
    </padded*>
  </macro>>

  <assign|render-proof|<\macro|which|body>
    <\render-enunciation|<theorem-name|<arg|which>><remark-sep>>
      <surround||<htab|5mm><qed>|<arg|body>>
    </render-enunciation>
  </macro>>

  <assign|theorem-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|remark-name|<macro|name|<with|font-shape|italic|<arg|name>>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|odd-page-text|<macro|s|<assign|page-odd-header|<style-with|src-compact|none|<no-indent><with|font-family|ss|<arg|s>><htab|5mm><page-number>>>>>

  <assign|even-page-text|<macro|s|<assign|page-even-header|<style-with|src-compact|none|<no-indent><page-number><htab|5mm><with|font-family|ss|<arg|s>>>>>>

  \;

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|>

  <assign|page-even-footer|>

  \;

  <assign|header-title|<macro|name|<odd-page-text|<arg|name>><blanc-page>>>

  <assign|header-author|<macro|name|<even-page-text|<arg|name>><blanc-page>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Title information.
    </src-comment>
  </active*>

  <assign|doc-block|<\macro|body>
    <\wide-tabular>
      <tformat|<table|<row|<\cell>
        <\with|par-first|0fn>
          <arg|body>
        </with>
      </cell>>>>
    </wide-tabular>
  </macro>>

  <assign|doc-title-block|<\macro|body>
    <\doc-block>
      <arg|body>
    </doc-block>
  </macro>>

  <assign|doc-author-block|<\macro|body>
    <\doc-block>
      <arg|body>
    </doc-block>
  </macro>>

  <assign|doc-authors-block|<\macro|body>
    <\doc-block>
      <arg|body>
    </doc-block>
  </macro>>

  <assign|doc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.412|<with|font-family|ss|font-series|bold|<arg|x>>>>
  </surround>>>

  <assign|author-by|<macro|body|<arg|body>>>

  <assign|author-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<with|font-family|ss|<author-by|<change-case|<arg|author>|UPCASE>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>