<TeXmacs|1.99.19>

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

  <use-package|std|env|title-generic|header-article|section-base|std-latex-base|html-font-size>

  <active*|<src-comment|Global layout parameters>>

  <assign|font|Linux Libertine>

  <assign|font|math basic-letters=Linux Libertine,math=termes,Linux
  Libertine>

  <assign|math-font-sizes|<tuple|<tuple|all|*0.8|*0.6|*0.5>>>

  <assign|par-first|<macro|10pt>>

  <active*|<src-comment|Font sizes>>

  <assign|tiny|<macro|x|<with|font-size|0.5|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|0.7|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|0.9|par-sep|1pt|<arg|x>>>>

  <assign|flat-size|<macro|x|<with|font-size|0.9|par-sep|1pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-size|1.0|par-sep|1pt|<arg|x>>>>

  <assign|sharp-size|<macro|x|<with|font-size|1.1|par-sep|1.5pt|<arg|x>>>>

  <assign|large|<macro|x|<with|font-size|1.2|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|1.4|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|1.7|par-sep|5pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|2.0|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|2.5|par-sep|5pt|<arg|x>>>>

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
      <surround||<if|<occurs-inside|<quote|<qed>>|body>||<htab|5mm><qed>>|<arg|body>>
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
      <tformat|<cwith|1|1|1|1|cell-vcorrect|n>|<table|<row|<\cell>
        <\with|par-first|0fn|par-sep|0fn|par-par-sep|0fn>
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

  <assign|doc-make-title|<macro|body|<\surround||<vspace|0.5fn>>
    <\compact>
      <\doc-title-block>
        <arg|body>

        \;
      </doc-title-block>
    </compact>
  </surround>>>

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>|<tuple|abbreviate-authors|cluster-by-affiliation>>>>

  <\active*>
    <\src-comment>
      Author information.
    </src-comment>
  </active*>

  <assign|author-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<large|<with|font-family|ss|<change-case|<arg|author>|UPCASE>>>>>>>

  <assign|author-name-affiliation|<macro|author|address|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<large|<with|font-family|ss|<change-case|<arg|author>|UPCASE>,
  >><arg|address>>>>>

  <assign|vertical-author|<\macro|data>
    <arg|data>
  </macro>>

  <assign|doc-authors|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\doc-author>
          <\with|doc-author|<value|doc-author*>>
            <unquote*|<map|vertical-author|<quote-arg|data>>>
          </with>
        </doc-author>
      </quasi>
    </style-with>
  </xmacro>>

  <\active*>
    <\src-comment>
      Abstracts.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\surround|<no-indent>|>
      <\small>
        <arg|body>
      </small>
    </surround>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>