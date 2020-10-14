<TeXmacs|1.99.13>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmarticle|1.0>

      <\src-purpose>
        A future style for <TeXmacs> articles.
      </src-purpose>

      <\src-copyright|1998--2004>
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

  <use-package|article>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\with|par-left|15mm|par-right|15mm>
      <\small>
        <\padded-bothlined|2.5bls|2.5bls|1ln|1ln|0.5bls|0.5bls>
          <arg|body>
        </padded-bothlined>
      </small>
    </with>
  </macro>>

  <assign|doc-render-title|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-title-block|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|<style-with|src-compact|none|<really-large|<arg|x>>>>>>>>

  <assign|author-by|<macro|body|<arg|body>>>

  <assign|author-render-name|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<with|font-shape|italic|<font-magnify|1.189|<arg|x>>>>>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-even-header|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<style-with|src-compact|none|<unquote|<page-number>><htab|5mm><with|font-shape|small-caps|<unquote|<arg|name>>>>>>>>>>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<with|font-shape|small-caps|<unquote|<arg|name>>><htab|5mm><unquote|<page-number>>>>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sectional tags.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|.<space|2spc>>>

  <assign|sectional-post-sep|<macro|<space|2spc>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|3fn><with|font-shape|small-caps|<really-large|<arg|name>>><vspace|2fn>>>>>

  <assign|chapter-numbered-title|<macro|name|<style-with|src-compact|none|<chapter*|<style-with|src-compact|none|<very-huge|<chapter-text>
  <the-chapter>><right-flush><vspace|1.5fn><new-line><left-flush><arg|name>>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|2fn><with|font-shape|small-caps|<larger|<arg|name>>><vspace|1fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><large|<arg|name>><vspace|0.5fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><arg|name><vspace|0.5fn>>>>>

  <\active*>
    <\src-comment>
      Other customization.
    </src-comment>
  </active*>

  <assign|tmhtml-render-enunciation|<\macro|which|body>
    <\padded*>
      <surround|<html-class|enunciation-name|<arg|which>>|<yes-indent*>|<arg|body>>
    </padded*>
  </macro>>

  <assign|tmhtml-render-theorem|<\macro|which|body>
    <\html-div-class|theorem>
      <render-enunciation|<theorem-name|<arg|which><theorem-sep>>|<with|font-shape|italic|<arg|body>>>
    </html-div-class>
  </macro>>

  <assign|theorem-name|<macro|name|<with|font-shape|small-caps|<arg|name>>>>

  <assign|toc-strong-2|<macro|left|right|<style-with|src-compact|none|<vspace*|1fn><with|font-series|bold|math-font-series|bold|font-shape|small-caps|<arg|left>><toc-dots><arg|right><vspace|0.5fn>>>>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>