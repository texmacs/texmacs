<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|tmconf|1.0>

      <\src-purpose>
        A future style for <TeXmacs> conference papers.
      </src-purpose>

      <\src-copyright|2021>
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

  <use-package|tmarticle|two-columns>

  <\active*>
    <\src-comment>
      Style parameters.
    </src-comment>
  </active*>

  <assign|page-type|letter>

  <assign|page-odd|2cm>

  <assign|page-even|2cm>

  <assign|page-right|2cm>

  <assign|page-top|2.25cm>

  <assign|page-bot|2cm>

  <\active*>
    <\src-comment>
      Title + abstract.
    </src-comment>
  </active*>

  <assign|headline|<macro|body|<with|font|Linux Biolinum|<arg|body>>>>

  <assign|doc-render-title|<macro|x|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-title-block|<headline|<with|math-font-series|bold|font-series|bold|font-shape|small-caps|<style-with|src-compact|none|<very-large|<arg|x>>>>>>>>>

  <assign|doc-title-name|<macro|x|<headline|<strong|<arg|x>>>>>

  <assign|xdoc-title|<macro|x|<\surround|<vspace*|0.5fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.682|<doc-title-name|<arg|x>>>>
  </surround>>>

  <assign|xdoc-subtitle|<macro|x|<\surround|<vspace*|0.25fn>|<vspace|0.5fn>>
    <doc-title-block|<font-magnify|1.297|<doc-title-name|<arg|x>>>>
  </surround>>>

  <assign|render-abstract|<\macro|body>
    <\padded-bothlined|2.5bls|2.5bls|1ln|1ln|0.5bls|0.5bls>
      <\headline>
        <\surround|<no-indent><strong|<abstract-text>. >|>
          <\em>
            <arg|body>
          </em>
        </surround>
      </headline>
    </padded-bothlined>
  </macro>>

  <assign|render-classify|<macro|scheme|text|<with|font-shape|right|<no-indent><strong|<arg|scheme><localize|:>>>
  <arg|text>>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|simple-page|<macro|<style-with|src-compact|none|<assign|page-this-header|><assign|page-this-footer|<headline|<no-indent><htab|5mm><page-number><htab|5mm>>>>>>

  <assign|header-title|<macro|name|<style-with|src-compact|none|<simple-page><assign|page-even-header|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<headline|<unquote|<page-number>><htab|5mm><unquote|<arg|name>>>>>>>>>>>

  <assign|header-author|<macro|name|<assign|page-odd-header|<quasiquote|<small|<style-with|src-compact|none|<wide-std-underlined|<headline|<unquote|<arg|name>><htab|5mm><unquote|<page-number>>>>>>>>>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Sectional tags.
    </src-comment>
  </active*>

  <assign|sectional-sep|<macro|<space|2spc>>>

  <assign|sectional-post-sep|<macro|<space|2spc>>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|2fn><headline|<very-large|<arg|name>>><vspace|1fn>>>>>

  <assign|chapter-numbered-title|<macro|name|<style-with|src-compact|none|<chapter*|<style-with|src-compact|none|<headline|<very-large|<chapter-text>
  <the-chapter>>><right-flush><vspace|1.5fn><new-line><left-flush><headline|<arg|name>>>>>>>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><headline|<large|<arg|name>>><vspace|0.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><headline|<sharp-size|<arg|name>>><vspace|0.5fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal|<vspace*|0.5fn><headline|<arg|name>><vspace|0.25fn>>>>>

  <\active*>
    <\src-comment>
      Lists.
    </src-comment>
  </active*>

  <assign|item-hsep|1.5tab>

  <assign|item-vsep|0.5spc>

  <assign|bibitem-width|1.5tab>

  <assign|transform-bibitem|<macro|body|<strong|[<arg|body>] >>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>