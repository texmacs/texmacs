<TeXmacs|1.99.1>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|acmconf|1.0>

      <\src-purpose>
        The acmconf style.
      </src-purpose>

      <\src-copyright|2002--2004>
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

  <use-package|std|env|title-generic|header-article|section-article|two-columns>

  <active*|<\src-comment>
    Conference information and copyrights
  </src-comment>>

  <assign|conference-boilerplate|Permission to make digital or hard copies of
  all or part of this work for personal or classroom use is granted without
  fee provided that copies are not made or distributed for profit or
  commercial advantage and that copies bear this notice and the full citation
  on the first page. \ To copy otherwise, to republish, to post on servers or
  to redistribute to lists, requires prior specific permission and/or a fee.>

  <assign|conference-name|(Please declare
  <with|font-shape|right|<src-macro|conferenceinfo>>,
  <with|font-shape|right|<src-macro|CopyrightYear>>, and
  <with|font-shape|right|<src-macro|crdata>> in your preamble, following ACM
  guidelines:<next-line><hlink|http://www.acm.org/sigs/publications/proceedings-templates|http://www.acm.org/sigs/publications/proceedings-templates>)>

  <assign|conference-info|>

  <assign|conference-copyright-year|20XX>

  <assign|conference-cr-data|X-XXXXX-XX-X/XX/XX>

  <assign|conference-price|$15.00>

  <assign|conferenceinfo|<macro|name|infos|<assign|conference-name|<arg|name>><assign|conference-info|<arg|infos>>>>

  <assign|CopyrightYear|<macro|year|<assign|conference-copyright-year|<arg|year>>>>

  <assign|crdata|<macro|data|<assign|conference-cr-data|<arg|data>>>>

  <assign|make-conference-permissions|<macro|<with|font|times|par-par-sep|0pt|par-line-sep|0pt|par-sep|1pt|font-base-size|8pt|<no-indent><value|conference-boilerplate><new-line><no-indent><with|font-shape|italic|<value|conference-name>>,
  <value|conference-info><new-line><no-indent>Copyright
  <value|conference-copyright-year> ACM <value|conference-cr-data>
  ...<value|conference-price>.>>>

  <\active*>
    <\src-comment>
      Global lay-out.
    </src-comment>
  </active*>

  <assign|font-base-size|9>

  <assign|page-odd|20mm>

  <assign|page-even|20mm>

  <assign|page-right|20mm>

  <assign|page-top|25mm>

  <assign|page-bot|25mm>

  <\active*>
    <\src-comment>
      Titles.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <\padded-normal|1.5fn|1.5fn>
      <with|font-series|bold|<abstract-text>><vspace|1.5fn><no-page-break>

      <surround|<no-indent>||<arg|body>>
    </padded-normal>
  </macro>>

  <\active*>
    <\src-comment>
      Headers.
    </src-comment>
  </active*>

  <assign|page-odd-header|>

  <assign|page-even-header|>

  <assign|page-odd-footer|<htab|5mm><page-number><htab|5mm>>

  <assign|page-even-footer|<htab|5mm><page-number><htab|5mm>>

  \;

  <assign|header-title|<macro|name|>>

  <assign|header-author|<macro|name|>>

  <assign|header-primary|<macro|name|nr|what|>>

  <assign|header-secondary|<macro|name|nr|what|>>

  <\active*>
    <\src-comment>
      Chapters.
    </src-comment>
  </active*>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|3fn><large|<arg|name>><vspace|1fn>>>>>

  <\active*>
    <\src-comment>
      Sections, subsections, subsubsections.
    </src-comment>
  </active*>

  <assign|section-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><arg|name><vspace|1.5fn>>>>>

  <assign|subsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1.5fn><arg|name><vspace|0.75fn>>>>>

  <assign|subsubsection-title|<macro|name|<style-with|src-compact|none|<sectional-normal-bold|<vspace*|1fn><arg|name><vspace|0.5fn>>>>>

  <\active*>
    <\src-comment>
      Other customizations.
    </src-comment>
  </active*>

  <assign|xaligned-item|<macro|name|<style-with|src-compact|none|<vspace*|<item-vsep>><with|par-first|<minus|<item-hsep>>|<yes-indent>><resize|<with|math-font-series|bold|font-series|bold|<arg|name>>
  |<minus|1r|<item-hsep>>||1r|>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>