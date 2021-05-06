<TeXmacs|1.99.19>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|sigconf|1.0>

      <\src-purpose>
        The ACM sigconf style.
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

  <use-package|acmart|two-columns>

  <active*|<\src-comment>
    Global style parameters.
  </src-comment>>

  <assign|page-type|letter>

  <assign|page-width|auto>

  <assign|page-height|auto>

  <assign|font-base-size|9>

  \;

  <assign|page-odd|54pt>

  <assign|page-even|54pt>

  <assign|page-right|54pt>

  <assign|page-top|<plus|52pt|13pt|21pt>>

  <assign|page-bot|<plus|75pt|2pc|-18pt>>

  <assign|page-head-sep|<plus|13pt|3pt>>

  <assign|page-foot-sep|2pc>

  \;

  <assign|par-columns-sep|2pc>

  <assign|marginal-note-width|2pc>

  <assign|marginal-note-sep|11pt>

  <active*|<\src-comment>
    Sizes.
  </src-comment>>

  <assign|tiny|<macro|x|<with|font-size|<over|5|9>|par-sep|1pt|<arg|x>>>>

  <assign|very-small|<macro|x|<with|font-size|<over|6|9>|par-sep|1pt|<arg|x>>>>

  <assign|smaller|<macro|x|<with|font-size|<over|7|9>|par-sep|1pt|<arg|x>>>>

  <assign|small|<macro|x|<with|font-size|<over|7|9>|par-sep|2pt|<arg|x>>>>

  <assign|flat-size|<macro|x|<with|font-size|<over|8|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|normal-size|<macro|x|<with|font-size|<over|9|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|sharp-size|<macro|x|<with|font-size|<over|11|9>|par-sep|1.5pt|<arg|x>>>>

  <assign|large|<macro|x|<with|font-size|<over|12|9>|par-sep|2pt|<arg|x>>>>

  <assign|larger|<macro|x|<with|font-size|<over|14|9>|par-sep|4pt|<arg|x>>>>

  <assign|very-large|<macro|x|<with|font-size|<over|17|9>|par-sep|3pt|<arg|x>>>>

  <assign|huge|<macro|x|<with|font-size|<over|20|9>|par-sep|5pt|<arg|x>>>>

  <assign|really-huge|<macro|x|<with|font-size|<over|25|9>|par-sep|5pt|<arg|x>>>>

  <active*|<src-comment|Sectional macros>>

  <assign|section-font|<macro|name|<sharp-size|<change-case|<arg|name>|UPCASE>>>>

  <assign|subsection-font|<macro|name|<sharp-size|<arg|name>>>>

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

  <assign|conference-cr-data|XXX-X-XXXX-XXXX-X/XX/XX>

  <assign|conference-price|$15.00>

  <assign|conferenceinfo|<macro|name|infos|<assign|conference-name|<arg|name>><assign|conference-info|<arg|infos>>>>

  <assign|CopyrightYear|<macro|year|<assign|conference-copyright-year|<arg|year>>>>

  <assign|crdata|<macro|data|<assign|conference-cr-data|<arg|data>>>>

  <assign|permission|<macro|data|<assign|conference-boilerplate|<arg|data>>>>

  <assign|conference-copyrightetc|Copyright <value|conference-copyright-year>
  ACM <value|conference-cr-data> ...<value|conference-price>.>

  <assign|copyrightetc|<macro|data|<assign|conference-copyrightetc|<arg|data>>>>

  <assign|conference-permission-par-line-sep|0pt>

  <assign|conference-permission-font-base-size|7>

  <assign|make-conference-permissions|<macro|<with|par-par-sep|0pt|par-line-sep|<value|conference-permission-par-line-sep>|par-sep|1pt|font-base-size|<value|conference-permission-font-base-size>|<no-indent><value|conference-boilerplate><new-line><no-indent><with|font-shape|italic|<value|conference-name>>,
  <value|conference-info><new-line><no-indent><value|conference-copyrightetc>>>>

  <\active*>
    <\src-comment>
      Title information.
    </src-comment>
  </active*>

  <assign|doc-title-block|<\macro|body>
    <tabular*|<tformat|<twith|table-width|1par>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>
  </macro>>

  <assign|doc-author-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular*|<tformat|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-tsep|0spc>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<cwith|1|-1|1|-1|cell-bsep|-3pt>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  <assign|doc-authors-block|<\macro|body>
    <style-with|src-compact|none|<space|0pt><tabular*|<tformat|<twith|table-valign|T>|<cwith|1|1|1|1|cell-lsep|0spc>|<cwith|1|1|1|1|cell-rsep|0spc>|<cwith|1|1|1|1|cell-bsep|0.75fn>|<cwith|1|1|1|1|cell-tsep|0.75fn>|<cwith|1|1|1|1|cell-hyphen|t>|<cwith|1|1|1|1|cell-hmode|min>|<cwith|1|1|1|1|cell-width|1par>|<table|<row|<\cell>
      <\with|par-mode|center>
        <arg|body>
      </with>
    </cell>>>>>>
  </macro>>

  \;

  <assign|doc-title|<\macro|x>
    <\surround|<new-line><vspace*|<minus|2em|10pt>>|<vspace|1em>>
      <doc-title-block|<with|font-family|ss|font-series|bold|font-size|<over|18|9>|<arg|x>>>
    </surround>
  </macro>>

  <assign|doc-subtitle|<macro|x|<\surround|<vspace*|0.25fn>|<vspace|0.25fn>>
    <doc-title-block|<font-magnify|1.297|<with|font-family|ss|<arg|x>>>>
  </surround>>>

  <assign|doc-data|<xmacro|args|<extern|doc-data|<quote-arg|args>|>>>

  <\active*>
    <\src-comment>
      Author information.
    </src-comment>
  </active*>

  <assign|doc-authors|<\xmacro|data>
    <\style-with|src-compact|none>
      <\quasi>
        <\doc-author>
          <\with|doc-author|<value|doc-author*>>
            <space|0spc><unquote*|<map|padded-author|<quote-arg|data>>>
          </with>
        </doc-author>
      </quasi>
    </style-with>
  </xmacro>>

  <assign|author-name|<macro|author|<surround|<vspace*|0.25fn>|<vspace|0.5fn>|<doc-author-block|<with|font-size|<over|12|9>|<arg|author>>>>>>

  <assign|author-affiliation|<\macro|address>
    <surround|<vspace*|0.25fn>|<vspace|0.25fn>|<doc-author-block|<with|font-size|<over|10|9>|<arg|address>>>>
  </macro>>

  <assign|author-email|<macro|email|<doc-author-block|<arg|email>>>>

  <assign|author-homepage|<macro|homepage|<doc-author-block|<arg|homepage>>>>

  <\active*>
    <\src-comment>
      Abstract information.
    </src-comment>
  </active*>

  <assign|render-abstract|<\macro|body>
    <section*|<abstract-text>>

    <surround|<no-indent>|<float|footnote||<smaller|<make-conference-permissions>>>|<arg|body>>
  </macro>>

  <assign|render-abstract*|<\macro|body|notes>
    <\render-abstract>
      <arg|body>

      <arg|notes>
    </render-abstract>
  </macro>>

  <assign|abstract-category-item|<macro|ind|cat|sub|det|<arg|ind><if|<unequal|<arg|cat>|<uninit>>|
  [<with|font-series|bold|<arg|cat>>]><if|<unequal|<arg|sub>|<uninit>>|:
  <arg|sub>><if|<unequal|<arg|det>|<uninit>>|\V<with|font-shape|italic|<arg|det>>>>>

  <assign|abstract-category|<\xmacro|args>
    <section*|CSS Concepts>

    <no-indent><concat-tuple|<quote-arg|args>|; >
  </xmacro>>

  <assign|abstract-terms|<\xmacro|args>
    <section*|General Terms>

    <no-indent><concat-tuple|<quote-arg|args>|, >
  </xmacro>>

  <assign|abstract-keywords|<\xmacro|args>
    <section*|<keywords-text>>

    <no-indent><concat-tuple|<quote-arg|args>|, >
  </xmacro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>