<TeXmacs|1.0.7.17>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|elsarticle|1.0>

      <\src-purpose>
        The elsarticle style.
      </src-purpose>

      <\src-copyright|2002--2004>
        François Poulain, Joris van der Hoeven
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <use-package|elsart>

  <use-module|(database title-markup-elsarticle)>

  \ <active*|<src-comment|Special texts.>>

  <assign|footnote-sep|<macro|>>

  <assign|noteref-stored-sep|<macro|,>>

  <assign|doc-footnote-text|<macro|sym|id|body|<custom-footnote-text|<rsup|<arg|sym>>|<arg|id>|<arg|body>>>>

  <assign|author-affiliation-text|<macro|sym|id|body|<rsup|<arg|sym>
  ><arg|body>>>

  <assign|doc-affiliations-bloc|<macro|body|<with|par-par-sep|0.2fn|<arg|body>>>>

  <assign|email-text|<macro|<localize|Email addresses:>>>

  <assign|homepage-text|<macro|<localize|URL:>>>

  \ <active*|<src-comment|Scheme wrappers.>>

  <assign|doc-data|<xmacro|args|<extern|doc-data-elsa|<quote-arg|args>>>>

  <assign|author-data|<xmacro|args|<extern|author-data-elsa|<quote-arg|args>>>>

  <assign|abstract-data-(todo)|<xmacro|args|<extern|abstract-data-elsa|<quote-arg|args>>>>

  \ <active*|<src-comment|Renderers.>>

  <assign|author-name|<macro|author|<surround|<vspace*|0.5fn>|<vspace|0.5fn>|<doc-author-block|<author-by|<arg|author>>>>>>

  <assign|authors-email|<macro|body|<with|font-shape|italic|<email-text>>
  <arg|body>>>

  <assign|authors-homepage|<macro|body|<with|font-shape|italic|<homepage-text>>
  <arg|body>>>

  <drd-props|authors-emails|arity|<tuple|repeat|1|1>|accessible|all>

  <drd-props|authors-homepages|arity|<tuple|repeat|1|1>|accessible|all>

  <assign|authors-emails|<xmacro|args|<extern|authors-emails|<quote-arg|args>>>>

  <assign|authors-homepages|<xmacro|args|<extern|authors-homepages|<quote-arg|args>>>>

  <active*|<src-comment|Footnote hacking.>>

  <assign|doc-note-ref|<macro|sym|sep|id|body|<custom-note-ref|<arg|sym>|<arg|sep>|<arg|id>|<arg|body>><assign|noteref-sep|<value|noteref-stored-sep>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
    <associate|sfactor|7>
  </collection>
</initial>