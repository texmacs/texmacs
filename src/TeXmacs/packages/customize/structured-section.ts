<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|structured-section|1.0>

    <\src-purpose>
      Support for recursive sections and sectional tags with an optional
      body.
    </src-purpose>

    <src-copyright|1998--2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you do not have a copy of the license, then write to the
      Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Allow sectional tags to take an additional body argument. By default
      the additional argument is put on a new paragraph. The *-variant puts
      the addional argument on the same paragraph.
    </src-comment>
  </active*>

  <assign|section-level|<macro|<if|<sectional-short-style>|section|chapter>>>

  <assign|enrich-section-sub|<macro|name|subname|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|unary-|<arg|name>>>|<value|<unquote|<arg|name>>>><assign|<unquote|<merge|binary-short-|<arg|name>>>|<\macro|x|y>
    <surround|<compound|<unquote|<merge|unary-|<arg|name>>>|<arg|x>>|<right-flush>|<arg|y>>
  </macro>><assign|<unquote|<merge|binary-long-|<arg|name>>>|<\macro|x|y>
    <compound|<unquote|<merge|unary-|<arg|name>>>|<arg|x>>

    <surround||<right-flush>|<arg|y>>
  </macro>><assign|<unquote|<merge|binary-|<arg|name>>>|<\macro|x|y>
    <style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<compound|<unquote|<merge|enrich-|<arg|name>|-long>>>|<unquote|<merge|binary-long-|<arg|name>>>|<unquote|<merge|binary-short-|<arg|name>>>>>|<arg|x>|<arg|y>>>
  </macro>><assign|<unquote|<merge|unary-r|<arg|name>>>|<macro|x|<style-with|src-compact|none|<assign|section-level|<macro|<unquote|<arg|subname>>>><compound|<unquote|<merge|unary-|<arg|name>>>|<arg|x>>>>><assign|<unquote|<merge|binary-r|<arg|name>>>|<\macro|x|y>
    <style-with|src-compact|none|<compound|<unquote|<merge|binary-|<arg|name>>>|<arg|x>|<with|section-level|<macro|<unquote|<arg|subname>>>|<arg|y>>>>
  </macro>><assign|<unquote|<arg|name>>|<macro|x|y|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<equal|<quote-arg|y>|<uninit>>|<unquote|<merge|unary-r|<arg|name>>>|<unquote|<merge|binary-r|<arg|name>>>>>|<arg|x>|<arg|y>>>>>>>>>

  <assign|enrich-section|<macro|name|subname|<style-with|src-compact|none|<enrich-section-sub|<arg|name>|<arg|subname>><style-with|src-compact|none|<if|<equal|<value|<unquote|<merge|enrich-|<arg|name>|-long>>>|<uninit>>|<assign|<unquote|<merge|enrich-|<arg|name>|-long>>|<macro|true>>>>>>>

  <assign|enrich-section*|<macro|name|sub|<style-with|src-compact|none|<enrich-section-sub|<arg|name>|<arg|subname>><style-with|src-compact|none|<if|<equal|<value|<unquote|<merge|enrich-|<arg|name>|-long>>>|<uninit>>|<assign|<unquote|<merge|enrich-|<arg|name>|-long>>|<macro|false>>>>>>>

  <\active*>
    <\src-comment>
      Recursive sections and the auxiliary sectional tags
      <verbatim|subappendix>, <verbatim|subappendix*> and
      <verbatim|subparagraph**>.
    </src-comment>
  </active*>

  <assign|rsection|<macro|title|body|<style-with|src-compact|none|<compound|<section-level>|<arg|title>|<arg|body>>>>>

  <assign|rsection*|<macro|title|body|<style-with|src-compact|none|<compound|<merge|<section-level>|*>|<arg|title>|<arg|body>>>>>

  \;

  <assign|subappendix|<macro|title|body|<style-with|src-compact|none|<compound|<if|<sectional-short-style>|subsection|section>|<arg|title>|<arg|body>>>>>

  <assign|subappendix*|<macro|title|body|<style-with|src-compact|none|<compound|<if|<sectional-short-style>|subsection*|section*>|<arg|title>|<arg|body>>>>>

  <assign|subparagraph**|<macro|title|body|<subparagraph*|<arg|title>|<arg|body>>>>

  <\active*>
    <\src-comment>
      Enrich the standard sectional tags.
    </src-comment>
  </active*>

  <enrich-section|chapter|section>

  <enrich-section|chapter*|section>

  <enrich-section|section|subsection>

  <enrich-section|section*|subsection>

  <enrich-section|subsection|subsubsection>

  <enrich-section|subsection*|subsubsection>

  <enrich-section|subsubsection|paragraph>

  <enrich-section|subsubsection*|paragraph>

  <enrich-section|appendix|subappendix>

  <enrich-section|appendix*|subappendix>

  <enrich-section*|paragraph|subparagraph>

  <enrich-section*|paragraph*|subparagraph>

  <enrich-section*|subparagraph|subparagraph*>

  <enrich-section*|subparagraph*|subparagraph*>

  \;
</body>

<\initial>
  <\collection>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
    <associate|preamble|true>
    <associate|sfactor|4>
  </collection>
</initial>