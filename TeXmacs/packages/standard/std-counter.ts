<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|std-counter|1.0>

    <\src-purpose>
      This package manages counters for sections, theorems, etc.
    </src-purpose>

    <src-copyright|2004|Joris van der Hoeven>

    <\src-license>
      This <TeXmacs> style package falls under the <hlink|GNU general public
      license|$TEXMACS_PATH/LICENSE> and comes WITHOUT ANY WARRANTY
      WHATSOEVER. If you don't have this file, then write to the Free
      Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
      02111-1307, USA.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      The <verbatim|new-counter> primitive is used for the definition of a
      new counter. Each new counter <verbatim|x> gives rise to several new
      environment variables: <verbatim|nr-x> contains the counter itself (an
      integer), <verbatim|display-x> a macro which will be applied to the
      counter for displaying it, <verbatim|counter-x> a macro which returns
      the name of the counter (<verbatim|nr-x> by default),
      <verbatim|the-counter> a macro which returns the counter for display,
      <verbatim|reset-x> a macro which resets the counter, and
      <verbatim|inc-x> a macro which increases the counter. The different
      macros may be changed in order to customize the behaviour of the
      counter; this feature is used in particular by the grouping primitives
      defined further below.
    </src-comment>
  </active*>

  <assign|new-counter|<macro|x|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|nr-|<arg|x>>>|0><assign|<unquote|<merge|display-|<arg|x>>>|<value|identity>><assign|<unquote|<merge|counter-|<arg|x>>>|<macro|<style-with|src-compact|none|<unquote|<merge|nr-|<arg|x>>>>>><assign|<unquote|<merge|the-|<arg|x>>>|<macro|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|x>>>|<value|<compound|<unquote|<merge|counter-|<arg|x>>>>>>>>><assign|<unquote|<merge|reset-|<arg|x>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|0>>>><assign|<unquote|<merge|inc-|<arg|x>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|<plus|<value|<compound|<unquote|<merge|counter-|<arg|x>>>>>|1>>>>>>>>>

  <assign|the-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|the-|<arg|x>>>>>>

  <assign|reset-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|reset-|<arg|x>>>>>>

  <assign|inc-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|inc-|<arg|x>>>>>>

  <assign|next-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|inc-|<arg|x>>><assign|thelabel|<compound|<merge|the-|<arg|x>>>>>>>

  <\active*>
    <\src-comment>
      Counters for similar environments (like <verbatim|theorem>,
      <verbatim|proposition>, etc.) can be grouped together from a logical
      point of view into ``counter groups''. When using this feature, the
      different environments can either share a common counter
      (<verbatim|group-common-counter>; ``american style'') or continue to
      use their own counters (<verbatim|group-individual-counters>;
      ``european style''). When calling a macro like <verbatim|reset-g> for a
      counter group <verbatim|g>, all counters in the group are set to zero.
      Similarly, sectional prefixes can be shared by the environments in a
      group.
    </src-comment>
  </active*>

  <assign|new-counter-group|<macro|g|<style-with|src-compact|none|<assign|<merge|<arg|g>|-group>|<tuple>><new-counter|<arg|g>><group-common-counter|<arg|g>>>>>

  <assign|add-to-counter-group|<macro|x|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|g>|-group>>|<merge|<value|<merge|<arg|g>|-group>>|<tuple|<arg|x>>>><new-counter|<arg|x>><assign|<unquote|<merge|ind-display-|<arg|x>>>|<value|<unquote|<merge|display-|<arg|x>>>>><assign|<unquote|<merge|ind-counter-|<arg|x>>>|<value|<unquote|<merge|counter-|<arg|x>>>>><assign|<unquote|<merge|display-|<arg|x>>>|<macro|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-in-|<arg|g>>>|<unquote|<arg|x>>|<arg|nr>>>>><assign|<unquote|<merge|counter-|<arg|x>>>|<macro|<style-with|src-compact|none|<compound|<unquote|<merge|counter-in-|<arg|g>>>|<unquote|<arg|x>>>>>>>>>>

  <assign|group-common-counter|<macro|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|display-in-|<arg|g>>>|<macro|x|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|g>>>|<arg|nr>>>>><assign|<unquote|<merge|counter-in-|<arg|g>>>|<macro|x|<style-with|src-compact|none|<compound|<unquote|<merge|counter-|<arg|g>>>>>>><assign|<unquote|<merge|reset-|<arg|g>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|g>>>>|0>>>>>>>>

  <assign|group-individual-counters|<macro|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|display-in-|<arg|g>>>|<macro|x|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|g>>>|<style-with|src-compact|none|<compound|<merge|ind-display-|<arg|x>>|<arg|nr>>>>>>><assign|<unquote|<merge|counter-in-|<arg|g>>>|<macro|x|<style-with|src-compact|none|<compound|<merge|ind-counter-|<arg|x>>>>>><assign|<unquote|<merge|reset-|<arg|g>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|0><for-each|reset-counter|<value|<unquote|<merge|<arg|g>|-group>>>>>>>>>>>

  <\active*>
    <\src-comment>
      A counter <verbatim|x> can be subordonated to another counter
      <verbatim|y>, so that <verbatim|x> will be reset whenever <verbatim|y>
      is increased. Similarly, the content of the counter <verbatim|y> may be
      used as a prefix for <verbatim|x>.
    </src-comment>
  </active*>

  <assign|subordonate-counter|<macro|x|y|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|inc-|<arg|y>>>|<merge|<macro|<compound|<unquote|<merge|reset-|<arg|x>>>>>|<value|<merge|inc-|<arg|y>>>>>>>>>

  <assign|prefix-counter|<macro|x|y|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|display-|<arg|x>>>|<macro|nr|<style-with|src-compact|none|<compound|<unquote|<merge|the-|<arg|y>>>>.<compound|<unquote|<value|<merge|display-|<arg|x>>>>|<arg|nr>>>>>>>>>
</body>

<\initial>
  <\collection>
    <associate|language|english>
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
  </collection>
</initial>