<TeXmacs|1.99.9>

<style|<tuple|source|std|english>>

<\body>
  <active*|<\src-title>
    <src-package|std-counter|1.0>

    <\src-purpose>
      This package manages counters for sections, theorems, etc.
    </src-purpose>

    <src-copyright|2004|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      The <verbatim|new-counter> primitive is used for the definition of a
      new counter. Each new counter <verbatim|x> gives rise to several new
      environment variables:

      \ \ <verbatim|x-nr>: the counter itself (an integer).

      \ \ <verbatim|display-x>: a macro which will be applied to the counter
      for displaying it.

      \ \ <verbatim|counter-x>: a macro which returns the name of the counter
      (<verbatim|x-nr> by default).

      \ \ <verbatim|the-x>: a macro which returns the counter for display.

      \ \ <verbatim|reset-x>: a macro which resets the counter.

      \ \ <verbatim|inc-x>: a macro which increases the counter.

      \ \ <verbatim|next-x>: a macro which increases the counter and sets the
      current label to the counter.

      The different macros may be changed in order to customize the behaviour
      of the counter; this feature is used in particular by the grouping
      primitives defined further below.
    </src-comment>
  </active*>

  <assign|new-counter|<macro|x|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|x>|-nr>>|0><assign|<unquote|<merge|display-|<arg|x>>>|<value|identity>><assign|<unquote|<merge|counter-|<arg|x>>>|<macro|<style-with|src-compact|none|<unquote|<merge|<arg|x>|-nr>>>>><assign|<unquote|<merge|the-|<arg|x>>>|<macro|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|x>>>|<value|<compound|<unquote|<merge|counter-|<arg|x>>>>>>>>><assign|<unquote|<merge|reset-|<arg|x>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|0>>>><assign|<unquote|<merge|inc-|<arg|x>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|<plus|<value|<compound|<unquote|<merge|counter-|<arg|x>>>>>|1>>>>><assign|<unquote|<merge|next-|<arg|x>>>|<macro|<style-with|src-compact|none|<compound|<unquote|<merge|inc-|<arg|x>>>><set-binding|<compound|<unquote|<merge|the-|<arg|x>>>>>>>>>>>>

  <assign|value-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|counter-|<arg|x>>>>>>

  <assign|reset-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|reset-|<arg|x>>>>>>

  <assign|inc-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|inc-|<arg|x>>>>>>

  <assign|next-counter|<macro|x|<style-with|src-compact|none|<compound|<merge|next-|<arg|x>>>>>>

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

  <assign|new-counter-group|<macro|g|<style-with|src-compact|none|<assign|<merge|<arg|g>|-group>|<tuple>><new-counter|<arg|g>><group-individual-counters|<arg|g>>>>>

  <assign|add-to-counter-group|<macro|x|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|<arg|g>|-group>>|<merge|<value|<merge|<arg|g>|-group>>|<tuple|<arg|x>>>><new-counter|<arg|x>><assign|<unquote|<merge|ind-display-|<arg|x>>>|<value|<unquote|<merge|display-|<arg|x>>>>><assign|<unquote|<merge|ind-counter-|<arg|x>>>|<value|<unquote|<merge|counter-|<arg|x>>>>><assign|<unquote|<merge|display-|<arg|x>>>|<macro|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-in-|<arg|g>>>|<unquote|<arg|x>>|<arg|nr>>>>><assign|<unquote|<merge|counter-|<arg|x>>>|<macro|<style-with|src-compact|none|<compound|<unquote|<merge|counter-in-|<arg|g>>>|<unquote|<arg|x>>>>>>>>>>

  <assign|group-common-counter|<macro|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|display-in-|<arg|g>>>|<macro|x|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|g>>>|<arg|nr>>>>><assign|<unquote|<merge|counter-in-|<arg|g>>>|<macro|x|<style-with|src-compact|none|<compound|<unquote|<merge|counter-|<arg|g>>>>>>><assign|<unquote|<merge|reset-|<arg|g>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|g>>>>|0>>>>>>>>

  <assign|group-individual-counters|<macro|g|<quasi|<style-with|src-compact|none|<assign|<unquote|<merge|display-in-|<arg|g>>>|<macro|x|nr|<style-with|src-compact|none|<compound|<unquote|<merge|display-|<arg|g>>>|<style-with|src-compact|none|<compound|<merge|ind-display-|<arg|x>>|<arg|nr>>>>>>><assign|<unquote|<merge|counter-in-|<arg|g>>>|<macro|x|<style-with|src-compact|none|<compound|<merge|ind-counter-|<arg|x>>>>>><assign|<unquote|<merge|reset-|<arg|g>>>|<macro|<style-with|src-compact|none|<assign|<compound|<unquote|<merge|counter-|<arg|x>>>>|0><for-each|reset-counter|<value|<unquote|<merge|<arg|g>|-group>>>>>>>>>>>

  <assign|individual-counter|<macro|x|<style-with|src-compact|none|<quasi|<assign|<unquote|<merge|display-|<arg|x>>>|<value|<unquote|<merge|ind-display-|<arg|x>>>>>><quasi|<assign|<unquote|<merge|counter-|<arg|x>>>|<value|<unquote|<merge|ind-counter-|<arg|x>>>>>>>>>

  \;
</body>

<initial|<\collection>
</collection>>