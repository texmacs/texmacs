<TeXmacs|1.0.3.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|structured-list|1.0>

    <\src-purpose>
      Allow items in lists to take an optional body as argument.
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
      Allow item tags to take an additional argument.

      <with|color|red|FIXME:> for the definition of <verbatim|binary-item*>
      we use a dirty hack for testing whether <verbatim|current-item> takes
      one or two arguments. This is OK as long as all long item rendering
      tags are explicitly defined using macros with the item text and body.
    </src-comment>
  </active*>

  <assign|unary-item*|<macro|x|<assign|last-item|<arg|x>><current-item|<arg|x>>>>

  <assign|binary-short-item*|<macro|x|y|<surround|<unary-item*|<arg|x>>|<right-flush>|<arg|y>>>>

  <assign|binary-long-item*|<\macro|x|y>
    <surround|<assign|last-item|<arg|x>>||<current-item|<arg|x>|<arg|y>>>
  </macro>>

  <assign|binary-item*|<macro|x|y|<style-with|src-compact|none|<compound|<style-with|src-compact|none|<if|<equal|<get-arity|<value|current-item>>|2>|binary-short-item*|binary-long-item*>>|<arg|x>|<arg|y>>>>>

  <assign|item*|<macro|x|y|<style-with|src-compact|none|<compound|<if|<equal|<quote-arg|y>|<uninit>>|unary-item*|binary-item*>|<arg|x>|<arg|y>>>>>

  <assign|item|<macro|x|<surround|<next-item>||<style-with|src-compact|none|<compound|<if|<equal|<quote-arg|x>|<uninit>>|unary-item*|binary-item*>|<the-item>|<arg|x>>>>>>

  <\active*>
    <\src-comment>
      Redefine long item rendering tags.
    </src-comment>
  </active*>

  <assign|binary-item-long|<\macro|name|body>
    <arg|name>

    <surround||<right-flush>|<arg|body>>
  </macro>>

  <assign|long-compact-strong-dot-item|<\macro|x|y>
    <binary-item-long|<compact-strong-dot-item|<arg|x>>|<arg|y>>
  </macro>>

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