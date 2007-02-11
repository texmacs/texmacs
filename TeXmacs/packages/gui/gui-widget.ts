<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-widget|1.0>

    <\src-purpose>
      Global appearance and low-level markup for widgets
    </src-purpose>

    <src-copyright|2007|Joris van der Hoeven>

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
      Global layout for widgets
    </src-comment>
  </active*>

  <assign|bg-color|light grey>

  <assign|page-medium|automatic>

  <assign|scroll-bars|false>

  <assign|font-family|ss>

  <assign|par-sep|0.2em>

  <assign|par-par-sep|0em>

  <assign|par-line-sep|0em>

  <assign|page-screen-left|0.5em>

  <assign|page-screen-right|0.5em>

  <assign|page-screen-top|0.5em>

  <assign|page-screen-bot|0.5em>

  <\active*>
    <\src-comment>
      Low-level macros for widgets
    </src-comment>
  </active*>

  <drd-props|widget-window|arity|<tuple|repeat|1|1>|border|no>

  <assign|widget-prefix|>

  <assign|widget|<macro|name|body|<style-with|src-compact|none|<with|widget-prefix|<merge|<value|widget-prefix>|<arg|name>|->|<wide-normal|<arg|body>>>>>>

  <assign|widget-id|<macro|name|<id|<merge|<value|widget-prefix>|<arg|name>>>>>

  <assign|widget-value|<macro|name|<extern|widget-ref|<arg|name>|<value|widget-prefix>>>>

  <assign|widget-cmd|<macro|cmd|<merge|(widget-delay (widget-with
  "|<value|widget-prefix>|" |<arg|cmd>|))>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
    <associate|sfactor|5>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>