<TeXmacs|1.0.6.8>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|gui-utils|1.0>

    <\src-purpose>
      Standard GUI utilities
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

  <use-package|gui-utils>

  <drd-props|form-window|arity|<tuple|repeat|1|1>|border|no>

  <\active*>
    <\src-comment>
      Submacros for form identifiers
    </src-comment>
  </active*>

  <assign|form-prefix|>

  <assign|form|<macro|name|body|<style-with|src-compact|none|<with|form-prefix|<merge|<value|form-prefix>|<arg|name>|->|<wide-normal|<arg|body>>>>>>

  <assign|form-id|<macro|name|<id|<merge|<value|form-prefix>|<arg|name>>>>>

  <assign|form-value|<macro|name|<extern|form-ref|<arg|name>|<value|form-prefix>>>>

  <assign|form-cmd|<macro|cmd|<merge|(form-delay (form-with
  "|<value|form-prefix>|" |<arg|cmd>|))>>>

  <\active*>
    <\src-comment>
      Toggles
    </src-comment>
  </active*>

  <assign|form-toggle-cmd|<macro|name|val|<form-cmd|<merge|(form-set!
  "|<arg|name>|" "|<not|<arg|val>>|")>>>>

  <assign|form-toggle|<macro|name|val|<style-with|src-compact|none|<action|<gui-toggle|<if|<equal|<arg|name>|>|<arg|val>|<arg|val>>>|<form-toggle-cmd|<arg|name>|<arg|val>>|<arg|val>><hidden|<locus|<form-id|<arg|name>>|<arg|val>>>>>>

  <assign|form-button-toggle|<macro|name|val|text|<style-with|src-compact|none|<action|<gui-button-toggle|<arg|val>|<arg|text>>|<form-toggle-cmd|<arg|name>|<arg|val>>|<arg|val>><hidden|<locus|<form-id|<arg|name>>|<arg|val>>>>>>

  <\active*>
    <\src-comment>
      Alternatives
    </src-comment>
  </active*>

  <assign|form-alternatives|<macro|name|val|body|<surround|<hidden|<locus|<form-id|<arg|name>>|<arg|val>>>||<with|<merge|form-value-|<arg|name>>|<arg|val>|<arg|body>>>>>

  <assign|form-alternative|<macro|name|val|<style-with|src-compact|none|<action|<gui-toggle|<equal|<arg|val>|<form-value|<arg|name>>>>|<form-cmd|<merge|(form-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

  <assign|form-button-alternative|<macro|name|val|text|<style-with|src-compact|none|<action|<gui-button-toggle|<equal|<arg|val>|<form-value|<arg|name>>>|<arg|text>>|<form-cmd|<merge|(form-set!
  "|<arg|name>|" "|<arg|val>|")>>|<arg|val>>>>>

  <assign|form-hide|<macro|body|<hidden|<arg|body>>>>

  <assign|form-conditional|<macro|cond|body|<compound|<if|<arg|cond>|identity|form-hide>|<arg|body>>>>

  <assign|form-sheet|<\macro|name|val|body>
    <\form-conditional|<equal|<arg|val>|<form-value|<arg|name>>>>
      <arg|body>
    </form-conditional>
  </macro>>

  <\active*>
    <\src-comment>
      Buttons
    </src-comment>
  </active*>

  <assign|form-action|<macro|body|cmd|<group|<action|<arg|body>|<form-cmd|<arg|cmd>>|<arg|body>>>>>

  <assign|form-button|<macro|body|cmd|<gui-small-raise|<action|<arg|body>|<form-cmd|<arg|cmd>>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      Input fields
    </src-comment>
  </active*>

  <assign|form-small-input|<macro|name|val|<gui-small-input|<locus|<form-id|<arg|name>>|<arg|val>>>>>

  <assign|form-line-input|<macro|name|val|<gui-line-input|<locus|<form-id|<arg|name>>|<arg|val>>>>>

  <assign|form-big-input|<\macro|name|val>
    <\gui-big-input>
      <\locus|<form-id|<arg|name>>>
        <arg|val>
      </locus>
    </gui-big-input>
  </macro>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>

<\references>
  <\collection>
    <associate||<tuple|<error|argument body>|?>>
  </collection>
</references>