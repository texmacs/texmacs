<TeXmacs|1.99.16>

<style|<tuple|source|std>>

<\body>
  <active*|<\src-title>
    <src-package|shortcut-editor|1.0>

    <\src-purpose>
      Internal style package for editing keyboard shortcuts.
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(doc tmdoc-markup)>

  <use-module|(source shortcut-edit)>

  <\active*>
    <\src-comment>
      Special tag for keyboard shortcut editing.
    </src-comment>
  </active*>

  <assign|render-key|<macro|key|<active*|<move|<small|<with|font-family|tt|<with|ornament-color|#e0e0e0|ornament-sunny-color|#f0f0f0|ornament-shadow-color|#c0c0c0|ornament-hpadding|2ln|ornament-vpadding|2ln|ornament-border|2ln|<ornament|<compound|inflate|<arg|key>>>>>>||0.075ex>>>>

  <assign|render-keys|<macro|keys|<extern|tmdoc-render-keys|<quote-arg|keys>>>>

  <assign|preview-shortcut|<macro|shortcut|<if|<equal|<arg|shortcut>|>|<arg|shortcut>|<extern|tmdoc-key|<quote-arg|shortcut>>>>>

  <drd-props|preview-shortcut|arity|1>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>