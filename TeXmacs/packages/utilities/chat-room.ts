<TeXmacs|1.99.16>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|chat|1.0>

    <\src-purpose>
      Chat rooms
    </src-purpose>

    <src-copyright|2020|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(client client-chat)>

  <\active*>
    <\src-comment>
      Rendering macros
    </src-comment>
  </active*>

  <assign|chat-render|<\macro|bar-color|title|body>
    <with|shadow-elevation|0.75|<\quarter-bend-in-shadow>
      <\wide-tabular>
        <tformat|<cwith|1|1|1|1|cell-background|<arg|bar-color>>|<cwith|1|-1|1|1|cell-lsep|1spc>|<cwith|1|-1|1|1|cell-rsep|1spc>|<cwith|1|-1|1|1|cell-tsep|0.5spc>|<cwith|1|-1|1|1|cell-bsep|0.5spc>|<cwith|2|-1|1|1|cell-tsep|2spc>|<cwith|2|-1|1|1|cell-bsep|1spc>|<table|<row|<\cell>
          <samp|<with|color|white|<arg|title>>>
        </cell>>|<row|<\cell>
          <arg|body>
        </cell>>>>
      </wide-tabular>
    </quarter-bend-in-shadow>>
  </macro>>

  <assign|chat-send|<macro|<with|ornament-shape|rounded|ornament-color|dark
  blue|ornament-border|0ln|ornament-corner|50%|<ornament|<with|color|white|<samp|Send>>>>>>

  <\active*>
    <\src-comment>
      Main macros
    </src-comment>
  </active*>

  <assign|chat-output|<\macro|sender|pseudo|picture|date|body>
    <\chat-render|dark grey|<arg|sender><htab|5mm><arg|date>>
      <arg|body>
    </chat-render>
  </macro>>

  <assign|chat-input|<\macro|body>
    <\chat-render|dark blue|New message>
      <surround||<right-flush><chat-send>|<arg|body>>
    </chat-render>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>