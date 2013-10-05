<TeXmacs|1.0.7.20>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|chat|1.0>

    <\src-purpose>
      An style package for internet chatting
    </src-purpose>

    <src-copyright|2006|Joris van der Hoeven>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      Style parameters.
    </src-comment>
  </active*>

  <assign|chat-input-color|dark green>

  <assign|chat-output-color|blue>

  <\active*>
    <\src-comment>
      Chat markup elements
    </src-comment>
  </active*>

  <assign|chat-session|<\macro|room|body>
    <arg|body>
  </macro>>

  <assign|chat-input|<macro|user|body|<\surround|<with|color|<value|chat-input-color>|<merge|<arg|user>|:>
  >|<right-flush>>
    <arg|body>
  </surround>>>

  <assign|chat-output|<macro|user|body|<surround|<with|color|<value|chat-output-color>|<merge|<arg|user>|:>
  >|<right-flush>|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>