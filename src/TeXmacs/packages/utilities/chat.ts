<TeXmacs|1.0.6.1>

<style|source>

<\body>
  <assign|chat-session|<\macro|room|body>
    <arg|body>
  </macro>>

  <assign|chat-input|<macro|user|body|<\surround|<with|color|dark
  green|<merge|<arg|user>|:> >|<right-flush>>
    <arg|body>
  </surround>>>

  <assign|chat-output|<macro|user|body|<surround|<with|color|blue|<merge|<arg|user>|:>
  >|<right-flush>|<arg|body>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|preamble|true>
  </collection>
</initial>