<TeXmacs|1.99.13>

<style|<tuple|generic|british|old-dots>>

<\body>
  <doc-data|<doc-title|The Java Language>>

  <section|Number>

  <subsection|Integer Literals>

  <\java-code>
    // decimal-literal suffix(optional)

    10, 10l, 10L

    // octal-literal

    010

    // hex-literal

    0x10, 0X10

    // binary-literal

    0b10, 0B10

    // No octal-literal

    0o10, 0O10 // invalid
  </java-code>

  <section|String>

  <\java-code>
    ('\\b', '\\t', '\\n', '\\f', '\\r', '\\"', '\\'', '\\\\')

    '\\u0041'

    '\\041'
  </java-code>

  <section|<hlink|Identifier|https://docs.oracle.com/javase/specs/jls/se13/html/jls-3.html#jls-3.8>>

  <\java-code>
    String hello;

    Integer hello1;

    Long hello2;

    int 123x; // invalid
  </java-code>

  <section|References>

  <\itemize>
    <item><hlink|The Java Language Specification(Java SE 13
    Edition)|https://docs.oracle.com/javase/specs/jls/se13/html/index.html>
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|1.1|?>>
    <associate|auto-3|<tuple|2|?>>
    <associate|auto-4|<tuple|3|?>>
    <associate|auto-5|<tuple|4|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>Number>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <with|par-left|<quote|1tab>|1.1<space|2spc>Integer Literals
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>String>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc><locus|<id|%379C16EA8-378B5FD40>|<link|hyperlink|<id|%379C16EA8-378B5FD40>|<url|https://docs.oracle.com/javase/specs/jls/se13/html/jls-3.html#jls-3.8>>|Identifier>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|4<space|2spc>References>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>