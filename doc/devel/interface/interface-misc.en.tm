<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Miscellaneous features>

  Several other features are supported in order to write interfaces between
  <TeXmacs> and extern applications. Some of these are very hairy or quite
  specific. Let us briefly describe a few miscellaneous features:

  <paragraph|Interrupts>

  The ``stop'' icon can be used in order to interrupt the evaluation of some
  input. When pressing this button, <TeXmacs> will just send a
  <verbatim|SIGINT> signal to your application. It expects your application
  to finish the output as usual. In particular, you should close all open
  <key|DATA_BEGIN>-blocks.

  <paragraph|Testing whether the input is complete>

  Some systems start a multiline input mode as soon as you start to define a
  function or when you enter an opening bracket without a matching closing
  bracket. <TeXmacs> allows your application to implement a special predicate
  for testing whether the input is complete. First of all, this requires you
  to specify the configuration option

  <\expand|scheme-fragment>
    (:test-input-done #t)
  </expand>

  As soon as you will press <key|<expand|key-return>> in your input,
  <TeXmacs> will then send the command

  <\quotation>
    <\expand|framed-fragment>
      <\verbatim>
        <key|DATA_COMMAND>(input-done? <em|input-string>)<key|<expand|key-return>>
      </verbatim>
    </expand>
  </quotation>

  Your application should reply with a message of the form

  <\quotation>
    <\expand|framed-fragment>
      <verbatim|<key|DATA_BEGIN>scheme:<em|done><key|DATA_END>>
    </expand>
  </quotation>

  where <verbatim|<em|done>> is either <verbatim|#t> or <verbatim|#f>. The
  <verbatim|multiline> plugin provides an example of this mechanism (see in
  particular the file <expand|example-plugin-link|multiline/src/multiline.cpp>).

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|toc-2|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <with|left margin|<quote|6fn>|font size|<quote|0.84>|Interrupts<value|toc-dots><pageref|toc-1>>
    </associate>
  </collection>
</auxiliary>
