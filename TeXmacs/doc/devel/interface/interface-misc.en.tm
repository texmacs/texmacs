<TeXmacs|1.0.6.10>

<style|tmdoc>

<\body>
  <tmdoc-title|Miscellaneous features>

  Several other features are supported in order to write interfaces between
  <TeXmacs> and extern applications. Some of these are very hairy or quite
  specific. Let us briefly describe a few miscellaneous features:

  <paragraph*|Interrupts>

  The ``stop'' icon can be used in order to interrupt the evaluation of some
  input. When pressing this button, <TeXmacs> will just send a
  <verbatim|SIGINT> signal to your application. It expects your application
  to finish the output as usual. In particular, you should close all open
  <render-key|DATA_BEGIN>-blocks.

  <paragraph*|Testing whether the input is complete>

  Some systems start a multiline input mode as soon as you start to define a
  function or when you enter an opening bracket without a matching closing
  bracket. <TeXmacs> allows your application to implement a special predicate
  for testing whether the input is complete. First of all, this requires you
  to specify the configuration option

  <\scheme-fragment>
    (:test-input-done #t)
  </scheme-fragment>

  As soon as you will press <shortcut|(kbd-return)> in your input, <TeXmacs> will
  then send the command

  <\quotation>
    <\framed-fragment>
      <\verbatim>
        <render-key|DATA_COMMAND>(input-done? <em|input-string>)<shortcut|(kbd-return)>
      </verbatim>
    </framed-fragment>
  </quotation>

  Your application should reply with a message of the form

  <\quotation>
    <\framed-fragment>
      <verbatim|<render-key|DATA_BEGIN>scheme:<em|done><render-key|DATA_END>>
    </framed-fragment>
  </quotation>

  where <verbatim|<em|done>> is either <scm|#t> or <scm|#f>. The
  <verbatim|multiline> plug-in provides an example of this mechanism (see in
  particular the file <example-plugin-link|multiline/src/multiline.cpp>).

  <tmdoc-copyright|1998--2002|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|language|english>
  </collection>
</initial>