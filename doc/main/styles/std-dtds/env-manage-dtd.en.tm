<TeXmacs|1.0.0.17>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Defining new environments>

  The <tmdtd|env-manage> contains high-level markup which can be used by the
  user to define new environments for theorems, remarks, exercises and
  figures:

  <\description>
    <expand|item*|<markup|newtheorem>>Defines a theorem-like environment. You
    should specify a name for the environment (like ``experiment'') and the
    corresponding text (like ``Experiment'').

    <expand|item*|<markup|newremark>>Similar as <markup|newtheorem>, but for
    remarks.

    <expand|item*|<markup|newexercise>>Similar as <markup|newtheorem>, but
    for exercises.

    <expand|item*|<markup|newfigure>>Similar as <markup|newtheorem>, but for
    figures (in big and small pairs).
  </description>

  The <abbr|d.t.d.> also contains low-level markup for the actual definitions
  of the environments. In fact, the definition of new theorems is done in two
  stages. At the first stage, the <markup|newtheorem> tag is used in order to
  specify which theorem-like environments should be defined. At the second
  stage (just before the user's document is processed) the theorem-like
  environments are actually defined. This mechanism makes it possible to
  customize the environments in packages which are processed between the two
  stages. For instance, the numbering of theorems is customized in this way.

  <\warning>
    At the moment, you should only use the <markup|newtheorem> and similar
    tags inside a personal style file or package. If you use
    <markup|newtheorem> directly inside a document, then the numbering can be
    incorrect, due to the two-stage scheme explained above. This
    inconvenience will disappear as soon as it will be possible to specify
    clean preambles for <TeXmacs> documents.
  </warning>

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
