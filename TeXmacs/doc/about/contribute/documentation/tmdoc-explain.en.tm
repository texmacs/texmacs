<TeXmacs|1.0.7.14>

<style|tmdoc>

<\body>
  <tmdoc-title|Explanations of macros, environment variables, and so on>

  The main environment which is used for explanations of macros, environment
  variables, <scheme> functions, <abbr|etc.> is inserted using the
  <subsubmenu|Manual|Explain|Explanatory item> entry of the
  <menu|Manual|Explain> and <icon|tm_explain.xpm> menus. The environment
  comes with two arguments: the first argument consists of the concept or
  concepts to be explained, and the second one contains the actual
  explanation. A typical example would be the following:

  <\explain>
    <explain-macro|demo-tag|body>

    <explain-macro|demo-tag|extras|body><explain-synopsis|short and long
    versions of a demo tag>
  <|explain>
    The <markup|demo-tag> is used for demonstration purposes and decorates
    the <src-arg|body><compound|src-var|> argument. An optional argument
    <src-arg|extras> can be given with details on the way to decorate the
    <src-arg|body>.
  </explain>

  In this example, we used <menu|Manual|Explain|TeXmacs macros> twice in
  order to insert the macros to be described. We also used
  <menu|Manual|Explain|Synopsis> in order to give a short description of the
  tags (in grey). In a similar way, one may use
  <menu|Manual|Explain|Environment variable> in order to describe an
  environment variable. Another example is:

  <\explain>
    <scm|(foo-bar <scm-arg|K> <scm-arg|x>)>
  <|explain>
    The function <scm|foo-bar> computes the foo-bar transform of the operator
    <scm|<scm-arg|K>> and applies it to <scm|<scm-arg|x>>.
  </explain>

  In this example, we notice that all <scheme> code was encapsulated into
  <markup|scm> tags (see <menu|Insert|Program|Inline code|Scheme>) and
  arguments were tagged using <markup|scm-arg>.

  <tmdoc-copyright|1998--2011|Joris van der Hoeven>

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