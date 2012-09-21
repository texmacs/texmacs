<TeXmacs|1.0.7.15>

<style|tmdoc>

<\body>
  <tmdoc-title|The link system>

  There is currently no comprehensive documentation for the link system. In
  the meantime, we'll collect here documentation for procedures related to
  it.

  <\explain>
    <scm|(go-to-url <scm-arg|u> . <scm-arg|opt-from>)><explain-synopsis|Jump
    to the url @u>
  <|explain>
    Opens a new buffer with the contents of the resource at <scm-arg|u>. This
    can be either a full <abbr|URL> or a file path, absolute or relative to
    the current <scm|buffer-master>. Both types of argument accept
    parameters. The second, optional argument, is an optional path for the
    cursor history.

    You can pass paramenters in <scm-arg|u> in two ways: appending a hash
    <tt|#> and some text, like in <tt|some/path/some-file.tm#blah> will open
    the file and jump to the first label of name <tt|blah> found, if any. The
    other possibility is the usual way in the web: append a question mark
    <tt|?> followed by pairs <tt|parameter=value>. Currently the parameters
    <tt|line> and <tt|column> are supported by default for any file of format
    <scm|generic-file> (see <scm|define-format>).
  </explain>

  \;

  <tmdoc-copyright|2012|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>