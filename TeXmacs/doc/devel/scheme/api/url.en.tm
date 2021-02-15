<TeXmacs|1.99.18>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|The URL system>

  There is currently no comprehensive documentation for the url system. In
  the meantime, we'll collect here documentation for procedures related to
  it.

  <subsection|Navigation>

  <\explain>
    <scm|(go-to-url <scm-arg|u> . <scm-arg|opt-from>)><explain-synopsis|Jump
    to the url @u>
  <|explain>
    Opens a new buffer with the contents of the resource at <scm-arg|u>. This
    can be either a full <abbr|URL> or a file path, absolute or relative to
    the current <scm|buffer-master>. Both types of argument accept
    parameters. The second, optional argument, is an optional path for the
    cursor history.

    You can pass parameters in <scm-arg|u> in two ways: appending a hash
    <tt|#> and some text, like in <verbatim|some/path/some-file.tm#blah> will
    open the file and jump to the first label of name <tt|blah> found, if
    any. The other possibility is the usual way in the web: append a question
    mark <tt|?> followed by pairs <tt|parameter=value>. Currently the
    parameters <tt|line>, <tt|column> and <tt|select>, which respectively
    jump to the chosen location and select the given text at that line, are
    supported by default for any file of format <scm|generic-file>. (see
    <scm|define-format>).
  </explain>

  <subsection|Predicates>

  <\explain>
    <scm|(url-concat? <scm-arg|u>)><explain-synopsis|No synopsis available>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(url-or? <scm-arg|u>)><explain-synopsis|No synopsis available>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(url-rooted? <scm-arg|u>)><explain-synopsis|Test whether @u is
    absolute>
  <|explain>
    Return <scm|#t> if the url is absolute. Absolute urls may be for instance
    full paths in the file system or internet <abbr|URL>s starting with a
    protocol specification like <verbatim|ftp> or <verbatim|http>. The
    <verbatim|tmfs> urls are also understood to be rooted. See also
    <scm|url-rooted-tmfs?>, <scm|url-rooted-web?> and
    <scm|url-rooted-protocol?>.
  </explain>

  <\explain>
    <scm|(url-descends? <scm-arg|u1> <scm-arg|u2>)><explain-synopsis|Test
    whether @u1 is a parent for @u2 ?>
  <|explain>
    \;
  </explain>

  <\explain>
    <scm|(url-regular? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to regular file>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a regular
    file, <scm|#f> otherwise. See also <scm|url-directory?> and
    <scm|url-link?>.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-regular? "$TEXMACS_PATH/LICENSE")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-directory? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to a directory>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a
    directory and it exists, <scm|#f> otherwise.

    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-directory? "/tmp")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-directory? "/tmp_not_exist")
      <|unfolded-io>
        #f
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-directory? "$TEXMACS_PATH/LICENSE")
      <|unfolded-io>
        #f
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-link? <scm-arg|u>)><explain-synopsis|Test whether the url
    refers to a symbolic link>
  <|explain>
    Applies only to filesystem urls. Returns <scm|#t> if the url is a
    symbolic link, <scm|#f> otherwise.
  </explain>

  <subsection|Accessors>

  <\explain>
    <scm|(url-basename <scm-arg|u>)><explain-synopsis|Return the basename as
    string for @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-basename "/tmp")
      <|unfolded-io>
        "tmp"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-basename "/tmp/hello.tm")
      <|unfolded-io>
        "hello"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-basename "/tmp/a.out")
      <|unfolded-io>
        "a"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-basename "/tmp/install-sh")
      <|unfolded-io>
        "install-sh"
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-head <scm-arg|u>)><explain-synopsis|Return the head part as url
    for @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-head "/tmp")
      <|unfolded-io>
        \<less\>url /\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-head "/tmp/a.out")
      <|unfolded-io>
        \<less\>url /tmp\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-head "/tmp/texmacs/TeXmacs/LICENSE")
      <|unfolded-io>
        \<less\>url /tmp/texmacs/TeXmacs\<gtr\>
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <\explain>
    <scm|(url-tail <scm-arg|u>)><explain-synopsis|Return the tail part as url
    for @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-tail "/tmp")
      <|unfolded-io>
        \<less\>url tmp\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-tail "/tmp/hello.tm")
      <|unfolded-io>
        \<less\>url hello.tm\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-tail "/tmp/a.out")
      <|unfolded-io>
        \<less\>url a.out\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-tail "/tmp/texmacs/TeXmacs/LICENSE")
      <|unfolded-io>
        \<less\>url LICENSE\<gtr\>
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
  </explain>

  <tmdoc-copyright|2013\U2021|the <TeXmacs> team.>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify
  this\ndocument under the terms of the GNU Free Documentation License,
  Version 1.1 or\nany later version published by the Free Software
  Foundation; with no Invariant\nSections, with no Front-Cover Texts, and
  with no Back-Cover Texts. A copy of\nthe license is included in the section
  entitled "GNU Free Documentation License".>
</body>

<initial|<\collection>
</collection>>