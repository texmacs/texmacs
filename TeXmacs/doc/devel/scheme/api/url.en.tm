<TeXmacs|2.1>

<style|<tuple|tmdoc|old-spacing|old-dots|old-lengths>>

<\body>
  <tmdoc-title|The URL system>

  <TeXmacs> uses a tree representation for urls. This allows us to generalize
  the concept of an url and allow paths and patterns to be regarded as urls
  too. There are three main types of urls:

  <\itemize-dot>
    <item>rootless urls, like a/b/c. These urls are mainly used in
    computations. For example, they can be appended to another url.

    <item>Standard rooted urls, like file:///usr or https://www.texmacs.org.
    These are the same as those used on the web.

    <item>System urls, characterized by a "default" root. These urls are
    similar to standard rooted urls, but they behave in a slightly different
    way with respect to concatenation. For instance
    https://www.texmacs.org/Web * file:///tmp would yield file:///tmp, where
    as https://www.texmacs.org/Web /tmp yields https://www.texmacs.org/tmp
  </itemize-dot>

  There are several formats for parsing (and printing) urls:

  <\itemize-dot>
    <item>System format: the usual format on your operating system. On unix
    systems "/usr/bin:/usr/local/bin" would be a valid url representing a
    path and on windows systems "c:\\windows;c:\\TeXmacs" would be OK.

    <item>Unix format: this format forces unix-like notation even for other
    systems like Windows. This is convenient for url's in the source code.
    Unix environment variables like ~ and $TEXMACS_PATH can also be part of
    the url.

    <item>Standard format: the format which is used on the web. Notice that
    ftp://www.texmacs.org/pub and ftp://www.texmacs.org/pub/ represent
    different urls. The second one is represented by concating on the right
    with an empty name.
  </itemize-dot>

  When an explicit operation on urls need to be performed, like reading a
  file, the url is first "resolved" into a single url with a unique name
  (modulo symbolic links) for the resource (first match). Next, the url is
  "concretized" as a system-specific file name which is understood by the
  operating system. Note that for remote urls this may involve downloading a
  file. Concretized urls should be used quickly and not memorized, since such
  names may be the names of temporary files, which can be destroyed
  afterwards.

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
    <scm|(url-concat? <scm-arg|u>)><explain-synopsis|Returns #t if @u
    contains multiple subdirs>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-concat? "a/b")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-concat? "file.ext")
      <|unfolded-io>
        #f
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-or? <scm-arg|u>)><explain-synopsis|#t if the url contains an
    alternative>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-or? "a/b:c")
      <|unfolded-io>
        #t
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-expand )
      <|unfolded-io>
        \<less\>url a/b:c\<gtr\>
      </unfolded-io>

      <\input|Scheme] >
        \;
      </input>
    </session>
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

  <subsection|Operations>

  <\explain>
    <scm|(url-head <scm-arg|u>)><explain-synopsis|Return the directory part
    of @u>
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
    </session>
  </explain>

  <\explain>
    <scm|(url-tail <scm-arg|u>)><explain-synopsis|Return the file name
    without path of @u>
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
    </session>
  </explain>

  <\explain>
    <scm|(url-suffix <scm-arg|u>)><explain-synopsis|Returns the suffix
    (extension) of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-suffix "/tmp/hello.tm")
      <|unfolded-io>
        "tm"
      </unfolded-io>
    </session>
  </explain>

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
    </session>
  </explain>

  <\explain>
    <scm|(url-glue <scm-arg|u> <scm-arg|s>)><explain-synopsis|Returns \ @u
    with suffix @s appended>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-glue (url-basename (current-buffer)) ".new")
      <|unfolded-io>
        \<less\>url url.en.new\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-unglue <scm-arg|u> <scm-arg|n>)><explain-synopsis|Removes @n
    characters from the suffix of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-unglue (current-buffer) 3) ;output edited
      <|unfolded-io>
        \<less\>url (...)/src/TeXmacs/doc/devel/scheme/api/url.en\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-relative <scm-arg|base> <scm-arg|u>)><explain-synopsis|Prepends
    the head of \ @base to @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-relative "/a/b/c.tm" "d.tm")
      <|unfolded-io>
        \<less\>url /a/b/d.tm\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-delta <scm-arg|base> <scm-arg|u>)><explain-synopsis|Computes
    the change in url from @base to @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-delta "/a/b/c/file.tm" "/a/b")
      <|unfolded-io>
        \<less\>url ../../b\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-root <scm-arg|u>)><explain-synopsis|Returns the root (protocol)
    of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-root (current-buffer))
      <|unfolded-io>
        "default"
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-root "https://www.texmacs.org")
      <|unfolded-io>
        "https"
      </unfolded-io>
    </session>
  </explain>

  <\explain>
    <scm|(url-unroot <scm-arg|u>)><explain-synopsis|Removes the root
    (protocol) of @u>
  <|explain>
    <\session|scheme|default>
      <\unfolded-io|Scheme] >
        (url-unroot "/a/b/c")
      <|unfolded-io>
        \<less\>url a/b/c\<gtr\>
      </unfolded-io>

      <\unfolded-io|Scheme] >
        (url-unroot "https://www.texmacs.org")
      <|unfolded-io>
        \<less\>url www.texmacs.org\<gtr\>
      </unfolded-io>
    </session>
  </explain>

  <todo|Yet to be documented:>

  <\cpp-code>
    url \ \ \ reroot (url u, string s); \ \ \ \ // reroot using new protocol

    url \ \ \ expand (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ // rewrite a/{b:c}
    -\<gtr\> a/b:a/c

    url \ \ \ sort (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // order items in
    ors

    url \ \ \ factor (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ // inverse of
    expand; also sorts

    bool \ \ descends (url u, url base); \ \ // does u descend from base?

    bool \ \ is_secure (url u); \ \ \ \ \ \ \ \ \ \ \ // is u secure?
  </cpp-code>

  <subsection|Resolution>

  <todo|Yet to be documented:>

  <\cpp-code>
    url \ complete (url u, string filter= "fr"); // wildcard completion

    url \ resolve (url u, string filter= "fr"); \ // find first match only

    url \ resolve_in_path (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ // find file
    in path

    bool exists (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ //
    file exists

    bool exists_in_path (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // file exists
    in path

    bool has_permission (url u, string filter); // check file permissions

    url \ descendance (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // utility
    for style&package menus

    url \ subdirectories (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // similarly
    for patters

    url \ concretize_url (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // variant of
    concretize below

    string concretize (url u); \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ // system
    name for resolved url

    string materialize (url u, string f= "fr"); // resolve +
  </cpp-code>

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