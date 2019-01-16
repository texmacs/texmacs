<TeXmacs|1.99.8>

<style|<tuple|tmdoc|old-spacing>>

<\body>
  <tmdoc-title|A <verbatim|tmfs> primer>

  <subsection|The <TeXmacs> filesystem>

  Many things in <TeXmacs> can be referenced through a <abbr|URI> with
  <verbatim|tmfs> as schema. Examples of entities in this system are buffers,
  views and windows or at a higher level help buffers and search results. A
  <TeXmacs> <abbr|URI> follows the format:

  <center|<verbatim|tmfs://handler[/query]>>

  Requests to open <abbr|URI>s such as these are sent to a <em|handler>,
  which actually is a set of procedures implementing the basic operations
  related to the type of content they handle: loading the content, saving it
  (if possible or necessary), setting the window title and establishing
  access permissions are the basic operations. Predefined handlers which the
  user usually encounters are <verbatim|grep>, <verbatim|help>,
  <verbatim|history>, <verbatim|revision> and <verbatim|apidoc>: they accept
  a query representing search strings, files or help pages and render results
  in the appropriate language into a new buffer. The <em|query> is a string
  in the usual format <tt|variable1=value1&variable2=value2>. Its parsing can
  be done using <scm|query-ref>.

  Situations where using this system makes more sense than regular documents
  are for instance documentation, which must be chosen from several languages
  and possibly be compiled on the fly from various sources (see module
  <verbatim|<hlink|doc.apidoc|tmfs://apidoc/type=module&what=doc.apidoc>> and
  related modules) and automatically generated content, like that resulting
  from interacting from an external system for version control of documents
  (see handler <tt|version> in module <verbatim|<hlink|version.version-tmfs|tmfs://apidoc/type=module&what=version.version-tmfs>>).

  <subsection|Implementing a handler>

  The definition of a handler is done via <scm|tmfs-handler> or with the
  convenience macros <scm|tmfs-load-handler>, <scm|tmfs-save-handler>,
  <scm|tmfs-permission-handler> and <scm|tmfs-title-handler>.

  Below we'll implement a basic load handler named <tt|simple> which will
  accept two sorts of arguments: <scm|type> and <scm|what>. We shall use two
  procedures, one to handle the requests, another to create the document.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tm-define (simple-load header body)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document (section ,header) ,body))))
    <|folded-io>
      \;
    </folded-io>
  </session>

  As you can see, we don't do much other than creating a <TeXmacs> document.
  The load handler won't be complicated either. We only parse the query
  string with the help of <scm|query-ref> and then display one of three
  possible buffers.

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-load-handler (simple qry)

      \ \ (let ((type (query-ref qry "type"))

      \ \ \ \ \ \ \ \ (what (query-ref qry "what")))

      \ \ \ \ (tm-\<gtr\>stree

      \ \ \ \ \ \ (cond ((== type "very") (simple-load "Very simple" what))

      \ \ \ \ \ \ \ \ \ \ \ \ ((== type "totally") (simple-load "Totally
      simple" what))

      \ \ \ \ \ \ \ \ \ \ \ \ (else (simple-load "Error"

      \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (string-append "Query
      unknown: " what)))))))
    <|folded-io>
      \;
    </folded-io>
  </session>

  We can test this right away with:

  <\session|scheme|default>
    <\input>
      Scheme]\ 
    <|input>
      (load-buffer "tmfs://simple/type=very&what=example")
    </input>
  </session>

  Or embedded in a document using tags like <markup|hlink> and
  <markup|branch>: <hlink|click here to test
  it|tmfs://simple/type=very&what=example>.

  You can set read/write permissions implementing a <em|permission handler>,
  and the window's title using a <em|title handler>:

  <\session|scheme|default>
    <\folded-io|Scheme] >
      (tmfs-permission-handler (simple name type)\ 

      \ \ (display* "Name= " name "\\nType= " type "\\n")

      \ \ #t)
    <|folded-io>
      \;
    </folded-io>

    <\folded-io|Scheme] >
      (tmfs-title-handler (simple qry doc) "Simple handler - Some title
      here")
    <|folded-io>
      \;
    </folded-io>
  </session>

  <\explain>
    <scm|(tmfs-load-handler (<scm-arg|name> <scm-arg|qry>)
    <scm-arg|body>)><explain-synopsis|define load handler for @name>
  <|explain>
    A <em|load handler> for <scm-arg|name> is invoked when <TeXmacs> receives
    a request to open a <abbr|URI> of type
    <verbatim|tmfs://<scm-arg|name>/<scm-arg|qry>>. The <scm-arg|body> of the
    handler is passed <verbatim|qry> as parameter (see <scm|query-ref>) and
    must return a complete <TeXmacs> buffer. Consider the following example:

    <\scm-code>
      (tmfs-load-handler (id qry)

      \ \ `(document

      \ \ \ \ \ (TeXmacs ,(texmacs-version))

      \ \ \ \ \ (style (tuple "generic"))

      \ \ \ \ \ (body (document ,qry))))
    </scm-code>

    This will open <abbr|URI>s with the format
    <verbatim|tmfs://id/whatever_arguments>.

    Creation of the buffer contents may be simplified using the procedures
    defined in module <verbatim|<hlink|kernel.gui.gui-markup|tmfs://sapi/type=module&what=kernel.gui.gui-markup>>.
  </explain>

  <\explain>
    <scm|(tmfs-save-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define save handler for <scm-arg|name>>
  </explain|A <em|save handler> is invoked when the user tries to save a
  buffer of type <verbatim|tmfs://<scm-arg|name>/...> See also
  <scm|tmfs-load-handler> and others.>

  <\explain>
    <scm|(tmfs-title-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define title handler <scm-arg|name>>
  </explain|A <em|title handler> is invoked to build the title for a window
  displaying a buffer of type <verbatim|tmfs://<scm-arg|name>/...> It is
  expected to return a simple string in the right language for the user.>

  <\explain>
    <scm|(tmfs-permission-handler (<scm-arg|name> <scm-arg|qry>
    <scm-arg|kind>) <scm-arg|body>)><explain-synopsis|define master handler
    <scm-arg|name>>
  </explain|A <em|permissions handler> decides whether the buffer
  corresponding to the query made to the handler may be loaded/saved, etc.
  <scm-arg|kind> may take one of the values <scm|"load">, (...)>

  <\explain>
    <scm|(tmfs-master-handler (<scm-arg|name> <scm-arg|qry> <scm-arg|doc>)
    <scm-arg|body>)><explain-synopsis|define title handler <scm-arg|name>>
  </explain|A <em|master handler> is... (possibly related to the concept of
  master document in a project, but this needs checking)>

  <\explain>
    <scm|(query-ref <scm-arg|qry> <scm-arg|arg>)><explain-synopsis|return
    value of parameter <scm-arg|arg> in query <scm-arg|qry>>
  </explain|Given a <scm-arg|qry> string of type
  <tt|variable1=value1&variable2=value2>, <scm|query-ref> will return
  <tt|value1> for an <scm-arg|arg> value of <scm|value1>, etc.>

  <subsection|Installing the handler>

  In order to make your handler available from any menu item or document upon
  startup, you must add it to the initialization process, that is to
  <verbatim|init-texmacs.scm> or <verbatim|my-init-texmacs.scm>, using the
  macro <scm|lazy-tmfs-handler>. This will delay loading of your code either
  until it is required or <TeXmacs> is idle waiting for user input.

  <\remark>
    \ The keywords <tt|buffer>, <tt|view> and <tt|window> may not be used as
    names for handlers since they are used internally by <TeXmacs>.
  </remark>

  <\explain>
    <scm|(lazy-tmfs-handler <scm-arg|module>
    <scm-arg|handler>)><explain-synopsis|lazily install a <verbatim|tmfs>
    handler>
  <|explain>
    Inform <TeXmacs> that <scm-arg|handler> is available in module
    <scm-arg|module>. <scm-arg|module> must be a list of symbols (like
    <scm|(kernel gui gui-markup)>) representing the <scheme> module where
    you'll have defined your handler using <scm|tmfs-handler> or with the
    convenience macros <scm|tmfs-load-handler>, <scm|tmfs-save-handler>,
    <scm|tmfs-permission-handler> and <scm|tmfs-title-handler>.
  </explain>

  <tmdoc-copyright|2012|the <TeXmacs> team.>
</body>

<initial|<\collection>
</collection>>