<TeXmacs|1.0.3.7>

<style|tmdoc>

<\body>
  <\make-title>
    <title|Extended linking in <TeXmacs>>

    <author|Joris van der Hoeven>
  </make-title>

  <section|Introduction>

  The development of a robust linking system for <TeXmacs> is important for
  many aspects of the program:

  <\itemize>
    <item>Usual labels, references, hyperlinks, actions.

    <item>Links to automatically generated content (citations, tables of
    contents, indexes, <abbr|etc.>).

    <item>Multi-document editing.

    <item>Observers (used to observe a resource which is subject to change);
    may be used for automatically generated output from computer algebra or
    proof systems.

    <item>Spreadsheeds.

    <item>Document enrichment with automatically generated links (Wiki) or
    third party links.

    <item>Document extraction based on typing information and litterate
    programming.

    <item>Typed links for annotation, documentation traversal information,
    contextual browsing, <abbr|etc.>

    <item>Collaborative editing.
  </itemize>

  Our approach is similar to <name|Xlink> in several respects, but it differs
  at certain points:

  <\itemize>
    <item>Since <TeXmacs> is not attribute-based, <name|Xlink> attributes
    correspond to tags, which can either be used in an attribute-like fashion
    or in the content itself (using the concept of the current englobing
    locus).

    <item>Local and distant resources (<verbatim|xlink:resource> and
    <verbatim|xlink:locator>) correspond to different types of ``loci''. In
    contrast to <name|Xlink>, a locus identifies both a place and the
    corresponding (linking) environment. In particular, linkbases can be
    considered as special cases of loci.

    An alternative implementation would be to consider linkbases as different
    objects and to provide markup for associating a linkbase to a locus, or
    to link a locus to its corresponding linkspace using a link with a
    special role.

    <item><TeXmacs> does not distinguish between extended links, simple links
    and arcs. Links are merely relations between a finite named set of loci,
    according to a given set of roles. In particular, the special arc
    properties like <verbatim|xlink:show> and <verbatim|xlink:actuate> are
    part of the role.

    <item>Roles are not merely containers with a mere pointer to an informal
    description. Instead, a tentative is made to invent markup which
    describes the behaviour of a role. Additional behaviour may be defined in
    plug-ins.
  </itemize>

  <section|Loci>

  A locus is a well-identified portion of content which can participate in a
  link. A locus can either be

  <\enumerate>
    <item>Part of a local document.

    <item>Part of a distant document.

    <item>Part of some automatically generated data.

    <item>A container for linking data.
  </enumerate>

  Several properties are attached to the locus: a unique identifier and one
  or several names, one or several roles and the linking context for the
  locus.

  <\explain>
    <explain-macro|locus|attr_1|<with|mode|math|\<cdots\>>|attr_n|content>

    <explain-macro|resource|attr_1|<with|mode|math|\<cdots\>>|attr_n>

    <explain-macro|data|attr_1|<with|mode|math|\<cdots\>>|attr_n|content>

    <explain-macro|linkspace|attr_1|<with|mode|math|\<cdots\>>|attr_n>
  <|explain>
    Declare a locus of one of the four above types and encapsulate the
    <src-arg|content> (when specified) in the locus. The attributes are
    instructions for attaching properties to the locus.

    These instructions may also be specified <em|inside> the content (for
    instance, a theorem inside a locus may add a role <verbatim|theorem> to
    the locus, as well as data with the theorem number and the page number).

    Notice that loci may be nested. In that case, the inner locus inherits
    the linking properties from the outer locus (unless otherwise specified
    using the <verbatim|autonomous> role (opposite of <verbatim|inherit>))
    and vice versa (unless otherwise specified using the <verbatim|private>
    role (opposite of <verbatim|public>); in the case of data loci,
    <verbatim|private> is default).
  </explain>

  The following instructions specify properties for the locus. In addition,
  one may use the <markup|data> and <markup|linkspace> instructions as well
  as the <markup|link> and <markup|role> instructions from the next sections.

  <\explain>
    <explain-macro|locus-id|id>

    <explain-macro|locus-label|id>
  <|explain>
    Specify a name for the locus. In the case of <markup|locus-id>, the
    identifier <src-arg|id> must be unique in the englobing locus (which
    defaults to the entire document when there is no englobing locus).

    For the moment, we require the specification of unique identifiers for
    efficiency purposes. In the future, unique identifiers should be
    automatically generated, and the <markup|locus-id> primitive will become
    obsolete.

    When used as an attribute, one may use <src-arg|id> instead of
    <explain-macro|locus-id|id>.
  </explain>

  <\explain>
    <explain-macro|locus-role|id>
  <|explain>
    Specify a role for the locus. Roles are used to associate properties to
    loci which may be exploited when browsing or for other purposes. Built-in
    roles are <verbatim|autonomous>, <verbatim|inherit>, <verbatim|public>
    and <verbatim|private>.
  </explain>

  <\explain>
    <explain-macro|locus-href|href>
  <|explain>
    For use in <markup|resource> loci only. The <src-arg|href> specifies a
    way to retrieve the resource. Hyperlinks may either be

    <\itemize>
      <item>Identifiers for other loci.

      <item>Standard hyperlinks of the form <explain-macro|url|url>.

      <item>An expression <explain-macro|follow|href_1|id|role_1|<with|mode|math|\<cdots\>>|role_n>
      intended to follow any link which matches all roles to a locus with
      identifier <src-arg|id>.

      <item>An expression <explain-macro|union|href_1|<with|mode|math|\<cdots\>>|href_n>
      for specifying a union of resources.

      <item>An expression <explain-macro|execute|href|data> for resources
      which are the result of the evaluation of a program, a form or a query
      on <src-arg|data>.
    </itemize>
  </explain>

  <\section>
    Links
  </section>

  A link associates an arbitrary number of loci. It may specify one or
  several roles which specify the behaviour of the link (browsing behaviour,
  editing behaviour, typesetting behaviour, <abbr|etc.>).

  <\explain>
    <explain-macro|link|attr_1|<with|mode|math|\<cdots\>>|attr_n>
  <|explain>
    Declare a link between several loci. The attributes are instructions for
    attaching properties to the link.
  </explain>

  The following instructions may be used to specify properties of the links:

  <\explain>
    <explain-macro|link-label|id>
  <|explain>
    Specify an identifier for the link which may be used to refer to links.
  </explain>

  <\explain>
    <explain-macro|link-part|id|href>
  <|explain>
    Specify a part of the link with symbolic name <src-arg|id> and which
    points to <src-arg|href> (recall that <src-arg|href> may be the
    identifier of an other locus). The parts which are required for a given
    link depend on the roles of the link. Standard links with role
    <verbatim|standard> require <verbatim|src> and <verbatim|dest> parts
    (which correspond to the values of <src-arg|id>). When a part of a link
    which is required for a given role is not present, then its destination
    defaults to the current locus.
  </explain>

  <\explain>
    <explain-macro|link-role|id>
  <|explain>
    Specify a role for the link. Roles are used to associate properties to
    links which may be exploited when browsing or for other purposes.
    Built-in roles are

    <\description>
      <item*|<verbatim|standard>>Standard hyperlinks.

      <item*|<verbatim|arc>>Standard links, but without any indication on how
      to display or traverse the link.

      <item*|<verbatim|full>>Link all parts in the link.

      <item*|<verbatim|import>>Make all valid links for a source locus valid
      in a destination locus.

      <item*|<verbatim|access>>Automatic links from parent loci to child
      loci.
    </description>

    Users may provide roles like <verbatim|><verbatim|bidirectional> for
    bidirectional links or <verbatim|apply>, for applications of a theorem,
    <verbatim|replace> for replacing the source of a link by the contents of
    its destination, <abbr|etc.>
  </explain>

  <section|Roles>

  Users may define new roles for loci and links. These roles may be abstract
  containers with a behaviour which is defined in a plug-in. They may also
  specify special properties for the role or additional constraints on the
  role. Locus roles and link roles which are not speci

  <\explain>
    <explain-macro|role|attr_1|<with|mode|math|\<cdots\>>|attr_n>
  <|explain>
    Declare a role. The attributes are instructions for attaching properties
    to the role.
  </explain>

  The following instructions may be used to specify properties of roles:

  <\explain>
    <explain-macro|role-label|id>
  <|explain>
    Associate one or several names to the role.
  </explain>

  <\explain>
    <explain-macro|role-url|href>
  <|explain>
    Link to a page with an informal description of this role.
  </explain>

  <\explain>
    <explain-macro|role-model|id>
  <|explain>
    [For link roles] The linking structure of the locus with name
    <src-arg|id> is taken as model for the linking structure of this role.
    For simple link structures, the locus would typically be a small
    autonomous locus. For instance, if the locus <verbatim|bidi> defines
    subloci <verbatim|src> and <verbatim|dest> with standard hyperlinks from
    <verbatim|src> to <verbatim|dest> and vice versa, then
    <inactive*|<role|<role-label|bidirectional>|<role-model|bidi>>> would
    define a role for bidirectional links. The subloci <verbatim|src> and
    <verbatim|dest> of <verbatim|bidi> become required parts of bidirectional
    links.
  </explain>

  <\explain>
    <explain-macro|role-require|cond_1|<with|mode|math|\<cdots\>>|cond_n>
  <|explain>
    [For link roles] Specify (disjunctive) conditions for this link to be
    valid. The conditions are checked one by one for the set of candidate
    links. As soon as one condition is met by a non-empty subset of the set
    of candidate links, this non-empty subset is returned.

    For example, assume that we have two documents which are available in
    several languages: <verbatim|welcome.en.tm>, <verbatim|welcome.fr.tm>,
    <verbatim|welcome.es.tm>, <verbatim|details.en.tm> and
    <verbatim|details.fr.tm>. Given a link <verbatim|welcome>
    <with|mode|math|\<rightarrow\>> <verbatim|details>, we have 6 matching
    links. The first condition may check whether the languages of the source
    and destination languages coincide (the languages may either be roles or
    data associated to loci). A second condition may check whether the
    destination language is English. A third condition might always evaluate
    to <verbatim|true>.

    In a similar way, one may define conditions which privilege ``close
    links'' to ``distant links''.

    The markup for specifying conditions still has to be designed. We should
    probably rely on plug-ins. We may also provide markup for simple tests on
    matching roles or loci.
  </explain>

  <\explain>
    <explain-macro|role-show|when>

    <explain-macro|role-actuate|when>
  <|explain>
    Hints for how to render the link and when the link should be traversed
    (see <verbatim|xlink>).
  </explain>

  <section|Implementation>

  All loci currently known to the editor (with the associated links) are
  stored in a (probably hierarchically ordered) ``loci space'', which should
  be considered as a database. Each locus has a unique identifier in this
  database. The database is constructed during the typesetting phase.

  <paragraph|Unique identifiers>Each document (or view) has a unique
  identifier. Two ways are provided for associating unique identifiers to
  loci:

  <\itemize>
    <item>The user specifies a unique id in the englobing locus (which will
    be prefixed by the unique id of the englobing locus so as to yield a
    global unique id).

    <item>The id is automatically determined using a counter relative to the
    englobing locus.
  </itemize>

  The second method may be expensive when the englobing locus is large, since
  it may modify the environment. In the far future, we should associate
  unique ids to nodes of lazy rewriters.

  <paragraph|Updating>Loci which are no longer present may still remain in
  the database and references to newly modified loci may be incorrect. When
  an entire document is being typeset, we have to do several things:

  <\itemize>
    <item>Move all loci for the current document to a shadow space (where
    they are kept during the typesetting) and remove them when typesetting is
    done.

    <item>Collect all couples (reference, value) which are encountered during
    the typesetting in an auxiliary document. At the end, test whether the
    values are still correct. If not, then it may be necessary to retypeset
    the document.
  </itemize>

  <paragraph|The environment>

  <\description>
    <item*|<src-var|locus-current>>The unique identifier of the current locus
    in the database.

    <item*|<src-var|locus-nr>>For the automatic generation of a identifiers
    for subloci.

    <item*|<src-var|locus-contents>>The contents of the locus, except for
    subloci, which are entered directly into the database.
  </description>

  Each of the environment variables is locally redefined for each
  locus/sublocus. Notice that <markup|locus>, <markup|resource>,
  <markup|data> and <markup|linkspace> directly affect the linkspace (and not
  much the environment unless an identifier is automatically generated). All
  other linking primitives are directly entered into
  <src-var|locus-contents>. They are entered into the linkspace together with
  the englobing locus.

  <paragraph|Saving>When saving, all loci relative to the current document
  are collected and saved (without uodating). When loading, they are
  immediately entered in the database. Notice that the global variable
  <src-var|locus-contents> should also be saved.

  <paragraph|Automatic loci>Besides loci which are specified by the user,
  <TeXmacs> may also automatically generate new loci (with or without data).
  For instance, output from computer algebra systems might be stored in
  auxiliary loci before inclusion (via copying or linking) into documents.
  Similarly, a shared context with a proof system might be stored at an
  auxiliary locus. For these purposes, it may be useful to associate
  persistent unique identifiers to processes. This makes it possible to
  recognize the execution of the same program at a different time or
  computer.

  <paragraph|The linkspace and link resolution>In its brute form, the link
  space is a collection of loci with unique identifiers and such that each
  locus defines a set of locus properties including the links.

  In order to quickly resolve links, it is necessary to (incrementally)
  compute a contracted version of the linkspace from which it is clear which
  loci and links are valid at a certain locus. For this, we will use the
  postulate that environment importation is always transitive. More
  precisely, we need to maintain a table which associates to each locus
  <with|mode|math|L> its ``cyclic closure'', <abbr|i.e.> the set of all loci
  <with|mode|math|L<rprime|'>> such that <with|mode|math|L> depends on
  <with|mode|math|L<rprime|'>> and vice versa, and the induced uncyclic graph
  structure on this table, which we expect (hope) to be simple in practice.
  As a consequence, we have a fast decision procedure for knowing whether a
  given locus or link is visible inside a given other locus.

  Next, for each name, we maintain the list of loci/links/roles in which it
  participates and vice versa. Assume that we need the list of links for a
  given locus. Then we first lookup the list of names for the locus and next
  the list of links participating in one of the names. Next, we filter out
  the links so as the keep only those which are visible in the locus. Next,
  we lookup the roles of each remaining link, which amounts to further
  filtering (<abbr|cf.> <markup|role-require>). The remaining links are ready
  to be used for rendering or navigation.

  <\remark>
    Notice that importation of linking context may be subject to additional
    restrictions: it is reasonable to require links with the
    <verbatim|import> role to be outbound and defined in the locus (except
    for publicly exported context to parent loci). This restriction may make
    it easier to maintain the ``cyclic reduction graph'' in an incremental
    way.

    In practice, explicit context importation should only be used at a few
    places (<abbr|i.e.> at the document level or in a few dedicated special
    tags). During the incremental cyclic closure computations it is then
    mainly necessary to keep track of the <verbatim|public>,
    <verbatim|private>, <verbatim|autonomous> and <verbatim|inherit> roles of
    loci.
  </remark>
</body>

<\initial>
  <\collection>
    <associate|language|english>
    <associate|page-bot|30mm>
    <associate|page-even|30mm>
    <associate|page-odd|30mm>
    <associate|page-reduce-bot|15mm>
    <associate|page-reduce-left|25mm>
    <associate|page-reduce-right|25mm>
    <associate|page-reduce-top|15mm>
    <associate|page-right|30mm>
    <associate|page-top|30mm>
    <associate|page-type|a4>
    <associate|par-width|150mm>
  </collection>
</initial>