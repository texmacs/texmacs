<TeXmacs|1.0.7.20>

<style|<tuple|generic|doc>>

<\body>
  <doc-data|<doc-title|Plans and ideas for remote computations>>

  <section|Global design>

  The system for remote computations will be organized around two or three
  components:

  <\enumerate>
    <item>The server side. For the moment, this part is part of <TeXmacs>,
    but we do our best to keep it as independent as possible from <TeXmacs>.
    In particular, there should be no document generation on the server side,
    but rather an API for communication with clients.

    In the future, the server will be enriched with several modules: one
    module for live communications (collaborative editing, voice?, video?,
    etc.), one module for remote computations (computer algebra sessions,
    etc.).

    <item>The client side. For the moment, we mainly implement a <TeXmacs>
    client. A more traditional web client might also be implemented later.

    <item>Middleware. This optional part might factor out the generation of
    content between several types of clients: <TeXmacs> clients and usual
    browsers.
  </enumerate>

  <section|Remote file system>

  <subsection|Main design>

  The remote file system is a so called ``database file system''. The main
  server side object is called a<nbsp>``resource''. We maintain a database
  with properties of resources. Each property is currently a<nbsp>triple
  <scm|(rid attr val)>, where <verbatim|rid> is the unique identifier of the
  resource, <verbatim|attr> an attribute and<nbsp><verbatim|val> the
  corresponding value. An attribute can have several values.

  One special attribute is <verbatim|type>. There are currently three types
  of resources: <verbatim|user>, <verbatim|file> and <verbatim|dir>. Each
  resource should also admit the attribute <verbatim|name> and
  <verbatim|owner>. Special types of resources may admit additional specific
  attributes.

  <subsection|File management>

  The files and directories are organized in a usual directory structure, but
  they may admit addional attributes in order to facilitate searching (not
  yet implemented). The <verbatim|name> attribute of files and directories is
  the traditional file/directory name, whereas the <verbatim|dir> attribute
  specifies the directory (except for roots). Any user with a given
  <verbatim|pseudo> automatically acquire ownership over a few special
  directories, such as the home directory <verbatim|~pseudo>. The special
  attribute <verbatim|location> also specifies the physical location on the
  server side where the file is saved. An attributed <verbatim|mime> for the
  mime type of the file is planned for later.

  <subsection|Access rights>

  Currently, there are three types of access rights: <verbatim|owner>
  (ownership with read/write permission and the right to change the
  properties), <verbatim|readable> (read permission) and <verbatim|writable>
  (write permission).

  In addition, there are special virtual users which are called groups, which
  are really collections of other users (or groups); a member of the group is
  specified with the <verbatim|member> attribute. If a group is specified for
  a particular permission, then all its members have that permission. There
  is also one special group <verbatim|all>, which contains all users of the
  server.

  <subsection|Plans for versioning>

  The abstract way that we would wish to regard versioning is that a
  versioned file system (not yet implemented) may be contemplated using
  different views. Any view on the versioned file system results in an
  ordinary file system (implemented as described above). The file system
  consists of two parts: a database with <verbatim|(rid attr val)> entries,
  and attached files with bulk data. The view should therefore specify how to
  generate these two parts, at least virtually.

  For history management, the idea would be to use a database whose entries
  contain two additional fields <verbatim|start> and <verbatim|end>
  corresponding to the start and end dates of a property in the database. For
  instance <verbatim|(xy14z name test.tm 2008-08-02 2009-12-11)> might
  specify that the entry <verbatim|(xy14z name test.tm)> was valid between
  <verbatim|2008-08-02> and <verbatim|2009-12-11>.

  For branches (typically of projects), in which users may have their own
  modified versions of resources, the idea would be to regard the branch as a
  patch applied to the entire file system. On the one hand, this means that
  we have to patch the database. This is done through the introduction of
  additional fields <verbatim|branch> and <verbatim|direction> for database
  entries. The <verbatim|direction> (<verbatim|insert> or <verbatim|remove>)
  specifies whether an entry should be added to or removed from the unpatched
  database. On the other hand, the attached files to the database may have to
  be patched as well. This is done through two special attributes
  <verbatim|patcher> and <verbatim|patch>. The <verbatim|patcher> specifies a
  program or way which will be used for doing the patch (to the unpatched
  file at <verbatim|location>), and <verbatim|patch> contains the patch
  itself.

  Of course, views can be composed: we might apply the combined patches of
  several branches, or the patch corresponding to a past version of a branch
  to a past version of the file system.

  <section|User interface>

  <\itemize>
    <item>The property editor should contain three tabs ``General'',
    ``Permissions'' and ``Other''. The ``General'' tab should be read-only.
    In ``Permissions'', we only allow for addition and removal of users
    (maybe also search of users). In ``Other'', we also allow for adding new
    attributes. When pressing <key|return>, we should immediately activate
    new settings (no <key|Ok> button is necessary).

    <item>We should implement a facility for uploading or downloading a file
    or a directory tree. Bulk data might be transmitted as a unique
    compressed tar archive.

    <item>We should implement a remote file browser. This will in particular
    be used when moving a<nbsp>file or directory to another place.

    <item>We should make it possible to select one or more files in a
    directory. Selected files might be moved to another place, or we might
    want to edit their properties jointly.

    <item>We should implement smart directories with results of search
    queries.

    <item>We should implement various default pages or directories for users,
    such as public and private profiles, blogs, mailboxes, wikis, documents,
    projects, etc.

    <item>We should implement simple ways for managing groups. There are
    different kinds of groups, depending on various aspects:

    <\itemize>
      <item>Membership. In certain groups, only the owner may add or remove
      members (and people may explicitly request to become part of the
      group). In other groups, anyone may become a member (e.g. subscription
      to a mailing list).

      <item>Publicity. Where can we become aware of the existence of the
      group? Certain groups are purely private (friends of an anonymous
      coward). Other groups are listed by default on the public profiles of
      the owners. Public groups are listed on the public profiles of any of
      the members.
    </itemize>
  </itemize>

  <section|Remote editing>

  <\itemize>
    <item>The current remote file system still has to be made more
    asynchroneous.

    <\itemize>
      <item>Attributes such as mime type can only be retrieved with a delay.

      <item>Loading HTML files, images and inclusions should be made
      asynchroneous. We need a clean concept of ``remote resource'' on the
      client side (inside the C++ code), which can be updated via the remote
      connection, and triggers the necessary subsequent changes.

      <item>Live editing via remote resources or via loci? To be
      investigated.
    </itemize>
  </itemize>

  <section|Other points>

  <\itemize>
    <item>Simplified notation for urls, such as
    <verbatim|tmfs://localhost/~vdh/foo/bar.tm>. Also change the meaning of
    <verbatim|/> inside this kind of urls. For instance, inside the above
    url, a link to <verbatim|/other.tm> should really be a link to
    <verbatim|tmfs://localhost/~vdh/other.tm>.

    <item>Some code might be factored out between file and directory handling
    (<abbr|e.g.> <scm|remote-file-create> and <scm|remote-dir-create>).
  </itemize>
</body>

<initial|<\collection>
</collection>>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|2.1|?>>
    <associate|auto-4|<tuple|2.2|?>>
    <associate|auto-5|<tuple|2.3|?>>
    <associate|auto-6|<tuple|2.4|?>>
    <associate|auto-7|<tuple|3|?>>
    <associate|auto-8|<tuple|4|?>>
    <associate|auto-9|<tuple|5|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Global
      design> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Remote
      file system> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <with|par-left|<quote|1tab>|Main design
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|1tab>|File management
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|1tab>|Access rights
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5>>

      <with|par-left|<quote|1tab>|Plans for versioning
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|User
      interface> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Remote
      editing> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Other
      points> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>