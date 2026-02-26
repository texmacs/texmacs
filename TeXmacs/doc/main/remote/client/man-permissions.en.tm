<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Permissions and access control><label|sec-man-permissions><label|sec-permissions>

  All remote resources in <TeXmacs> use a unified permission system that
  controls who can access and modify your shared content. Understanding how
  permissions work is essential for effective collaboration and maintaining
  security of your work.

  <TeXmacs> provides three types of permissions for each resource:

  <\description>
    <item*|Owner>The owner has complete control over the resource. Owners
    can:

    <\itemize-dot>
      <item>read and write the resource content,

      <item>modify all permissions (readable, writable, and ownership),

      <item>delete the resource,

      <item>transfer ownership to other users,

      <item>invite or share the resource with others.
    </itemize-dot>

    A resource can have multiple owners. When you create a resource, you
    automatically become its initial owner.

    <item*|Readable>Users with read access can:

    <\itemize-dot>
      <item>view the resource and its content,

      <item>see the resource in listings and searches,

      <item>open the resource in <TeXmacs>,

      <item>for remote files: view the version history.
    </itemize-dot>

    Read access does not allow making changes to the resource.

    <item*|Writable>Users with write access can:

    <\itemize-dot>
      <item>modify the resource content,

      <item>for remote files, create new versions by saving changes,

      <item>for live documents, edit the document in real-time,

      <item>for chat rooms, send messages and share resources,

      <item>for directories, create, rename, or delete files within the
      directory.
    </itemize-dot>

    Write access automatically includes read access. Users with write access
    cannot modify permissions or delete the resource itself unless they are
    also owners.
  </description>

  <\traverse>
    <branch|Public versus private resources|permissions/man-public-private.en.tm>

    <branch|Managing permissions|permissions/man-managing.en.tm>

    <branch|Inviting and sharing resources|permissions/man-sharing.en.tm>

    <branch|Permission inheritance and defaults|permissions/man-defaults.en.tm>

    <branch|Best practices for permissions|permissions/man-best-practices.en.tm>
  </traverse>

  <tmdoc-copyright|2025|Robin Wils>

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
    <associate|preamble|false>
  </collection>
</initial>