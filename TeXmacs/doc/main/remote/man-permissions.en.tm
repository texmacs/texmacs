<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Permissions and access control><label|sec-man-permissions><label|sec-permissions>

  All remote resources in <TeXmacs> use a unified permission system that
  controls who can access and modify your shared content. Understanding how
  permissions work is essential for effective collaboration and maintaining
  security of your work.

  <subsection|Permissions>

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

  <subsection|Public versus private resources>

  Permissions can be set to specific users or made public.

  <\itemize>
    <item><strong|Specific users>: Add individual users by their username to
    the readable or writable permission lists. This gives fine-grained
    control over exactly who can access your resources.

    <item><strong|Public access (\Pall\Q)>: Setting a permission to
    <verbatim|all> makes the resource publicly accessible to anyone connected
    to the server. This is useful for:

    <\itemize-dot>
      <item>open chat rooms for community discussions,

      <item>shared reference documents that everyone may read,

      <item>collaborative documents with broad participation.
    </itemize-dot>

    Be cautious with public write access, as it allows anyone to modify the
    content.
  </itemize>

  <subsection|Managing permissions>

  As an owner of a resource, you can modify its permissions through the
  permissions editor:

  <\enumerate>
    <item>Open the resource (remote file, directory, live document, or chat
    room) in <TeXmacs>.

    <item>Select <menu|Remote|Permissions>. This menu item appears when
    viewing the resource (also available via the corresponding icon menu:
    <icon|tm_cloud_file.xpm> for files and chat rooms,
    <icon|tm_cloud_dir.xpm> for directories).

    <item>In the permissions dialog, you will see three tabs for Owner,
    Readable, and Writable permissions.

    <item>For each type of permission:

    <\itemize-dot>
      <item>select users from the list to grant them access,

      <item>deselect users to revoke their access,

      <item>select <verbatim|all> to make the permission public.
    </itemize-dot>

    <item>Changes are saved automatically as you modify the selections.
  </enumerate>

  The permissions editor displays all registered users on the server, making
  it easy to grant access to collaborators.

  <subsection|Inviting and sharing resources>

  <TeXmacs> provides convenient ways to share resources with specific users.

  <strong|Important.> Before inviting users or sharing resources, you should
  first set the appropriate permissions. Users cannot access a resource
  unless they have been granted read (and write, if needed) permissions on
  that resource.

  To share a resource with collaborators, you must first set permissions,
  then share:

  <\enumerate>
    <item>Open the resource in <TeXmacs>.

    <item>Select <menu|Remote|Permissions> and grant readable (and writable
    if they need to edit) access to the users you want to share with.

    <item>Select <menu|Remote|Share>. This menu item appears when viewing the
    resource.

    <item>Choose the users to share with from the list. Only users with
    permissions will appear in this list.

    <item>Click <menu|Send> to send the sharing notification.
  </enumerate>

  The share feature sends individual private messages to each selected user
  with a link to the resource. These messages appear in each user's private
  message inbox.

  <subsubsection|How sharing notifications work>

  When you use <menu|Remote|Share> or <menu|Remote|Invite>, the system sends
  individual private messages to each selected user. These messages contain a
  clickable link to the shared resource and appear in each user's private
  message inbox (accessed via <menu|Remote|Incoming messages>).

  Private messages are implemented as personal chat rooms (internally named
  <samp|mail-{username}>), ensuring that each user receives their
  notification privately and can access shared resources at their
  convenience.

  <subsection|Permission inheritance and defaults>

  <\itemize>
    <item><strong|New resources.> When you create a new resource, you become
    its owner by default. Other permissions start empty unless you explicitly
    set them.

    <item><strong|Chat rooms.> By default, newly created chat rooms have both
    readable and writable permissions set to <verbatim|all>, making them
    public. You can change this to restrict access.

    <item><strong|Live documents.> By default, new live documents have public
    read and write access (<verbatim|all>). This facilitates open
    collaboration but can be restricted as needed.

    <item><strong|Remote files and directories.> New remote files and
    directories are private by default (only the owner has access). You must
    explicitly grant access to collaborators.
  </itemize>

  <subsection|Best practices for permissions>

  <\itemize>
    <item><strong|Principle of least privilege.> Grant only the minimum
    permissions necessary. If users only need to read a document, don't give
    them write access.

    <item><strong|Review permissions regularly.> Periodically check who has
    access to your resources, especially for sensitive content.

    <item><strong|Use specific users for sensitive content.> Avoid public
    access (<verbatim|all>) for confidential or work-in-progress documents.

    <item><strong|Multiple owners for important resources.> Consider having
    multiple owners for critical resources to avoid single points of failure.

    <item><strong|Document your sharing decisions.> When sharing resources in
    chat, briefly explain who should have access and why.

    <item><strong|Remove access when collaboration ends.> When a project or
    collaboration concludes, remove write access from users who no longer
    need it.
  </itemize>

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