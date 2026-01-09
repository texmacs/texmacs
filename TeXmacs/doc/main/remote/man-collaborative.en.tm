<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Remote tools and collaborative
  editing><label|sec-man-collaborative>

  <TeXmacs> includes a powerful client-server architecture that enables
  real-time collaboration, remote file management, and instant messaging.
  This system allows multiple users to work together on documents, share
  resources, and communicate through integrated chat rooms. The remote tools
  integrate seamlessly with <TeXmacs>, making collaboration feel natural and
  intuitive.

  In this document, a remote resource refers to content with remote
  collaborative features. Remote resources can be:

  <\itemize>
    <item>A file or directory

    <item>A chat room (private messages are sent to the
    <verbatim|mail-{username}> chat room)

    <item>A live document
  </itemize>

  \ This document only describes how to interact with the remote resources,
  for information on how to setup a server, please read <hlink|Setting up a
  <TeXmacs> server|man-server.en.tm>.

  <section|Remote tools>

  To access remote facilities, you must enable the <menu|Tools|Remote tool>.
  This adds the <menu|Remote> menu and toolbar icons, which provide all the
  actions to communicate with a remote server and access its resources.

  <subsection|Main menus>

  <icon|tm_cloud.svg> Connection facility: Login, logout, create account,
  reset credentials.

  <icon|tm_cloud_home.svg> My resources on the server: Access Home, chat
  rooms, shared resources and live documents.

  <icon|tm_cloud_mail.svg> Mail: Access and send messages.

  <subsection|Contextual menus>

  <icon|tm_cloud_file.svg> Interact with the current file or file-like
  resource (remote file, live document, chat room): change permissions, share
  with users.

  <icon|tm_cloud_dir.svg> Interact with the current directory or
  directory-like resource (remote directory, live documents list): create new
  resources, share with users, synchronize.

  <section|Connecting to a TeXmacs server <icon|tm_cloud.svg>>

  Before using any collaborative features, you need to connect to a <TeXmacs>
  server. A server can be hosted by your institution, research group, or you
  can connect to a public server if available.

  To connect to a server:

  <\enumerate>
    <item>Go to <menu|Remote|Login> (or click the <icon|tm_cloud.xpm> cloud
    icon) to open the connection dialog.

    <item>Enter the server address (hostname or IP address) and optionally
    specify a port number if the server uses a non-standard port (the default
    is 6561).

    <item>If you already have an account, enter your username and password.
    Otherwise, you can create a new account (see the section below).

    <item>Click <menu|Connect> to establish the connection.
  </enumerate>

  Once connected, you will have access to all remote features. You can
  disconnect at any time using <menu|Remote|Logout>.

  The server supports both TLS-encrypted connections (<strong|highly
  recommended>) and legacy unencrypted connections (<strong|not
  recommended>). If security is important for your work, ensure that your
  server administrator has enabled TLS support.

  <subsection|Creating and managing accounts>

  User accounts on a <TeXmacs> server provide access to personal storage,
  chat rooms, and collaborative documents. When creating an account, you will
  need to provide:

  <\itemize>
    <item>A unique username (also called a pseudonym)

    <item>A password (servers may enforce strong password requirements)

    <item>Your full name

    <item>An email address (used for password recovery and notifications)
  </itemize>

  Account management features include:

  <\itemize>
    <item>Changing your password through <menu|Remote|Edit account>

    <item>Recovering your password via email if you forget your credentials

    <item>Updating account preferences and personal information

    <item>Viewing your account status and server permissions
  </itemize>

  <section|Messages and chat rooms <icon|tm_cloud_mail.svg>>

  Chat rooms provide a convenient way to communicate with collaborators
  directly within <TeXmacs>. Unlike email or external messaging apps, chat
  rooms are integrated with the document editing environment, allowing you to
  seamlessly share resources and discuss your work.

  <subsection|Creating a chat room>

  To create a new chat room:

  <\enumerate>
    <item>Go to <menu|Remote|Chat rooms> to view your chat rooms list.

    <item>Select <menu|Remote|New chat room> (also available via the
    <icon|tm_cloud_dir.xpm> icon menu). This menu item appears when viewing
    the chat rooms list.

    <item>Enter a name for your chat room. Choose a descriptive name that
    reflects the purpose or topic.

    <item>The chat room will be created and you will be automatically joined
    as the owner.
  </enumerate>

  As the owner of a chat room, you have full control over its settings and
  can invite other users. When viewing a chat room, use <menu|Remote|Invite>
  (also accessible via the <icon|tm_cloud_file.xpm> icon menu) to invite
  users. See <hlink|Permissions and access control|#sec-permissions> for more
  details on managing chat room access.

  <subsection|Joining and using chat rooms>

  To join an existing chat room:

  <\enumerate>
    <item>Go to <menu|Remote|Chat rooms> to view your chat rooms list.

    <item>Select <menu|Remote|Join chat room>.

    <item>Select the chat room you want to join from the list.
  </enumerate>

  Once inside a chat room, you can:

  <\itemize>
    <item>Send text messages to all participants

    <item>Format messages using <TeXmacs> markup and mathematical formulas

    <item>Share resources such as documents, remote files, and live documents
    (see <hlink|sharing below|#sharing-resources-with-users>)

    <item>View who is currently present in the room

    <item>See the history of messages and shared resources
  </itemize>

  <subsection|Sharing resources with users>

  Resources can be shared with other users to facilitate collaboration. When
  you share a resource, the selected users receive a private message with a
  clickable link. You can only share with users who have been granted
  permissions to access that resource.

  <section|Understanding shared resources>

  <TeXmacs> provides three main types of shared remote resources, each
  designed for different collaboration scenarios. Understanding the
  differences between these resource types will help you choose the right
  tool for your needs.

  <subsection|Remote files <icon|tm_cloud_file.svg>>

  Remote files are documents stored on the <TeXmacs> server with full version
  history. When you create or edit a remote file, the server maintains a
  complete record of all changes over time.

  Key characteristics of remote files:

  <\itemize>
    <item><em|Versioned storage>: Every save creates a new version that can
    be retrieved later.

    <item><em|Asynchronous collaboration>: Multiple people can work on the
    same remote file, but not simultaneously. When you open a remote file,
    you are working with your own local copy.

    <item><em|Manual conflict resolution>: If multiple people edit the same
    remote file, you will need to manually compare versions and resolve
    conflicts using the versioning tool (see <hlink|Versioning
    tools|man-versioning.en.tm>).

    <item><em|Full history>: You can view the complete history of edits, see
    who made which changes and when, and revert to any previous version.
  </itemize>

  Remote files are ideal for projects where you want to maintain a complete
  audit trail of changes, or when collaborators work at different times
  rather than simultaneously.

  To create a remote file, navigate to your home directory or any remote
  directory (<menu|Remote|Home directory>), then select <menu|Remote|New
  remote file>. This menu item appears when viewing a remote directory.
  Alternatively, you can save a buffer with a <samp|tmfs://remote-file/> URL.
  By default, new remote files are private; you must grant permissions to
  collaborators (see <hlink|Permissions and access
  control|#sec-permissions>).

  <subsection|Remote directories <icon|tm_cloud_dir.svg>>

  Remote directories provide a way to organize and navigate collections of
  remote files on the server. They function similarly to folders in a regular
  filesystem, but exist on the server where all team members can access them.

  Remote directories:

  <\itemize>
    <item>Can contain remote files, subdirectories, and other resources

    <item>Are navigable through the <TeXmacs> interface

    <item>Provide a shared organizational structure for team resources

    <item>Can be shared with users to give access to entire collections of
    files
  </itemize>

  Directory permissions control who can view and modify files within the
  directory (see <hlink|Permissions and access control|#sec-permissions>).

  <subsection|Live documents>

  Live documents provide true real-time collaborative editing, similar to
  tools like Google Docs. When multiple people open the same live document,
  they can all edit simultaneously and see each other's changes appear
  instantly.

  Key characteristics of live documents:

  <\itemize>
    <item><em|Real-time synchronization>: Changes made by any participant are
    broadcast to all connected users.

    <item><em|Patch-based merging>: The system uses patch operations to
    combine concurrent edits. Compatible edits are automatically merged using
    a pull/rebase algorithm similar to version control systems.

    <item><em|Conflict handling>: When concurrent edits conflict (e.g.,
    editing the same region), the system retracts to the latest compatible
    state and reapplies changes where possible.

    <item><em|Session-based>: Live documents maintain state per connection,
    tracking each participant's view of the document.
  </itemize>

  The live document system tracks document states and uses patch-pull
  operations to combine changes from multiple participants.

  To create a live document, go to <menu|Remote|Live documents>, then select
  <menu|Remote|New live document>. This menu item appears when viewing the
  live documents list. By default, live documents are publicly accessible for
  reading and writing; you can restrict access through
  <hlink|permissions|#sec-permissions> if needed. Live documents are
  particularly useful for:

  <\itemize>
    <item>Real-time brainstorming and note-taking during meetings

    <item>Pair programming or collaborative code development

    <item>Simultaneous editing of mathematical proofs or formulas

    <item>Any scenario where immediate feedback and synchronous work is
    valuable
  </itemize>

  <section|Permissions and access control><label|sec-permissions>

  All remote resources in <TeXmacs> use a unified permission system that
  controls who can access and modify your shared content. Understanding how
  permissions work is essential for effective collaboration and maintaining
  security of your work.

  <subsection|Permission levels>

  <TeXmacs> provides three levels of access control for each resource:

  <\description>
    <item*|Owner>The owner has complete control over the resource. Owners
    can:

    <\itemize-dot>
      <item>Read and write the resource content

      <item>Modify all permissions (readable, writable, and ownership)

      <item>Delete the resource

      <item>Transfer ownership to other users

      <item>Invite or share the resource with others
    </itemize-dot>

    A resource can have multiple owners. When you create a resource, you
    automatically become its initial owner.

    <item*|Readable>Users with read access can:

    <\itemize-dot>
      <item>View the resource and its contents

      <item>See the resource in listings and searches

      <item>Open the resource in <TeXmacs>

      <item>For remote files: view the version history
    </itemize-dot>

    Read access does not allow making changes to the resource.

    <item*|Writable>Users with write access can:

    <\itemize-dot>
      <item>Modify the resource content

      <item>For remote files: create new versions by saving changes

      <item>For live documents: edit the document in real-time

      <item>For chat rooms: send messages and share resources

      <item>For directories: create, rename, or delete files within the
      directory
    </itemize-dot>

    Write access automatically includes read access. Users with write access
    cannot modify permissions or delete the resource itself unless they are
    also owners.
  </description>

  <subsection|Public vs. private resources>

  Permissions can be set to specific users or made public:

  <\itemize>
    <item><strong|Specific users>: Add individual users by their username to
    the readable or writable permission lists. This gives fine-grained
    control over exactly who can access your resources.

    <item><strong|Public access ("all")>: Setting a permission to
    <verbatim|all> makes the resource publicly accessible to anyone connected
    to the server. This is useful for:

    <\itemize-dot>
      <item>Open chat rooms for community discussions

      <item>Shared reference documents that everyone should read

      <item>Collaborative documents with broad participation
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

    <item>For each permission level:

    <\itemize-dot>
      <item>Select users from the list to grant them access

      <item>Deselect users to revoke their access

      <item>Select <verbatim|all> to make the permission public
    </itemize-dot>

    <item>Changes are saved automatically as you modify the selections.
  </enumerate>

  The permissions editor displays all registered users on the server, making
  it easy to grant access to collaborators.

  <subsection|Inviting and sharing resources>

  <TeXmacs> provides convenient ways to share resources with specific users.

  <strong|Important>: Before inviting users or sharing resources, you should
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
    <item><strong|New resources>: When you create a new resource, you become
    its owner by default. Other permissions start empty unless you explicitly
    set them.

    <item><strong|Chat rooms>: By default, newly created chat rooms have both
    readable and writable permissions set to <verbatim|all>, making them
    public. You can change this to restrict access.

    <item><strong|Live documents>: By default, new live documents have public
    read and write access (<verbatim|all>). This facilitates open
    collaboration but can be restricted as needed.

    <item><strong|Remote files and directories>: New remote files and
    directories are private by default (only the owner has access). You must
    explicitly grant access to collaborators.
  </itemize>

  <subsection|Best practices for permissions>

  <\itemize>
    <item><strong|Principle of least privilege>: Grant only the minimum
    permissions necessary. If users only need to read a document, don't give
    them write access.

    <item><strong|Review permissions regularly>: Periodically check who has
    access to your resources, especially for sensitive content.

    <item><strong|Use specific users for sensitive content>: Avoid public
    access (<verbatim|all>) for confidential or work-in-progress documents.

    <item><strong|Multiple owners for important resources>: Consider having
    multiple owners for critical resources to avoid single points of failure.

    <item><strong|Document your sharing decisions>: When sharing resources in
    chat, briefly explain who should have access and why.

    <item><strong|Remove access when collaboration ends>: When a project or
    collaboration concludes, remove write access from users who no longer
    need it.
  </itemize>

  <section|Versioning vs. live documents: choosing the right tool>

  A common point of confusion is the difference between <TeXmacs> versioning
  system (discussed in <hlink|Versioning tools|../editing/man-versioning.en.tm>)
  and live documents. Both support collaboration, but they serve different
  purposes and use different workflows.

  <\wide-tabular>
    <tformat|<cwith|1|1|1|-1|cell-background|pastel
    grey>|<cwith|1|-1|1|1|cell-lborder|1ln>|<cwith|1|-1|2|2|cell-rborder|1ln>|<cwith|1|1|1|-1|cell-bborder|1ln>|<cwith|-1|-1|1|-1|cell-bborder|1ln>|<cwith|1|-1|1|-1|cell-tborder|1ln>|<table|<row|<\cell>
      <strong|Versioning system (for remote files)>
    </cell>|<\cell>
      <strong|Live documents>
    </cell>>|<row|<\cell>
      <em|Use when you want:>

      <\itemize-dot>
        <item>Asynchronous collaboration

        <item>Complete control over accepting or rejecting changes

        <item>A permanent record of all versions and who made which changes

        <item>The ability to compare versions side-by-side and selectively
        merge

        <item>Integration with external version control systems like Git or
        Subversion

        <item>To review changes before incorporating them into your document
      </itemize-dot>
    </cell>|<\cell>
      <em|Use when you want:>

      <\itemize-dot>
        <item>Real-time collaboration

        <item>Immediate feedback and visibility of changes

        <item>Natural back-and-forth during meetings or working sessions

        <item>Patch-based merging of compatible edits

        <item>Faster iteration and dynamic collaboration
      </itemize-dot>
    </cell>>>>
  </wide-tabular>

  In practice, you can use both approaches: live documents for active working
  sessions and brainstorming, and remote files with versioning for more
  formal document management and long-term storage.

  <section|Notifications>

  <TeXmacs> can receive push notifications from the server, alerting you to:

  <\itemize>
    <item>New messages in chat rooms you participate in

    <item>Invitations to join new chat rooms or collaborate on documents
  </itemize>

  <section|Security and privacy>

  When using remote tools, keep in mind:

  <\itemize>
    <item>All communication between client and server can be encrypted using
    TLS if the server supports it.

    <item>Passwords are stored using salted hashing on the server.

    <item>Access control for resources (files, directories, chat rooms) can
    be configured by resource owners.

    <item>Server administrators may have access to all stored content for
    backup and maintenance purposes.

    <item>Check with your server administrator about data retention policies
    and backup procedures.
  </itemize>

  <section|Troubleshooting>

  If you encounter connection issues:

  <\itemize>
    <item>Verify that you entered the correct server address and port number.

    <item>Check that your network allows outgoing connections to the server.

    <item>Ensure your username and password are correct.

    <item>If the server uses TLS, verify that the GnuTLS plugin is properly
    installed.

    <item>Contact your server administrator if problems persist.
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