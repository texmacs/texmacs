<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Shared resources><label|sec-man-share>

  <TeXmacs> provides three main types of shared remote resources, each
  designed for different collaboration scenarios. Understanding the
  differences between these resource types will help you choose the right
  tool for your needs.

  <subsection|Remote files>

  Remote files are documents stored on the <TeXmacs> server with full version
  history. When you create or edit a remote file, the server maintains a
  complete record of all changes over time.

  Key characteristics of remote files:

  <\itemize>
    <item><em|Versioned storage>. Every save creates a new version that can
    be retrieved later.

    <item><em|Asynchronous collaboration>. Multiple people can work on the
    same remote file, but not simultaneously. When you open a remote file,
    you are working with your own local copy.

    <item><em|Manual conflict resolution>. If multiple people edit the same
    remote file, you will need to manually compare versions and resolve
    conflicts using the versioning tool (see <hlink|Versioning
    tools|man-versioning.en.tm>).

    <item><em|Full history>. You can view the complete history of edits, see
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

  <subsection|Remote directories>

  Remote directories provide a way to organize and navigate collections of
  remote files on the server. They function similarly to folders in a regular
  filesystem, but exist on the server where all team members can access them.

  Remote directories:

  <\itemize>
    <item>Can contain remote files, subdirectories, and other resources.

    <item>Are navigable through the <TeXmacs> interface.

    <item>Provide a shared organizational structure for team resources.

    <item>Can be shared with users to give access to entire collections of
    files.
  </itemize>

  Directory permissions control who can view and modify files within the
  directory (see <hlink|Permissions and access control|#sec-permissions>).

  <subsection|Live documents>

  Live documents provide true real-time collaborative editing. When multiple
  people open the same live document, they can all edit simultaneously and
  see each other's changes appear instantly.

  Key characteristics of live documents:

  <\itemize>
    <item><em|Real-time synchronization>. Changes made by any participant are
    broadcast to all connected users.

    <item><em|Patch-based merging>. The system uses patch operations to
    combine concurrent edits. Compatible edits are automatically merged using
    a pull/rebase algorithm similar to version control systems.

    <item><em|Conflict handling>. When concurrent edits conflict (e.g.,
    editing the same region), the system retracts to the latest compatible
    state and reapplies changes where possible.

    <item><em|Session-based>. Live documents maintain state per connection,
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
    <item>real-time brainstorming and note-taking during meetings,

    <item>pair programming or collaborative code development,

    <item>simultaneous editing of mathematical proofs or formulas,

    <item>any scenario where immediate feedback and synchronous work is
    valuable.
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