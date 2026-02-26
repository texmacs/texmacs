<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Remote files>

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
    tools|../../../editing/man-versioning.en.tm>).

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
  control|../man-permissions.en.tm>).

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
