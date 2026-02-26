<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Live documents>

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
  <hlink|permissions|../man-permissions.en.tm> if needed. Live documents are
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