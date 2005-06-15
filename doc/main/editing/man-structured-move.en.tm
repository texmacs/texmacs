<TeXmacs|1.0.5.3>

<style|tmdoc>

<\body>
  <tmdoc-title|Structured cursor movement>

  As a general rule, the behaviour of structured cursor movements (and many
  other structured editing operations) is conditioned by the <em|innermost
  tag> for which a particular behaviour has been defined.

  For instance, the structured cursor movements <key|H-<key-left>>,
  <key|H-<key-right>>, <key|H-<key-up>> and <key|H-<key-down>> have a
  particular meaning both inside tables and trees. Inside tables, they allow
  you to move the cursor cell-by-cell manner. Inside trees, they correspond
  to node-by-node movements. Whenever you inside a tree inside a table, then
  the innermost tag is a tree, and the node-by-node movements will take
  precedence over the cell-by-cell movements.

  <TeXmacs> implements the three main mechanisms for structured cursor
  movement:

  <\enumerate>
    <item>Traversal of the entire structure of the document.

    <item>Traversal of tags which are similar to the innermost tag.

    <item>Movements inside the innermost tag.
  </enumerate>

  Other structured editing operations...

  Customizing the behaviour...

  <paragraph|Structured traversal of the document>

  The <key|C-<key-left>>, <key|C-<key-right>>, <key|C-<key-up>> and
  <key|C-<key-down>> keys are used for the structured traversal of the entire
  document.

  <tmdoc-copyright|1998--2005|Joris van der Hoeven>

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
  </collection>
</initial>