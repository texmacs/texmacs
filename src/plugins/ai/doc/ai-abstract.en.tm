<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Introduction to AI tools inside <TeXmacs>>

  <TeXmacs> contains experimental support for various chatbots. For
  conversations with programs such as <TeXmacs> (which are not recognized as
  web browsers), most chatbots require you to generate a private key for all
  conversations (they can often generate such keys for free). Below, you will
  find specific instructions how to setup various chatbots for communication
  with <TeXmacs>.

  Assuming that your chatbot, say <name|ChatGPT> is recognized by <TeXmacs>,
  you may use it the following ways:

  <\enumerate>
    <item>For direct chats, inside a traditional shell session, using
    <menu|Insert|Shell|ChatGPT>. In that case, <TeXmacs> allows you to
    directly put mathematical formulas in your queries and output with
    mathematical formulas can directly be cut and pasted into your documents.

    <item>For translations into another language. In that case, you first
    have to select your favorite engine via <menu|Tools|AI engine>. Next, you
    may simply select a piece of text and translate it to another language
    using <menu|Tools|Translate>. Note that chatbots are typically fairly
    slow, so you need to be a little bit patient, especially when selecting a
    large piece of text.

    <item>For correcting the spelling and grammar of a text. This works in a
    similar way as translation, except that you should now do
    <menu|Tools|Correct>.
  </enumerate>

  If setting up a chatbot for <TeXmacs> is too much work, or if you wish to
  use an unsupported service, then we provide one final possibility to
  interact with structured <TeXmacs> documents. Assume for instance that we
  wish to translate a piece of <TeXmacs> text from English into French using
  <name|DeepL>:

  <\enumerate>
    <item>irst copy this piece of text using <menu|Tools|External AI|Copy>.
    Now paste the text into <name|DeepL>. (This results your text to be
    pasted as an HTML document, while replacing all non-textual content by
    unique codes for internal use by <TeXmacs>.)

    <item>Next use <name|DeepL> to translate the selected text into a
    language of your choosing.

    <item>Finally paste the translation back into <TeXmacs> using
    <menu|Tools|External AI|Paste>. The structure of the original selection
    should be recovered automatically.
  </enumerate>

  <tmdoc-copyright|2025|Joris van der Hoeven>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|page-medium|papyrus>
  </collection>
</initial>