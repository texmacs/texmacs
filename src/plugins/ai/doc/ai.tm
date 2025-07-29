<TeXmacs|2.1.4>

<style|tmdoc>

<\body>
  <tmdoc-title|Artificial intelligence tools inside <TeXmacs>>

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

  Let us now detail how to install the support for individual chatbots. If
  you wish to automate the process, in order to permanently support various
  chatbots, then you should put the appropriate commands in your personal
  startup file for shell sessions, such as <hgroup|<verbatim|~/.bashrc>> or
  <verbatim|~/.profile>.

  <section*|ChatGPT>

  Please follow the following instructions for setting up <name|ChatGPT> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|ChatGPT> and obtain a key. Keys
    typically start with <verbatim|sk->.

    <item>In your terminal, set the <verbatim|OPENAI_API_KEY> environment
    variables with your key:

    <\shell-code>
      export OPENAI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>You need to install <verbatim|openai-cli>, the <name|OpenAI>
    command line interface. This is a bit tricky, because it requires you to
    create a <name|Python> virtual environment (assuming that you already
    have <name|Python> on your computer; otherwise install <name|Python>
    first). You can do this in any directory <verbatim|<with|color|dark
    green|<em|dir>>> from where you wish to launch <TeXmacs>, as
    follows:<no-break-here>

    <\shell-code>
      cd <text|<verbatim|<with|color|dark green|<em|dir>>>>

      python3 -m venv <text|<verbatim|<with|color|dark green|<em|myenv>>>>

      source <text|<verbatim|<with|color|dark
      green|<em|dir>>>>/<text|<verbatim|<with|color|dark
      green|<em|myenv>>>>/bin/activate
    </shell-code>

    <item>Now install the <name|OpenAI> command line interface:

    <\shell-code>
      pip3 install openai-cli
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|ChatGPT>.
  </itemize>

  <section*|Gemini>

  Please follow the following instructions in order to setup <name|Gemini>
  for use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|Gemini> and obtain a key.

    <item>In your terminal, set the <verbatim|GEMINI_API_KEY> environment
    variables with your key:

    <\shell-code>
      export GEMINI_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|Gemini>.
  </itemize>

  <section*|Llama>

  <name|Llama> has the advantage that the models can be run on your own
  computer, without any internet connection. Our preferred way to do this is
  through <verbatim|ollama>:

  <\itemize>
    <item>Install <verbatim|ollama> on your computer following the
    instructions from <hlink|here|https://ollama.com/>.

    <item>Download the model that you wish to use, <abbr|e.g.>
    <verbatim|llama3>:

    <\shell-code>
      ollama pull llama3
    </shell-code>

    <TeXmacs> also supports <verbatim|llama4>.

    <item>When launching <TeXmacs>, you should now be able to use <name|Llama
    3>.
  </itemize>

  <section*|Mistral>

  Please follow the following instructions for setting up <name|Mistral> for
  use inside <TeXmacs>.

  <\itemize>
    <item>Create an account for <name|Mistral> and obtain a key.

    <item>In your terminal, set the <verbatim|MISTRAL_API_KEY> environment
    variables with your key:

    <\shell-code>
      export MISTRAL_API_KEY=<text|<verbatim|<with|color|dark
      green|<em|your_key>>>>
    </shell-code>

    <item>When launching <TeXmacs>, you should now be able to use
    <name|Mistral>.
  </itemize>

  <tmdoc-copyright|2025|Joris van der Hoeven|Marc Lalaude-Labayle|Robin Wils>

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