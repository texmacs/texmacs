<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Convenciones para los nombres de archivos>

  La mayoría de la documentación debe ser organizada como una función del
  tópico en un arbol de directorios. Los subdirectorios en el directorio
  superior son los siguientes:

  <\description>
    <expand|item*|devel>Documentación para desarrolladores.

    <expand|item*|examples>Ejemplos de documentos <TeXmacs>.

    <expand|item*|incoming>Documentación por venir, la cual es aún un poco
    experimental.

    <expand|item*|main>La documentación principal.

    <expand|item*|meta>Como escribir documentación y la compilación de
    documentación.
  </description>

  Por favor trate de mantener el número de entradas por directorio
  razonablemente pequeño.

  Los nombres de archivos en el directorio principal deben ser de la forma
  <verbatim|nombre-tipo.lenguaje.tm>. En los otros directorios, ellos son de
  la forma <verbatim|nombre.lenguaje.tm>. Aquí <verbatim|tipo> es una
  indicación principal del tipo de documentación; debe ser uno de los
  siguientes:

  <\description>
    <expand|item*|adv>Documentación para usuarios avanzados.

    <expand|item*|man>Para inclusión en el manual <TeXmacs>.

    <expand|item*|tut>Para inclusión en el tutorial <TeXmacs>.
  </description>

  Usted debe tratar de mantener la documentación sobre el mismo tema junta,
  sin consideraciones del tipo. De hecho, esto le permite encontrar más
  fácilmente toda la documentación existente sobre un tópico particular.
  También, puede pasar que usted quiere incluir documentación la cual fue
  incialmente concebida para el tutorial o el manual. El <verbatim|lenguaje>
  en el cual esta documentación ha diso escrita debe ser un código de dos
  letras como <verbatim|en>, <verbatim|fr>, etc. El <verbatim|nombre>
  principal de su archivo debería ser el mismo para traducciónes en otros
  lenguajes. Por ejemplo <verbatim|man-keyboard.en.tm> no debe ser traducido
  como <verbatim|man-clavier.fr.tm>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven, Offray Vladimir
  Luna Cárdenas>

  <expand|tmdoc-license|Se garantiza el permiso para copiar, distribuir y/o
  modificar este documento bajo los terminos de la GNU Free Documentation
  License, Versión 1.1 o cualquier versión posterior publicada por la Free
  Software Foundation; sin Secciones Invariantes, sin Textos de Portada, y
  sin Textos de Contraportada. Una copia de la licencia está incluida en la
  sección titulada "GNU Free Documentation License".>

  <expand|tmdoc-license|Permission is granted to copy, distribute and/or
  modify this document under the terms of the GNU Free Documentation License,
  Version 1.1 or any later version published by the Free Software Foundation;
  with no Invariant Sections, with no Front-Cover Texts, and with no
  Back-Cover Texts. A copy of the license is included in the section entitled
  "GNU Free Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|paragraph width|150mm>
    <associate|odd page margin|30mm>
    <associate|shrinking factor|4>
    <associate|page right margin|30mm>
    <associate|page top margin|30mm>
    <associate|reduction page right margin|25mm>
    <associate|page type|a4>
    <associate|reduction page bottom margin|15mm>
    <associate|even page margin|30mm>
    <associate|reduction page left margin|25mm>
    <associate|page bottom margin|30mm>
    <associate|reduction page top margin|15mm>
    <associate|language|spanish>
  </collection>
</initial>
