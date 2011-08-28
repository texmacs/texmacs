<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Conversion de <TeXmacs> a <LaTeX>>

  La situación más común es que quiera convertir un artículo desde
  <apply|TeXmacs> a <apply|LaTeX>, con objeto de enviarlo a alguna
  publicación. Dado un fichero <apply|TeXmacs> que llamaremos
  <verbatim|nombre.tm>, puede convertirlo a un fichero <apply|LaTeX>
  <verbatim|nombre.tex> mediante <apply|menu|Fichero|Exportar|LaTeX>. Como
  primera opción puede ejecutar <apply|LaTeX> sobre <verbatim|nombre.tex>, y
  comprobar si obtiene resultados satisfactorios. En caso afirmativo, debe
  enviar <verbatim|nombre.tex> junto con el fichero de estilo
  <verbatim|TeXmacs.sty>, que podrá encontrar en el directorio
  <verbatim|$TEXMACS_PATH/misc/latex>.

  Es usual que la publicación a la que usted envíe sus trabajos use su propio
  fichero de estilo (llamémoslo <verbatim|journal.sty>). En ese caso , debe
  también copiar el fichero

  <\verbatim>
    \ \ \ \ $TEXMACS_PATH/styles/article.ts
  </verbatim>

  como\ 

  <\verbatim>
    \ \ \ \ ~/.TeXmacs/styles/journal.ts
  </verbatim>

  y usar <verbatim|journal> como su estilo de documento mediante
  <apply|menu|Documento|Estilo|Otro>. Opcionalmente puede editar
  <verbatim|journal.ts>, para que la presentación del artículo sea lo más
  conforme al estilo de la revista. En algunos casos, tendrá que crear una
  nueva copia de <verbatim|TeXmacs.sty>, y modificar alguno de los entornos
  para conseguir compatibilidad con el fichero <verbatim|journal.sty> de la
  revista.

  Si su primer intento de convertir tu documento a <apply|LaTeX> no produce
  un resultado satisfactorio, usualmente será debido a que la conversión
  incorrecta de pequeñas partes del texto. Esto puede ser debido a tres
  razones fundamentalmente:

  <\itemize>
    <item>Su texto usa algunas características exclusivas de <apply|TeXmacs>.

    <item>Está usando una característica de <apply|TeXmacs> que no ha sido
    implementada aún en el algoritmo de conversión.

    <item>Acaba de encontrar un fallo en el algoritmo de conversión.
  </itemize>

  Estas posibilidades serán discutidas en la siguiente sección.

  En caso de problemas, una estrategia simple sería corregir el fichero
  <apply|LaTeX> producido y enviarlo a la revista. Sin embargo, esta
  estrategia tiene la desventaja de que debe hacer las correcciones una y
  otra vez con cada conversión de tu fichero <apply|TeXmacs>
  <verbatim|nombre.tm> tras hacer modificaciones extra. Una estrategia mejor
  es usar <apply|menu|Insertar|Específico|Latex> e
  <apply|menu|Insertar|Específico|Texmacs> para escribir texto que solo será
  visible en el fichero convertido o en el original respectivamente.

  Por ejemplo: asumamos que la palabra ``blauwbilgorgel'' es partida al final
  de la línea correctamente en <apply|TeXmacs>, pero no en la conversión a
  <apply|LaTeX>. Deberá hacer lo siguiente:

  <\enumerate>
    <item>Seleccionar ``blauwbilgorgel''.

    <item>Click en <apply|menu|Insertar|Específico|Texmacs> para hacer el
    texto ``blauwbilgorgel'' específico de <apply|TeXmacs>.

    <item>Click en <apply|menu|Insertar|Specífico|Latex>.

    <item>Teclear el código latex <verbatim|blauw\\-bil\\-gor\\-gel> con las
    divisiones correctas.

    <item>Pulse <key|enter> para activar el texto específico <apply|LaTeX>.
  </enumerate>

  De la misma forma puede insertar saltos de línea o de página, espacios
  verticales, modificaciones de los parámetros del estilo, etc.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Álvaro Cantero
  Tejero|Pablo Ruiz Múzquiz|David Moriano Garcia|Offray Vladimir Luna
  Cárdenas>

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

<\references>
  <\collection>
    <associate|idx-5|<tuple|2.|?>>
    <associate|idx-6|<tuple|3.|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Fichero>|<with|font
      family|<quote|ss>|Exportar>|<with|font
      family|<quote|ss>|LaTeX>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Estilo>|<with|font
      family|<quote|ss>|Otro>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Específico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Específico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Específico>|<with|font
      family|<quote|ss>|Texmacs>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Specífico>|<with|font
      family|<quote|ss>|Latex>>|<pageref|idx-6>>
    </associate>
  </collection>
</auxiliary>
