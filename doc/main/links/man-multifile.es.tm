<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Libros y documentos de múltiples archivos>

  Cuando un documento se vuelve realmente grande, usted puede querer
  subdividirlo en piezas más pequeñas. Así las piezas son reutilizables en
  otros trabajos y por otra parte, se mejora la velocidad de edición. Se
  puede insertar un fichero entero dentro de otro utilizando
  <apply|menu|Insertar|Enlace|Incluir fichero>. Para acelerar el tratamiento
  de todos los documentos incluidos por este procedimiento, <TeXmacs> recurre
  a una técnica conocida como <em|buffering>. Eso hace necesario actualizar
  los documentos incluídos a través de <apply|menu|Tools|Update|Inclusions>.

  Por ejemplo, si está escribiendo un libro, es normal que ponga los
  capítulos en archivos individuales, separados, llamados quizá
  <verbatim|c1.tm>, <verbatim|c2.tm...>Un archivo, <verbatim|libro.tm> hace
  de <em|documento maestro>, simplemente insertando en él los enlaces a las
  partes como se describe en el párrafo anterior. Es normal ubicar el índice,
  la bibliografía, etc, en <verbatim|libro.tm>.

  De todos modos, este modo de identificar el documento maestro sólo funciona
  en una dirección: se le dice al documento maestro cuáles son sus partes,
  pero no a las partes cuál es su documento maestro. Y esto último se hace
  imprescindible si se quieren ver las referencias cruzadas entre partes
  (capítulos en nuestro ejemplo) mientras se edita una de ellas en
  particular. La solución es sencilla: especificar <verbatim|libro.tm> como
  <em|documento maestro> para <verbatim|ci.tm>, mediante la opción de menú
  <apply|menu|Document|Master|Attach>. Como por el momento esta operación no
  resuelve la numeración de capítulos, recuerde que puede asignar al
  principio de cada capítulo la variable de entonrno <verbatim|chapternr> de
  modo que al estar editando <verbatim|ci.tm> aparezca ``Capítulo i'' al
  principio del documento y no ``Capítulo 1''.

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
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Enlace>|<with|font family|<quote|ss>|Incluir
      fichero>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|ss>|Herramientas>|<with|font
      family|<quote|ss>|Actualizar>|<with|font
      family|<quote|ss>|Inclusiones>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|ss>|Documento>|<with|font
      family|<quote|ss>|Maestro>|<with|font
      family|<quote|ss>|Vincular>>|<pageref|idx-3>>
    </associate>
  </collection>
</auxiliary>
