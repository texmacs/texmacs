<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Objetos dinámicos>

  Algunos objetos más complejos tienen varios estados durante el proceso de
  edición. Ejemplos de tales <em|objetos dinámicos> son los rótulos y las
  referencias, porque la aparición de la referencias depende de un número
  dinámicamente determinado. Muchos otros ejemplos de etiquetamiento dinámcio
  pueden ser encontrados en la documentación acerca de la
  <apply|hyper-link|escritura de archivos de
  estilo|../../../devel/style/keyboard/style-kbd.en.tm>.

  Cuando se ingresa un objeto dinámico como un rótulo usando
  <shortcut|(make-label)>, el estado por defecto es <em|inactivo>. Este estado
  inactibo le permite teclar información que es relevandte al objeto
  dinámico, tal como el nombre del rótulo en nuestro caso. Ciertos objetos
  dinámicos toman un número arbitrario de parámetros y los nuevos pueden ser
  insertados usando <key|tab>.

  Cuando termine de teclar la información relevante para su objeto dinámico,
  puede teclar <key|enter> a fin de <em|activar> el objeto. Un objeti
  dinámico activo puede ser desactivado colocando su cursor justo después del
  objeto y pulsando <key|backspace>.

  <apply|tmdoc-copyright|1998--2002|Joris van der Hoeven|Offray Vladimir Luna
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
    <associate|language|english>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|toc-1|<tuple|<uninit>|?>>
    <associate|idx-1|<tuple|1|?>>
    <associate|idx-2|<tuple|1|?>>
  </collection>
</references>
