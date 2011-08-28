<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Planes para el futuro>

  <with|color|red|[Debería ser actualizado]>

  <section|Composición tipográfica>

  Algunas facilidades importantes de composición tipográfica que aún no han
  sido implementadas son las siguientes

  <\itemize>
    <item>Separación de un documento en varias partes.

    <item>Objetos dinámicos como en HTML.

    <item>Ambientes para las interfaces con sistemas de algebra
    computacional.\ 
  </itemize>

  Muchas facilidades menores deben también ser completadas. Listamos unas
  pocas de ellas:

  <\itemize>
    <item>Considerar fracciones como operadores
    <with|mode|math|\<Rightarrow\>> espacios antes y después. Similarmente
    para scripts <with|mode|math|\<Rightarrow\>> pequeño espacio antes de
    scripts izquieros and después de scripts derechos.
  </itemize>

  <section|Facilidades extra para edición de textos>

  Aunque el movimiento del curso, selecciones, etc. han sido ya
  implementadas, algunas otras facilidades de edición estándar no han sido
  aún completadas. Permítanos mencionar unas pocas de ellas:

  <\itemize>
    <item>Buscar textos, fórmulas ciertos entornos, etc.

    <item>Reemplzao de consulta.

    <item>Facilidades matemáticas: simplificación de una región seleccionada,
    sustituciones de fómrulas en otras fórmulas, etc.

    <item>Control de version\ 

    <item>Compresión y protección de datos.\ 

    <item>Revisores gramaticales y programas de traducción automáticas. Sabe
    alguien donde encontrar diccionarios libres detallados y cosas como esas?

    <item>Incorporación de un pgorama de reconocimiento de habla libre.
  </itemize>

  <section|Una hoja de cálculo universal>

  Nos gustaría incorporar una facilidad de "hoja de cálculo universal" en
  <TeXmacs>. La idea es que todas las dependencias entre las celdas en una
  hoja son analizadas por <TeXmacs>, pero todas las computaciones actuales
  son delegadas a un sistema externo de su elección, como alguno de los
  sistemas de álgebra computacional soportados actualmente. También, los
  datos en la hoja de cálculo no necesariamente están formateados en una
  tabla rectangular; uno puede también imaginar dependencias entre los nodos
  de un árbol, elementos de un grafo, o cualquier otra cosa.

  <section|Diagramas técnicos>

  Me gustaría también incluir una facilidad para dibujar diagramas técnicos.
  En esta implementación usted debería ser capaz de beneficiarse del hecho de
  que puede definir macros para hacer construcciones geométricas. Sería por
  ejemplo posible escribir un estilo para dibujar circuitos electrónicos o
  componentes químicos con una agradable barra de herramientas para
  seleccionar circuidos o componetes, justo como usted selecciona líneas y
  círculos en los dibujos usuales.

  <section|Interface con sistemas de álgebra computacional>

  Las siguientes mejoras deberían aún ser hechas a fin de enlazar
  <apply|TeXmacs> a sistemas de álgebra computacional:

  <\enumerate>
    <item>Mejorar el estrato de las sesiones de álgebra computacional.

    <item>Adicionar características extra para incrementar la
    interoperabilidad entre <apply|TeXmacs> y sistemas de álgebra
    computacional para dar un control adicional sobre el estrato de una
    salida grande.)

    <item>Más semántica para los objetos siendo comunicados. Esto bien puede
    ser información de alto nivel (como Openmath o el etiquetamiento
    matemático) o información de bajo nivel (incluyendo información sobre la
    representación de los datos), dependiendo de la velocidad requerida.

    <item>Posibilidades futuras para la evolución concerniente a relsaltado,
    facilidades de depuración y cosas por el estilo.
  </enumerate>

  <section|Interacción con otros proyectos estilo-GNU>

  Podría ser agradable incrementar la interacción entre <apply|TeXmacs> y
  otros proyectos estilo-GNU, tales como Gnome o GUI's multiplataforma. Esto
  podría facilitar la incorporación de datos externos dentro de documentos
  <apply|TeXmacs> o incrementar el número de plataformas soportadas. Por otro
  lado, varias características <apply|TeXmacs>, tales como manejo de fuentes,
  podrían ser interesantes para otros proyectos también.

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

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|toc-1|<tuple|1|?>>
    <associate|toc-2|<tuple|2|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|toc-3|<tuple|3|?>>
    <associate|toc-4|<tuple|4|?>>
    <associate|toc-5|<tuple|5|?>>
    <associate|toc-6|<tuple|6|?>>
    <associate|toc-7|<tuple|<uninit>|?>>
    <associate|toc-8|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|1<space|2spc>Composición
      tipográfica><value|toc-dots><pageref|toc-1><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|2<space|2spc>Facilidades extra para edición de
      textos><value|toc-dots><pageref|toc-2><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|3<space|2spc>Una hoja de cálculo
      universal><value|toc-dots><pageref|toc-3><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|4<space|2spc>Diagramas
      técnicos><value|toc-dots><pageref|toc-4><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|5<space|2spc>Interface con sistemas de álgebra
      computacional><value|toc-dots><pageref|toc-5><vspace|0.5fn>

      <vspace*|1fn><with|font series|<quote|bold>|math font
      series|<quote|bold>|6<space|2spc>Interacción con otros proyectos
      estilo-GNU><value|toc-dots><pageref|toc-6><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>
