<TeXmacs|1.0.1>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Insertar imágenes>

  Puede incluir imágenes en el texto usando el menú
  <apply|menu|Insert|Image>. Actualmente <TeXmacs> reconoce los formatos
  <verbatim|ps>, <verbatim|eps>, <verbatim|tif>, <verbatim|pdf>,
  <verbatim|pdm>, <verbatim|gif>, <verbatim|ppm>, <verbatim|xpm> y
  <verbatim|fig>. Para ver las imágenes Postscript, <TeXmacs>
  utiliza<verbatim| gs> (ghostscript). Si no está instalado en su sistema,
  puede descargarlo desde\ 

  <\verbatim>
    \ \ \ \ www.cs.wisc.edu/~ghost/index.html
  </verbatim>

  De hecho, los otros formatos de fichero se traducen a Postscrip a través de
  los scripts <verbatim|tiff2ps>, <verbatim|pdf2ps>, <verbatim|pnmtops>,
  <verbatim|giftopnm>, <verbatim|ppmtogif>, <verbatim|xpmtoppm>. Si no están
  disponibles en su sistema, pregunte al administrador del sistema.

  Por defecto las imágenes se muestran al tamaño que fueron diseñadas. Las
  imágenes admiten las siguientes operaciones:

  <\itemize>
    <item>Recortar según un rectángulo. Se toma la esquina inferior izquierda
    de la imagen por defecto como origen para especificar un rectángulo que
    sirva de máscara para el recorte.

    <item>Redimensionar. Cuando se especifica una sola de las dimensiones
    (ancho y no alto o vice versa) la imagen cambia de tamaño manteniendo la
    proporción inicial.

    <item>Ampliar. Una forma alternativa de redimensionar, multiplicando
    altura y anchura por el mismo factor constante.
  </itemize>

  <TeXmacs> también incluye un <em|script> para convertir figuras,
  opcionalmente con fórmulas <LaTeX> incluídas en Postscript encapsulado.
  Para incluir una fórmula <LaTeX> en una figura de <verbatim|xfig> hay que
  introducir la fórmula como texto, seleccionando una fuente <LaTeX>, y fijar
  el indicador especial en los indicadores de texto.

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
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|ss>|Insertar>|<with|font
      family|<quote|ss>|Imagen>>|<pageref|idx-1>>
    </associate>
  </collection>
</auxiliary>
