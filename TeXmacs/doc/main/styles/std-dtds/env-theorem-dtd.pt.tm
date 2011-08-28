<TeXmacs|1.0.1.20>

<style|tmdoc>

<\body>
  <expand|tmdoc-title|Ambientes para teoremas e afins>

  A <abbr|d.t.d.> <tmdtd|env-theorem> fornece marcação para a diagramação de
  teoremas e similares. As mais importantes etiquetas são:

  <\description>
    <expand|item*|<markup|theorem*>>Um macro para formatar ambientes
    similares a teoremas. O primeiro argumento especifica o nome do teorema,
    algo como ``Teorema 1.2'' e o segund argumento contém o corpo do teorema.
    Este ambiente é usado para teoremas definidos por <markup|newtheorem>.

    <expand|item*|<markup|remark*>>Semelhante a <markup|theorem*>, mas usado
    para ambientes similares a comentários.

    <expand|item*|<markup|exercise*>>Semelhante a <markup|theorem*>, mas para
    ambientes semelhates a exercícios.

    <expand|item*|<markup|proof*>>Semelhante a <markup|theorem*>, mas para
    provas. Este ambiente é usado principalmente para alterar o nome da
    prova, como em ``Fim da prova do teorema 1.2''

    <expand|item*|<markup|dueto>>Um ambiente para especificar os criadores de
    um teorema.

    <expand|item*|<markup|corollary*>>Para corolários não numerados. Este
    ambiente é baseado em <markup|theorem*>.

    <expand|item*|<markup|proof>>Para provas de teoremas. Este ambiente é
    baseado em <markup|proof*>.
  </description>

  As etiquetas seguintes podem ser usadas para modificação dos ambientes.

  <\description>
    <expand|item*|<markup|theoremname>>Um macro que controla a aparência dos
    nomes dos ambiente para teoremas <em|e> comentários. A maioria dos
    estilos usa negrito ou maiúsculas pequenas.

    <expand|item*|<markup|exercisename>>Semelhante a <markup|theoremname>,
    mas para exercícios.

    <expand|item*|<markup|theoremsep>>O separador entre o nome do teorema ou
    similar e seu corpo. Em geral, um ponto seguido de um espaço.

    <expand|item*|<markup|exercisesep>>Semelhante a <markup|theoremsep>, mas
    para exercícios.
  </description>

  <apply|tmdoc-copyright|1998--2003|Joris van der Hoeven|Ramiro Brito
  Willmersdorf>

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
    <associate|language|portuguese>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|idx-1|<tuple|<uninit>|?>>
    <associate|idx-2|<tuple|<uninit>|?>>
    <associate|idx-3|<tuple|<uninit>|?>>
    <associate|idx-4|<tuple|<uninit>|?>>
    <associate|idx-5|<tuple|<uninit>|?>>
    <associate|idx-6|<tuple|<uninit>|?>>
    <associate|idx-7|<tuple|<uninit>|?>>
    <associate|idx-8|<tuple|<uninit>|?>>
    <associate|idx-9|<tuple|<uninit>|?>>
    <associate|idx-20|<tuple|<uninit>|?>>
    <associate|idx-10|<tuple|<uninit>|?>>
    <associate|idx-11|<tuple|<uninit>|?>>
    <associate|idx-12|<tuple|<uninit>|?>>
    <associate|idx-13|<tuple|<uninit>|?>>
    <associate|idx-14|<tuple|<uninit>|?>>
    <associate|idx-15|<tuple|<uninit>|?>>
    <associate|idx-16|<tuple|<uninit>|?>>
    <associate|idx-17|<tuple|<uninit>|?>>
    <associate|idx-18|<tuple|<uninit>|?>>
    <associate|idx-19|<tuple|<uninit>|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      magenta>|env-theorem>>|<pageref|idx-1>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-2>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|newtheorem>>|<pageref|idx-3>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|remark*>>|<pageref|idx-4>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-5>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercise*>>|<pageref|idx-6>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-7>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-8>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-9>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|dueto>>|<pageref|idx-10>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|corollary*>>|<pageref|idx-11>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theorem*>>|<pageref|idx-12>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof>>|<pageref|idx-13>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|proof*>>|<pageref|idx-14>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-15>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisename>>|<pageref|idx-16>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremname>>|<pageref|idx-17>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-18>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|exercisesep>>|<pageref|idx-19>>

      <tuple|<tuple|<with|font family|<quote|tt>|color|<quote|dark
      green>|theoremsep>>|<pageref|idx-20>>
    </associate>
  </collection>
</auxiliary>
