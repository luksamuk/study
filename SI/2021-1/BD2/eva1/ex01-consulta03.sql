SELECT DISTINCT c.*
  FROM inscricao i
  JOIN candidato c
    ON i.cod_candidato = c.cod_candidato
  JOIN tipo_inscricao ti
    ON i.tipo_inscricao = ti.cod_tipo
 WHERE ti.preco IN
       (SELECT DISTINCT ti.preco
          FROM inscricao i
          JOIN tipo_inscricao ti
                ON i.tipo_inscricao = ti.cod_tipo
          JOIN municipio m
                ON i.cod_municipio = m.cod_municipio
         WHERE m.descricao = 'Formiga'
           AND ti.preco > 25)
