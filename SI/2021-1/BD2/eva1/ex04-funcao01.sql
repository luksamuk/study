DELIMITER &
CREATE FUNCTION f_rentabilidade(municipio varchar(20)) RETURNS varchar(12)
BEGIN
  DECLARE valor float;

  SELECT sum(ti.preco) INTO valor
    FROM municipio m
    JOIN inscricao i
      ON i.cod_municipio = m.cod_municipio
    JOIN tipo_inscricao ti
      ON i.tipo_inscricao = ti.cod_tipo
   WHERE m.descricao = municipio;

  CASE
    WHEN (valor <= 300)
         THEN RETURN 'BAIXO';     
    WHEN (valor > 300 AND valor < 500)
         THEN RETURN 'MEDIO';
    WHEN (valor >= 500)
         THEN RETURN 'ALTO';
    ELSE RETURN 'DESCONHECIDO';
  END CASE;
END &
DELIMITER ;
