CREATE VIEW vw_inscricoes_municipio AS
SELECT m.*, count(i.cod_inscricao) AS num_inscricoes
  FROM municipio m
  JOIN inscricao i
    ON m.cod_municipio = i.cod_municipio
  GROUP BY m.cod_municipio;
