DELIMITER &
CREATE PROCEDURE sp_lista_inscricoes_candidatos()
BEGIN
        SELECT i.cod_inscricao AS codigo,
               m.descricao AS municipio_nome,
               c.nome, c.cpf, c.telefone, c.endereco
          FROM inscricao i
          JOIN candidato c
            ON i.cod_candidato = c.cod_candidato
          JOIN municipio m
            ON i.cod_municipio = m.cod_municipio;
END &
DELIMITER ;
