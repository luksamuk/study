DELIMITER &
CREATE PROCEDURE sp_num_insc_por_mun(out num_insc int, mun_desc VARCHAR(20))
BEGIN
        SELECT count(*) into num_insc
          FROM inscricao i
          JOIN municipio m
            ON i.cod_municipio = m.cod_municipio
           AND m.descricao = mun_desc;
END &
DELIMITER ;
