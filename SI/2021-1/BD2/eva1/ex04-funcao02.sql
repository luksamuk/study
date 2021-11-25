DELIMITER &
CREATE FUNCTION f_horasempregadas(proj varchar(20)) RETURNS float
BEGIN
        DECLARE t_horas float;

        SELECT sum(horas) INTO t_horas
          FROM projeto p
          JOIN trabalha_em r
            ON r.pno = p.pnumero
         WHERE p.pjnome = proj;

    RETURN t_horas;
END &
DELIMITER ;
