SELECT *
  FROM empregado e
 WHERE e.ssn IN
       (SELECT r.essn
          FROM projeto p
          JOIN trabalha_em r
                ON r.pno = p.pnumero
         WHERE p.pjnome LIKE 'Produto%'
           AND r.horas > 10)
