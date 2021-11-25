SELECT c.nome_cliente, e.total
  FROM cliente c
  JOIN devedor d
    ON c.cod_cliente = d.cod_cliente
  JOIN emprestimo e
    ON d.numero_emprestimo = e.numero_emprestimo
 WHERE c.cod_cliente IN
       (SELECT cod_cliente
          FROM devedor d
          JOIN emprestimo e
                ON d.numero_emprestimo = e.numero_emprestimo
         WHERE total > 1000
           AND d.cod_agencia = 1);
