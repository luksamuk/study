CREATE VIEW vw_clientes_saldo_total AS
SELECT c.cod_cliente, c.nome_cliente, c.rua_cliente,
       c.cidade_cliente,
       count(dt.numero_conta) AS num_contas,
       sum(ct.saldo) AS saldo_total
  FROM devedor d
  JOIN cliente c
    ON c.cod_cliente = d.cod_cliente
  JOIN depositante dt
    ON dt.cod_cliente = c.cod_cliente
  JOIN conta ct
    ON dt.numero_conta = ct.numero_conta
  GROUP BY c.cod_cliente;
