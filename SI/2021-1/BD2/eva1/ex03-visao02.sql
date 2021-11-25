CREATE VIEW vw_empregados_horas_projetos AS
SELECT e.nome, e.ssn, e.datanasc, e.endereco, e.sexo,
       e.salario,
       sum(coalesce(r.horas, 0)) AS horas_projetos,
       count(p.pnumero) as num_projetos
  FROM empregado e
  JOIN trabalha_em r
    ON e.ssn = r.essn
  JOIN projeto p
    ON r.pno = p.pnumero
  GROUP BY e.ssn;
