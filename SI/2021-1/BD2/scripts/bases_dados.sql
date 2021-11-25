-- MySQL Administrator dump 1.4
--
-- ------------------------------------------------------
-- Server version	5.0.75-0ubuntu10.5


/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES UTF8MB4 */;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;


--
-- Create schema companhia
--

CREATE DATABASE IF NOT EXISTS companhia;
USE companhia;

--
-- Definition of table `companhia`.`departamento`
--

DROP TABLE IF EXISTS `companhia`.`departamento`;
CREATE TABLE  `companhia`.`departamento` (
  `dnome` varchar(20) NOT NULL,
  `dnumero` int(11) NOT NULL,
  `gerssn` int(11) NOT NULL,
  `gerdataini` date NOT NULL,
  PRIMARY KEY  (`dnumero`),
  KEY `gerssn` (`gerssn`),
  CONSTRAINT `departamento_ibfk_1` FOREIGN KEY (`gerssn`) REFERENCES `empregado` (`ssn`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`departamento`
--

/*!40000 ALTER TABLE `departamento` DISABLE KEYS */;
LOCK TABLES `departamento` WRITE;
INSERT INTO `companhia`.`departamento` VALUES  ('Sede administrativa',1,888665555,'1981-06-19'),
 ('Administração',4,987654321,'1995-01-01'),
 ('Pesquisa',5,333445555,'1988-05-22');
UNLOCK TABLES;
/*!40000 ALTER TABLE `departamento` ENABLE KEYS */;


--
-- Definition of table `companhia`.`dependente`
--

DROP TABLE IF EXISTS `companhia`.`dependente`;
CREATE TABLE  `companhia`.`dependente` (
  `essn` int(11) NOT NULL,
  `num_dependente` int(11) NOT NULL,
  `nome_dependente` varchar(30) NOT NULL,
  `sexo` char(1) NOT NULL,
  `datanasc` date NOT NULL,
  `parentesco` varchar(15) NOT NULL,
  PRIMARY KEY  (`essn`,`num_dependente`),
  CONSTRAINT `dependente_ibfk_1` FOREIGN KEY (`essn`) REFERENCES `empregado` (`ssn`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`dependente`
--

/*!40000 ALTER TABLE `dependente` DISABLE KEYS */;
LOCK TABLES `dependente` WRITE;
INSERT INTO `companhia`.`dependente` VALUES  (123456789,1,'Michael','M','1988-01-04','filho'),
 (123456789,2,'Alice','F','1988-12-30','filha'),
 (123456789,3,'Elizabeth','F','1967-05-05','cônjuge'),
 (333445555,1,'Alice','F','1986-04-05','filha'),
 (333445555,2,'Theodore','M','1983-10-25','filho'),
 (333445555,3,'Joy','F','1958-05-03','cônjuge'),
 (987654321,1,'Abner','M','1942-02-28','cônjuge');
UNLOCK TABLES;
/*!40000 ALTER TABLE `dependente` ENABLE KEYS */;


--
-- Definition of table `companhia`.`dept_localizacoes`
--

DROP TABLE IF EXISTS `companhia`.`dept_localizacoes`;
CREATE TABLE  `companhia`.`dept_localizacoes` (
  `dnumero` int(11) NOT NULL,
  `dlocalizacao` int(11) NOT NULL,
  `localizacao` varchar(20) NOT NULL,
  PRIMARY KEY  (`dnumero`,`dlocalizacao`),
  CONSTRAINT `dept_localizacoes_ibfk_1` FOREIGN KEY (`dnumero`) REFERENCES `departamento` (`dnumero`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`dept_localizacoes`
--

/*!40000 ALTER TABLE `dept_localizacoes` DISABLE KEYS */;
LOCK TABLES `dept_localizacoes` WRITE;
INSERT INTO `companhia`.`dept_localizacoes` VALUES  (1,1,'Houston'),
 (4,1,'Stafford'),
 (5,1,'Bellaire'),
 (5,2,'Sugarland'),
 (5,3,'Houston');
UNLOCK TABLES;
/*!40000 ALTER TABLE `dept_localizacoes` ENABLE KEYS */;


--
-- Definition of table `companhia`.`empregado`
--

DROP TABLE IF EXISTS `companhia`.`empregado`;
CREATE TABLE  `companhia`.`empregado` (
  `nome` varchar(30) NOT NULL,
  `ssn` int(11) NOT NULL,
  `datanasc` date NOT NULL,
  `endereco` varchar(50) NOT NULL,
  `sexo` char(1) NOT NULL,
  `salario` float NOT NULL,
  `superssn` int(11) default NULL,
  `dno` int(11) default NULL,
  PRIMARY KEY  (`ssn`),
  KEY `superssn` (`superssn`),
  KEY `dno` (`dno`),
  CONSTRAINT `empregado_ibfk_1` FOREIGN KEY (`superssn`) REFERENCES `empregado` (`ssn`),
  CONSTRAINT `empregado_ibfk_2` FOREIGN KEY (`dno`) REFERENCES `departamento` (`dnumero`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`empregado`
--

/*!40000 ALTER TABLE `empregado` DISABLE KEYS */;
LOCK TABLES `empregado` WRITE;
INSERT INTO `companhia`.`empregado` VALUES  ('John Smith',123456789,'1965-01-09','Rua Fondren 731, Houston, Texas','M',25000,333445555,5),
 ('Franklin Wong',333445555,'1955-12-08','Rua Voss 638, Houston, Texas','M',40000,888665555,5),
 ('Joyce English',453453453,'1972-07-31','Rua Rice 5631, Houston, Texas','F',25000,333445555,5),
 ('Ramesh Narayan',666884444,'1962-09-15','Rua Fire Oak 975, Humble, Texas','M',38000,333445555,5),
 ('James Borg',888665555,'1937-11-10','Rua Stone 450, Houston, Texas','M',55000,NULL,1),
 ('Jennifer Wallace',987654321,'1941-06-20','Rua Berry 291, Bellaire, Texas','F',43000,888665555,4),
 ('Ahmad Jabbar',987987987,'1969-03-29','Rua Dallas 980, Houston, Texas','M',25000,987654321,4),
 ('Alicia Zelaya',999887777,'1968-01-19','Rua Castle 3321, Spring, Texas','F',25000,987654321,4);
UNLOCK TABLES;
/*!40000 ALTER TABLE `empregado` ENABLE KEYS */;


--
-- Definition of table `companhia`.`projeto`
--

DROP TABLE IF EXISTS `companhia`.`projeto`;
CREATE TABLE  `companhia`.`projeto` (
  `pjnome` varchar(20) NOT NULL,
  `pnumero` int(11) NOT NULL,
  `plocalizacao` int(11) NOT NULL,
  `dnum` int(11) NOT NULL,
  PRIMARY KEY  (`pnumero`),
  KEY `dnum` (`dnum`,`plocalizacao`),
  CONSTRAINT `projeto_ibfk_1` FOREIGN KEY (`dnum`, `plocalizacao`) REFERENCES `dept_localizacoes` (`dnumero`, `dlocalizacao`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`projeto`
--

/*!40000 ALTER TABLE `projeto` DISABLE KEYS */;
LOCK TABLES `projeto` WRITE;
INSERT INTO `companhia`.`projeto` VALUES  ('Produto X',1,1,5),
 ('Produto Y',2,2,5),
 ('Produto Z',3,3,5),
 ('Automatização',10,1,4),
 ('Reorganização',20,1,1),
 ('Novos Benefícios',30,1,4);
UNLOCK TABLES;
/*!40000 ALTER TABLE `projeto` ENABLE KEYS */;


--
-- Definition of table `companhia`.`trabalha_em`
--

DROP TABLE IF EXISTS `companhia`.`trabalha_em`;
CREATE TABLE  `companhia`.`trabalha_em` (
  `essn` int(11) NOT NULL,
  `pno` int(11) NOT NULL,
  `horas` float default NULL,
  PRIMARY KEY  (`essn`,`pno`),
  KEY `pno` (`pno`),
  CONSTRAINT `trabalha_em_ibfk_1` FOREIGN KEY (`essn`) REFERENCES `empregado` (`ssn`),
  CONSTRAINT `trabalha_em_ibfk_2` FOREIGN KEY (`pno`) REFERENCES `projeto` (`pnumero`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `companhia`.`trabalha_em`
--

/*!40000 ALTER TABLE `trabalha_em` DISABLE KEYS */;
LOCK TABLES `trabalha_em` WRITE;
INSERT INTO `companhia`.`trabalha_em` VALUES  (123456789,1,32.5),
 (123456789,2,7.5),
 (333445555,2,10),
 (333445555,3,10),
 (333445555,10,10),
 (333445555,20,10),
 (453453453,1,20),
 (453453453,2,20),
 (666884444,3,40),
 (888665555,20,NULL),
 (987654321,20,15),
 (987654321,30,20),
 (987987987,10,35),
 (987987987,30,5),
 (999887777,10,10),
 (999887777,30,30);
UNLOCK TABLES;
/*!40000 ALTER TABLE `trabalha_em` ENABLE KEYS */;

--
-- Create schema concurso
--

CREATE DATABASE IF NOT EXISTS concurso;
USE concurso;

--
-- Definition of table `concurso`.`auditoria`
--

DROP TABLE IF EXISTS `concurso`.`auditoria`;
CREATE TABLE  `concurso`.`auditoria` (
  `cod_candidato` int(11) NOT NULL,
  `datahora` datetime NOT NULL,
  `valor_antigo` int(11) NOT NULL,
  `valor_novo` int(11) NOT NULL,
  PRIMARY KEY  (`cod_candidato`,`datahora`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`auditoria`
--

/*!40000 ALTER TABLE `auditoria` DISABLE KEYS */;
LOCK TABLES `auditoria` WRITE;
INSERT INTO `concurso`.`auditoria` VALUES  (2,'2010-10-16 17:52:18',5,4),
 (2,'2010-10-16 17:52:56',4,5);
UNLOCK TABLES;
/*!40000 ALTER TABLE `auditoria` ENABLE KEYS */;


--
-- Definition of table `concurso`.`candidato`
--

DROP TABLE IF EXISTS `concurso`.`candidato`;
CREATE TABLE  `concurso`.`candidato` (
  `cod_candidato` int(11) NOT NULL auto_increment,
  `nome` varchar(30) NOT NULL,
  `cpf` decimal(11,0) default NULL,
  `telefone` varchar(13) NOT NULL,
  `endereco` varchar(50) NOT NULL,
  `tempo_servico` float default '0',
  PRIMARY KEY  (`cod_candidato`)
) ENGINE=InnoDB AUTO_INCREMENT=25 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`candidato`
--

/*!40000 ALTER TABLE `candidato` DISABLE KEYS */;
LOCK TABLES `candidato` WRITE;
INSERT INTO `concurso`.`candidato` VALUES  (1,'Jose da Silva','98787887887','92321222','Rua José dos Santos, centro, Bambuí',10),
 (2,'Carlos Lacerda','98723487887','91331222','Rua João Ribeiro, centro, Bambuí',5),
 (3,'Juca Chaves','34387834587','91452322','Rua Cel. Bento José, 42, cerrado, Bambuí',0),
 (4,'Francelino Pereira de Souza','3498743455','91223333','Rua das Flores, 44, centro, Iguatama',3),
 (5,'Oscar Junior Moreira','76543356733','91338765','Rua Leão Silva Pinto, 57, Guarujá, Medeiros',3),
 (6,'Vantuir Galdino','44323422211','91223222','Rua Vincent Brown, 44, centro, Formiga',7),
 (7,'Maria Conceição Carvalho','22234322944','91223232','Rua Viramundo, 32, centro, Tapiraí',6),
 (8,'Joana Galvão Santos','33423498788','81232223','Rua dos ventos uivantes, 33, Pompéu, Divinópolis',15),
 (9,'Betânia Alencar Rocha','11143223444','81234455','Rua A, 23, apto 102, centro, Belo Horizonte',10),
 (10,'Juliana Siqueira Campos','76543344454','91234343','Rua Francisco Bento, 44, centro, Tapiraí',16),
 (11,'Wesley Snipes de Souza','22234323334','81223434','Avenida dos Andradas, 44, cerrado, Bambuí',4);
INSERT INTO `concurso`.`candidato` VALUES  (12,'Carlos dos Santos Souza','76543444544','82334323','Rua Aparecida Souza, Novo Oriente, Bambuí',6),
 (13,'Marcelo Santos Paiva Neto','44433345455','81234343','Rua Cabral Neto Filho, 44, Pampulha, Formiga',2),
 (14,'Karla Kelly dos Santos','33234432322','91334333','Rua José Siqueira, 44, centro, Bambui',1),
 (15,'Flávio Aparecido Rocha Santos','11187644433','93223343','Rua Santos Silva, 23, Tapiraí',0),
 (16,'Patrick Gorceix','11123232232','81231122','Rua Calvin Klein, 33, Mangueira, Formiga',15),
 (17,'Sandra Félix','22234312323','81232222','Rua Carijós, 33, Gutierrez, Belo Horizonte',2),
 (18,'Cândida Mendes Braga','12344454434','91332211','Rua José Mendes, 44, centro, Medeiros',3),
 (19,'Vagner Moura Silva','33343432343','81232232','Rua Cabral Filho, 74, apto 101, centro, Formiga',10),
 (20,'Fabio Resende Santos','12345634333','91232222','Rua Mourinho Neto, 33, centro, Formiga',6),
 (21,'jose','3499999999','34444444','rua a, 44',2),
 (23,'joao','3499999998','34444444','rua a, 44',2);
INSERT INTO `concurso`.`candidato` VALUES  (24,'Pedro','3498999995','91332508','rua a',4);
UNLOCK TABLES;
/*!40000 ALTER TABLE `candidato` ENABLE KEYS */;


--
-- Definition of table `concurso`.`disciplina`
--

DROP TABLE IF EXISTS `concurso`.`disciplina`;
CREATE TABLE  `concurso`.`disciplina` (
  `cod_disciplina` int(11) NOT NULL auto_increment,
  `descricao` varchar(20) default NULL,
  PRIMARY KEY  (`cod_disciplina`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`disciplina`
--

/*!40000 ALTER TABLE `disciplina` DISABLE KEYS */;
LOCK TABLES `disciplina` WRITE;
INSERT INTO `concurso`.`disciplina` VALUES  (1,'Matematica'),
 (2,'Português'),
 (3,'Física'),
 (4,'Química');
UNLOCK TABLES;
/*!40000 ALTER TABLE `disciplina` ENABLE KEYS */;


--
-- Definition of table `concurso`.`disciplina_formacao`
--

DROP TABLE IF EXISTS `concurso`.`disciplina_formacao`;
CREATE TABLE  `concurso`.`disciplina_formacao` (
  `cod_disciplina` int(11) NOT NULL,
  `cod_formacao` int(11) NOT NULL,
  PRIMARY KEY  (`cod_disciplina`,`cod_formacao`),
  KEY `cod_formacao` (`cod_formacao`),
  CONSTRAINT `disciplina_formacao_ibfk_1` FOREIGN KEY (`cod_disciplina`) REFERENCES `disciplina` (`cod_disciplina`),
  CONSTRAINT `disciplina_formacao_ibfk_2` FOREIGN KEY (`cod_formacao`) REFERENCES `formacao` (`cod_formacao`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`disciplina_formacao`
--

/*!40000 ALTER TABLE `disciplina_formacao` DISABLE KEYS */;
LOCK TABLES `disciplina_formacao` WRITE;
INSERT INTO `concurso`.`disciplina_formacao` VALUES  (1,1),
 (3,1),
 (1,2),
 (3,2),
 (1,3),
 (3,3),
 (1,4),
 (3,4),
 (2,5),
 (2,6),
 (2,7),
 (2,8),
 (1,9),
 (3,9),
 (1,10),
 (3,10),
 (1,11),
 (3,11),
 (1,12),
 (3,12),
 (1,13),
 (3,13),
 (1,14),
 (3,14),
 (1,15),
 (3,15),
 (1,16),
 (3,16),
 (4,17),
 (4,18),
 (4,19),
 (4,20);
UNLOCK TABLES;
/*!40000 ALTER TABLE `disciplina_formacao` ENABLE KEYS */;


--
-- Definition of table `concurso`.`formacao`
--

DROP TABLE IF EXISTS `concurso`.`formacao`;
CREATE TABLE  `concurso`.`formacao` (
  `cod_formacao` int(11) NOT NULL auto_increment,
  `descricao` varchar(30) default NULL,
  `cod_nivel` int(11) NOT NULL,
  PRIMARY KEY  (`cod_formacao`),
  KEY `cod_nivel` (`cod_nivel`),
  CONSTRAINT `formacao_ibfk_1` FOREIGN KEY (`cod_nivel`) REFERENCES `nivel_formacao` (`cod_nivel`)
) ENGINE=InnoDB AUTO_INCREMENT=21 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`formacao`
--

/*!40000 ALTER TABLE `formacao` DISABLE KEYS */;
LOCK TABLES `formacao` WRITE;
INSERT INTO `concurso`.`formacao` VALUES  (1,'Matematica',1),
 (2,'Matematica',2),
 (3,'Matematica',3),
 (4,'Matematica',4),
 (5,'Letras',1),
 (6,'Letras',2),
 (7,'Letras',3),
 (8,'Letras',4),
 (9,'Ciência da Computação',1),
 (10,'Ciência da Computação',2),
 (11,'Ciência da Computação',3),
 (12,'Ciência da Computação',4),
 (13,'Física',1),
 (14,'Física',2),
 (15,'Física',3),
 (16,'Física',4),
 (17,'Química',1),
 (18,'Química',2),
 (19,'Química',3),
 (20,'Química',4);
UNLOCK TABLES;
/*!40000 ALTER TABLE `formacao` ENABLE KEYS */;


--
-- Definition of table `concurso`.`inscricao`
--

DROP TABLE IF EXISTS `concurso`.`inscricao`;
CREATE TABLE  `concurso`.`inscricao` (
  `cod_inscricao` int(11) NOT NULL auto_increment,
  `data_inscricao` date NOT NULL,
  `tipo_inscricao` int(11) NOT NULL,
  `cod_candidato` int(11) NOT NULL,
  `cod_municipio` int(11) NOT NULL,
  `cod_formacao` int(11) NOT NULL,
  `cod_disciplina` int(11) NOT NULL,
  PRIMARY KEY  (`cod_inscricao`),
  KEY `tipo_inscricao` (`tipo_inscricao`),
  KEY `cod_candidato` (`cod_candidato`),
  KEY `cod_municipio` (`cod_municipio`),
  KEY `cod_formacao` (`cod_formacao`),
  KEY `cod_disciplina` (`cod_disciplina`),
  CONSTRAINT `inscricao_ibfk_1` FOREIGN KEY (`tipo_inscricao`) REFERENCES `tipo_inscricao` (`cod_tipo`),
  CONSTRAINT `inscricao_ibfk_2` FOREIGN KEY (`cod_candidato`) REFERENCES `candidato` (`cod_candidato`),
  CONSTRAINT `inscricao_ibfk_3` FOREIGN KEY (`cod_municipio`) REFERENCES `municipio` (`cod_municipio`),
  CONSTRAINT `inscricao_ibfk_4` FOREIGN KEY (`cod_formacao`) REFERENCES `formacao` (`cod_formacao`),
  CONSTRAINT `inscricao_ibfk_5` FOREIGN KEY (`cod_disciplina`) REFERENCES `disciplina` (`cod_disciplina`)
) ENGINE=InnoDB AUTO_INCREMENT=44 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`inscricao`
--

/*!40000 ALTER TABLE `inscricao` DISABLE KEYS */;
LOCK TABLES `inscricao` WRITE;
INSERT INTO `concurso`.`inscricao` VALUES  (1,'2008-10-01',1,1,1,3,1),
 (2,'2008-10-01',2,1,1,3,1),
 (3,'2008-10-01',1,2,1,3,1),
 (4,'2008-10-01',3,2,1,13,3),
 (5,'2008-10-01',4,3,1,6,2),
 (6,'2008-10-02',2,4,1,2,3),
 (7,'2008-10-02',1,4,1,17,4),
 (8,'2008-10-02',1,4,1,2,1),
 (9,'2008-10-03',4,5,1,8,2),
 (10,'2008-10-03',2,6,1,11,1),
 (11,'2008-10-03',2,6,2,11,1),
 (12,'2008-10-03',1,7,1,7,2),
 (13,'2008-10-03',1,7,1,13,3),
 (14,'2008-10-03',1,7,2,7,2),
 (15,'2008-10-04',1,8,2,1,1),
 (16,'2008-10-04',1,8,3,1,1),
 (17,'2008-10-04',2,9,3,9,1),
 (18,'2008-10-04',1,9,3,9,1),
 (19,'2008-10-05',2,10,2,9,1),
 (20,'2008-10-05',3,10,2,17,4),
 (21,'2008-10-06',1,11,3,1,1),
 (22,'2008-10-06',3,11,2,5,2),
 (23,'2008-10-06',3,12,2,17,4),
 (24,'2008-10-06',3,12,3,17,4),
 (25,'2008-10-07',4,13,1,20,4),
 (26,'2008-10-07',1,14,3,10,1),
 (27,'2008-10-07',1,14,3,17,4),
 (28,'2008-10-07',2,15,3,6,2),
 (29,'2008-10-08',1,15,3,10,3),
 (30,'2008-10-09',2,16,2,6,2),
 (31,'2008-10-09',2,16,3,6,2),
 (32,'2008-10-10',1,17,1,9,3),
 (33,'2008-10-10',1,17,2,9,3);
INSERT INTO `concurso`.`inscricao` VALUES  (34,'2008-10-10',1,17,3,9,1),
 (35,'2008-10-10',3,18,2,4,1),
 (36,'2008-10-10',1,18,2,4,1),
 (37,'2008-10-10',1,19,3,17,4),
 (38,'2008-10-10',1,19,2,17,4),
 (39,'2008-10-10',2,20,2,20,4),
 (40,'2008-10-10',3,20,2,13,3),
 (41,'2010-10-01',1,23,1,1,1),
 (42,'2010-08-10',1,24,1,1,1),
 (43,'2010-08-10',1,24,1,1,2);
UNLOCK TABLES;
/*!40000 ALTER TABLE `inscricao` ENABLE KEYS */;


--
-- Definition of table `concurso`.`municipio`
--

DROP TABLE IF EXISTS `concurso`.`municipio`;
CREATE TABLE  `concurso`.`municipio` (
  `cod_municipio` int(11) NOT NULL auto_increment,
  `descricao` varchar(20) NOT NULL,
  PRIMARY KEY  (`cod_municipio`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`municipio`
--

/*!40000 ALTER TABLE `municipio` DISABLE KEYS */;
LOCK TABLES `municipio` WRITE;
INSERT INTO `concurso`.`municipio` VALUES  (1,'Bambuí'),
 (2,'Iguatama'),
 (3,'Formiga');
UNLOCK TABLES;
/*!40000 ALTER TABLE `municipio` ENABLE KEYS */;


--
-- Definition of table `concurso`.`nivel_formacao`
--

DROP TABLE IF EXISTS `concurso`.`nivel_formacao`;
CREATE TABLE  `concurso`.`nivel_formacao` (
  `cod_nivel` int(11) NOT NULL auto_increment,
  `descricao` varchar(30) default NULL,
  `pontos` int(11) NOT NULL,
  PRIMARY KEY  (`cod_nivel`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`nivel_formacao`
--

/*!40000 ALTER TABLE `nivel_formacao` DISABLE KEYS */;
LOCK TABLES `nivel_formacao` WRITE;
INSERT INTO `concurso`.`nivel_formacao` VALUES  (1,'Graduacao',10),
 (2,'Pós-Graduação',13),
 (3,'Mestrado',25),
 (4,'Doutorado',45);
UNLOCK TABLES;
/*!40000 ALTER TABLE `nivel_formacao` ENABLE KEYS */;


--
-- Definition of table `concurso`.`tipo_inscricao`
--

DROP TABLE IF EXISTS `concurso`.`tipo_inscricao`;
CREATE TABLE  `concurso`.`tipo_inscricao` (
  `cod_tipo` int(11) NOT NULL auto_increment,
  `descricao` varchar(30) NOT NULL,
  `preco` float default NULL,
  PRIMARY KEY  (`cod_tipo`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=latin1;

--
-- Dumping data for table `concurso`.`tipo_inscricao`
--

/*!40000 ALTER TABLE `tipo_inscricao` DISABLE KEYS */;
LOCK TABLES `tipo_inscricao` WRITE;
INSERT INTO `concurso`.`tipo_inscricao` VALUES  (1,'Professor 8 horas',25),
 (2,'Professor 16 horas',35),
 (3,'Professor 20 horas',40),
 (4,'Professor Dedicação Exclusiva',65);
UNLOCK TABLES;
/*!40000 ALTER TABLE `tipo_inscricao` ENABLE KEYS */;

--
-- Create schema banco
--

CREATE DATABASE IF NOT EXISTS banco;
USE banco;

--
-- Definition of table `banco`.`agencia`
--

DROP TABLE IF EXISTS `banco`.`agencia`;
CREATE TABLE  `banco`.`agencia` (
  `cod_agencia` int(11) NOT NULL,
  `nome_agencia` char(15) NOT NULL,
  `cidade_agencia` char(30) default NULL,
  `fundos` int(11) default NULL,
  PRIMARY KEY  (`cod_agencia`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`agencia`
--

/*!40000 ALTER TABLE `agencia` DISABLE KEYS */;
LOCK TABLES `agencia` WRITE;
INSERT INTO `banco`.`agencia` VALUES  (1,'Downtown','Brooklyn',24390454),
 (2,'Redwood','Palo Alto',4948790),
 (3,'Perryridge','Horseneck',4006162),
 (4,'Mianus','Horseneck',1140578),
 (5,'Round Hill','Horseneck',18852528),
 (6,'Pownal','Bennington',777672),
 (7,'North Town','Rye',8719289),
 (8,'Brighton','Brooklyn',19241355);
UNLOCK TABLES;
/*!40000 ALTER TABLE `agencia` ENABLE KEYS */;


--
-- Definition of table `banco`.`cliente`
--

DROP TABLE IF EXISTS `banco`.`cliente`;
CREATE TABLE  `banco`.`cliente` (
  `cod_cliente` int(11) NOT NULL,
  `nome_cliente` char(20) NOT NULL,
  `rua_cliente` char(30) default NULL,
  `cidade_cliente` char(30) default NULL,
  PRIMARY KEY  (`cod_cliente`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`cliente`
--

/*!40000 ALTER TABLE `cliente` DISABLE KEYS */;
LOCK TABLES `cliente` WRITE;
INSERT INTO `banco`.`cliente` VALUES  (1,'Jones','Rua A','New York'),
 (2,'Smith','North','Rye'),
 (3,'Hayes','Main','Harrison'),
 (4,'Curry','North','Rye'),
 (5,'Lindsay','Park','Pittsfield'),
 (6,'Turner','Putnam','Stanford'),
 (7,'Williams','Nassau','Princeton'),
 (8,'Adams','Spring','Pittsfield'),
 (9,'Johnson','Alma','Palo Alto'),
 (10,'Glenn','Sand Hill','Woodside'),
 (11,'Brooks','Senator','Brooklyn'),
 (12,'Green','Walnut','Stanford');
UNLOCK TABLES;
/*!40000 ALTER TABLE `cliente` ENABLE KEYS */;


--
-- Definition of table `banco`.`conta`
--

DROP TABLE IF EXISTS `banco`.`conta`;
CREATE TABLE  `banco`.`conta` (
  `numero_conta` int(11) NOT NULL,
  `cod_agencia` int(11) NOT NULL,
  `saldo` float default NULL,
  PRIMARY KEY  (`numero_conta`,`cod_agencia`),
  KEY `cod_agencia` (`cod_agencia`),
  CONSTRAINT `conta_ibfk_1` FOREIGN KEY (`cod_agencia`) REFERENCES `agencia` (`cod_agencia`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`conta`
--

/*!40000 ALTER TABLE `conta` DISABLE KEYS */;
LOCK TABLES `conta` WRITE;
INSERT INTO `banco`.`conta` VALUES  (101,1,0),
 (101,2,0),
 (102,3,400),
 (201,4,450),
 (215,1,770),
 (217,7,900),
 (222,5,700),
 (222,6,850),
 (305,8,1500),
 (320,5,415),
 (330,4,1700),
 (400,6,780);
UNLOCK TABLES;
/*!40000 ALTER TABLE `conta` ENABLE KEYS */;


--
-- Definition of table `banco`.`depositante`
--

DROP TABLE IF EXISTS `banco`.`depositante`;
CREATE TABLE  `banco`.`depositante` (
  `cod_cliente` int(11) NOT NULL,
  `numero_conta` int(11) NOT NULL,
  `cod_agencia` int(11) NOT NULL,
  PRIMARY KEY  (`cod_cliente`,`numero_conta`,`cod_agencia`),
  KEY `numero_conta` (`numero_conta`,`cod_agencia`),
  CONSTRAINT `depositante_ibfk_1` FOREIGN KEY (`cod_cliente`) REFERENCES `cliente` (`cod_cliente`),
  CONSTRAINT `depositante_ibfk_2` FOREIGN KEY (`numero_conta`, `cod_agencia`) REFERENCES `conta` (`numero_conta`, `cod_agencia`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`depositante`
--

/*!40000 ALTER TABLE `depositante` DISABLE KEYS */;
LOCK TABLES `depositante` WRITE;
INSERT INTO `banco`.`depositante` VALUES  (1,101,1),
 (1,101,2),
 (3,102,3),
 (4,201,4),
 (2,215,1),
 (8,217,7),
 (5,222,5),
 (6,222,6),
 (7,222,6),
 (9,305,8),
 (10,320,5),
 (10,330,4),
 (10,400,6);
UNLOCK TABLES;
/*!40000 ALTER TABLE `depositante` ENABLE KEYS */;


--
-- Definition of table `banco`.`devedor`
--

DROP TABLE IF EXISTS `banco`.`devedor`;
CREATE TABLE  `banco`.`devedor` (
  `cod_cliente` int(11) NOT NULL,
  `numero_emprestimo` int(11) NOT NULL,
  `cod_agencia` int(11) NOT NULL,
  PRIMARY KEY  (`cod_cliente`,`numero_emprestimo`,`cod_agencia`),
  KEY `numero_emprestimo` (`numero_emprestimo`,`cod_agencia`),
  CONSTRAINT `devedor_ibfk_1` FOREIGN KEY (`cod_cliente`) REFERENCES `cliente` (`cod_cliente`),
  CONSTRAINT `devedor_ibfk_2` FOREIGN KEY (`numero_emprestimo`, `cod_agencia`) REFERENCES `emprestimo` (`numero_emprestimo`, `cod_agencia`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`devedor`
--

/*!40000 ALTER TABLE `devedor` DISABLE KEYS */;
LOCK TABLES `devedor` WRITE;
INSERT INTO `banco`.`devedor` VALUES  (2,11,4),
 (3,12,5),
 (3,14,1),
 (2,15,2),
 (1,17,1),
 (5,17,6),
 (7,17,7),
 (6,18,3),
 (9,19,2),
 (1,23,2),
 (7,23,3),
 (4,93,3);
UNLOCK TABLES;
/*!40000 ALTER TABLE `devedor` ENABLE KEYS */;


--
-- Definition of table `banco`.`emprestimo`
--

DROP TABLE IF EXISTS `banco`.`emprestimo`;
CREATE TABLE  `banco`.`emprestimo` (
  `numero_emprestimo` int(11) NOT NULL,
  `cod_agencia` int(11) NOT NULL,
  `total` double default NULL,
  PRIMARY KEY  (`numero_emprestimo`,`cod_agencia`),
  KEY `cod_agencia` (`cod_agencia`),
  CONSTRAINT `emprestimo_ibfk_1` FOREIGN KEY (`cod_agencia`) REFERENCES `agencia` (`cod_agencia`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1;

--
-- Dumping data for table `banco`.`emprestimo`
--

/*!40000 ALTER TABLE `emprestimo` DISABLE KEYS */;
LOCK TABLES `emprestimo` WRITE;
INSERT INTO `banco`.`emprestimo` VALUES  (11,4,900),
 (12,5,700),
 (14,1,1500),
 (15,2,1500),
 (17,1,1000),
 (17,6,800),
 (17,7,400),
 (18,3,400),
 (19,2,2000),
 (23,2,2000),
 (23,3,700),
 (93,3,500);
UNLOCK TABLES;
/*!40000 ALTER TABLE `emprestimo` ENABLE KEYS */;




/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
