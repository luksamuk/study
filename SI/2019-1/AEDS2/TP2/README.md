# TP2

## Introdução

Esta é uma implementação do Trabalho Prático 2 de Algoritmos e Estruturas de
Dados II, com o tema de métodos de ordenação.

## Compilação

Para compilar o programa em questão, basta utilizar o arquivo `Makefile` incluso
no projeto. Este `Makefile` só funcionará em sistemas operacionais baseados
em Unix.

Para compilar usando o `Makefile`, basta navegar até a pasta do projeto e
invocar o programa `make`:

```bash
    cd /path/to/project
    make
```

Isto gerará o binário `tp2-sort`, que poderá ser utilizado para testes.

## Utilização

Este projeto possui três métodos de utilização, sendo dois deles através de
scripts Bash que automatizam algumas tarefas, e o terceiro é a utilização do
programa gerado em si.

Adicionalmente, há dois subdiretórios de relevância: um chamado `testes/`, outro
chamado `simple/`. Ambos os diretórios possuem arquivos populados com números.

### Scripts

O projeto é acompanhado de dois scripts, escritos para Bash Shell.

O primeiro é `exec-arquivo.sh`. Este script executa o programa `tp2-sort` para
um único arquivo, que tenha sido fornecido via linha de comando para o script.

```bash
    ./exec-arquivo.sh nome-do-arquivo.txt
```

O segundo script é `exec-testes.sh`. Este script executa o script
`exec-arquivo.sh` para todos os arquivos no diretório `testes/`.

```bash
    ./exec-testes.sh
```

### Executável

O programa gerado, que opera sob ambos os scripts, foi construído sob a
filosofia Unix e, portanto, é responsável por processar apenas um arquivo, sob
um único método de ordenação. Ambas as informações são repassadas via linha de
comando.

Para maiores informações, execute o programa com a flag de ajuda:

```bash
    ./tp2-sort --help
```


## Licenciamento

Este programa é de código aberto, distribuído sob a Licença MIT. Para maiores
informações, consulte o arquivo LICENSE.

