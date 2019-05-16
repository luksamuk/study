#!/bin/bash

# Percorre a lista de arquivos em ordem reversa
for file in `ls testes/*.txt | sort -V`; do
    echo "Testando \"$file\":";
    echo "";
    ./exec-arquivo.sh "$file";
    echo " --------------------- ";
done
