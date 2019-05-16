#!/bin/bash

## Executa todos os casos de teste para um único arquivo.

./tp2-sort "$1" --bubble
./tp2-sort "$1" --selection
./tp2-sort "$1" --insertion
./tp2-sort "$1" --merge
./tp2-sort "$1" --quick
