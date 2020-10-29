#!/bin/bash
filename=$1
dot -Kdot -Tpng $1 >"${filename%.*}.png"

