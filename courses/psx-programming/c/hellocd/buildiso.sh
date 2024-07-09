#!/bin/bash

exec docker run --rm \
     -v $(pwd):/source \
     -w /source \
     luksamuk/psxtoolchain:latest \
     "make && mkpsxiso -y CDLAYOUT.xml"
