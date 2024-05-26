#!/bin/bash
exec docker run -it --rm \
       -e DISPLAY=${DISPLAY} \
       -v /tmp/.X11-unix:/tmp/.X11-unix \
       -v ~/.Xauthority:/root/.Xauthority \
       -v $(pwd):/source \
       -w /source \
       --net=host \
       luksamuk/psxtoolchain:latest \
       timedit
