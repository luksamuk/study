#!/bin/bash

# Run this script within the toolchain image
echo "Building application..."
make

echo "Generating image file..."
mkpsxiso -y project.xml

