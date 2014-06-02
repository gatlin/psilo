#!/bin/bash

###
# Generate the documentation from sources

if [ ! -d doc ]; then
    mkdir doc
    mkdir -p doc/tex
    mkdir -p doc/pdf
#    mkdir -p doc/html
fi


###
# readme
pandoc -s -t latex -f markdown -o ./doc/tex/readme.tex

###
# src dir
for f in $(ls src/*.lhs); do
    filename=`basename $f`
    extension="${filename##*.}"
    filename="${filename%.*}"
    lowername=$(echo $filename | tr '[:upper:]' '[:lower:]')
    out="$lowername.tex"
    pandoc -s -t latex  -f markdown+lhs -o ./doc/tex/$out $f
done
