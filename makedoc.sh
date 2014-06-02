#!/bin/bash

###
# Generate the documentation from sources

if [ ! -d doc ]; then
    mkdir doc
    mkdir -p doc/tex
#    mkdir -p doc/html
fi

for f in $(ls src/*.lhs); do
    filename=`basename $f`
    extension="${filename##*.}"
    filename="${filename%.*}"
    lowername=$(echo $filename | tr '[:upper:]' '[:lower:]')

    pandoc -f markdown+lhs -o ./doc/tex/$lowername.tex -i $f
done
