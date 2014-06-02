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
# if pandoc is present, we will generate various documentation formats
if hash pandoc 2>/dev/null; then
    ###
    # readme
    pandoc -s -t latex -f markdown -o ./doc/tex/readme.tex README.md

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
else
    echo "Install pandoc to generate documentation."
    exit 0
fi

###
# if pdflatex is present, we will generate PDFs from the TeX sources.
if hash pdflatex 2>/dev/null; then
    for f in $(ls doc/tex/*.tex); do
        filename=`basename $f`

        pdflatex --output-directory=./doc/pdf ./doc/tex/$filename
    done

    rm doc/pdf/*.{aux,log,out}
else
    echo "Install pdflatex to convert TeX documentation sources to PDF."
fi
