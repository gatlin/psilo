#!/bin/bash

###
# Generate the documentation from sources

mkdir -p tex
mkdir -p html
mkdir -p pdf

pdfoutputdir="./pdf"

###
# if pandoc is present, we will generate various documentation formats
if hash pandoc 2>/dev/null; then
    ###
    # readme
    pandoc -s -t latex -f markdown -o ./tex/readme.tex ../README.md
    pandoc -s -S -t html+header_attributes -f markdown \
    --template=templates/main.tmpl.html --toc \
    -o ./html/src/index.html ../README.md

    ###
    # src dir
    for f in $(ls ../src/*.lhs); do
        filename=`basename $f`
        extension="${filename##*.}"
        filename="${filename%.*}"
        out=$(echo $filename | tr '[:upper:]' '[:lower:]')
        pandoc -s -t latex -f markdown+lhs -o ./tex/$out.tex $f
        pandoc -s -t html5 -f markdown+lhs+yaml_metadata_block \
        --template=templates/src.tmpl.html \
        -o ./html/src/$out.html $f
    done
else
    echo "Install pandoc to generate documentation."
    exit 0
fi

###
# if pdflatex is present, we will generate PDFs from the TeX sources.
if hash pdflatex 2>/dev/null; then

    for f in $(ls tex/*.tex); do
        filename=`basename $f`
        pdflatex --output-directory=$pdfoutputdir ./tex/$filename
    done

    rm pdf/*.{aux,log,out}
else
    echo "Install pdflatex to convert TeX documentation sources to PDF."
fi
