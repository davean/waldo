#!/bin/sh

mkdir panels
mkdir loadedPanels

rm panels/*.png

for f in highres/**/*.??g
do
    echo $f
    convert $f -resize 6% "panels/`basename $f`"
done
