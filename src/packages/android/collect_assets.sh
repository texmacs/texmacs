#!/bin/sh
mkdir -p $2
i=$(find "$2" -name "*.raw" -type f | wc -l | tr -d '[:space:]')
i=$(((i - 1) + 1))
# rm -f "$2/resource_files.txt"
find $1 -type f | while read filename; do 
    md5=$(md5sum "$filename" | cut -d ' ' -f 1)
    echo "r$i $filename $md5sum" >> "$2/resource_files.txt"
    cp "$filename" "$2/r$i.raw"
    i=$((i + 1))
done
IFS="$OIFS"