#!/bin/bash

## Author: Mark Klick

while [[ $# -gt 0 ]]
do
    key="$1"

    case $key in
        -i)
            INDIR="$2"
            shift;;
	-o)
	    OUTDIR="$2"
	    shift;;
        -h|--help)
            printf "\nUSAGE: rearange_fastq_files.sh -i [input dir] -o [output dir]\n"
            exit;;
        *)
            printf "\nERROR: Invalid script usage.\n"
            rearange_fastq_files.sh -h
            exit 1;;
    esac
    shift
done

# Make sure output directory exists
if [ ! -d "$OUTDIR" ]
then
    mkdir -p "$OUTDIR"
    #OUTDIR=$(readlink -f "$OUTDIR")
else
    #OUTDIR=$(readlink -f "$OUTDIR")
    echo "output directory exists"
fi

#for f in $(ls "$INDIR"/*.fastq); do
	#mv $f $OUTDIR
	#echo "$f"
#done

for f in `find $INDIR -name *.fastq`; do
	#echo $f
	mv $f $OUTDIR
done

echo "done rearanging"
	
