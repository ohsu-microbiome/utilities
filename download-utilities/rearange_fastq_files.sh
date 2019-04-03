#!/bin/bash

## Author: Mark Klick
## Purpose: Rearange .fastq or .fastq.gz files in a specified input directory and move to a specified output 
## directory. Optional -r RECURSIVE argument uses "find" to recursively search an entire direcotry tree for any 
## .fastq or .fastq.gz files.

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
	-r)
	    RECURSIVE="$2"
	    shift;;
        -h|--help)
            printf "\nUSAGE: rearange_fastq_files.sh -i [input dir] -o [output dir] -r [search directory recursively(default looks only in specified -o output directory)\n"
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

# Check for the -r flag to either search recursively or in a specific directory
if [ -z "$RECURSIVE" ]; then
        for f in $(ls "$INDIR"/*.fastq*); do
        	mv $f $OUTDIR
        	echo "$f"
	done
else
        for f in `find $INDIR -name *.fastq*`; do
        	echo $f
        	mv $f $OUTDIR
	done
fi

echo "done rearanging"
	
