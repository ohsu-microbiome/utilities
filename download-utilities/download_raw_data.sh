#!/usr/bin/bash

# Title:
#    Download Raw Data for MiSeq Run 483
# Filename:
#    download_raw_data.sh
# Description:
#    Code to download the raw data for this sequencing run and associated
#    FastQC results.
# Usage:
#    $ bash download_raw_data.sh
# Author:
#    Mark Klick adpated from Lisa Karstens/Eric Leung
# Created:
#    2018-02-15
# Last Modified:
#    2018-09-28


while [[ $# -gt 0 ]]
do
	key="$1"

	case $key in
		-i)
			DIR="$2"
			shift;;
		-f)
			FaDIR="$2"
			shift;;
		-q)
                        QaDIR="$2"
                        shift;;
		-h)
			printf "\USAGE: download_raw_data.sh -i [input directory] -f [fastq files url] -q [quality files url] \n"
			exit;;
		*)
			printf "\nERROR: Invalid script usage.\n"
			download_raw_data.sh -h
			exit 1;;
	esac
	shift
done	

if [ ! -d "$DIR"]
then			
	DIR=$PWD
else
	echo "using $DIR as directory"
fi

# Download raw data files with wget ----------
# make a directory called fastq_files
mkdir "$DIR"/data/raw

wget \
  -r \
  -e \
  robots=off \
  -A gz,csv,html,php \
  -t 7 \
  -w 5 \
  --waitretry=14 \
  -P "$DIR"/data/raw/ \
  "$FaDIR"

echo "finished downloading raw fastq files"

# Download quality control FastQC data files with wget ----------
mkdir "$DIR"/data/quality
wget \
  -r \
  -e \
  robots=off \
  -A gz,csv,html,txt,php \
  -t 7 \
  -w 5 \
  --waitretry=14 \
  -P "$DIR"/data/quality/ \
  "$QaDIR"

echo "finished downloading quality files"
