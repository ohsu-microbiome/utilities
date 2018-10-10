#!/bin/bash

### https://raw.githubusercontent.com/abalter/microbiome-16s/master/create-dada2_conda_env.sh

env_name=$1

env_name=${env_name:=dada2}

conda_home=$(which conda | sed 's/\/bin.*$//')
echo "conda home: $conda_home"
conda_sh="$conda_home/etc/profile.d/conda.sh"
echo "activating $conda_sh"
conda_env_dir="$conda_home/envs"

source "$conda_sh"

if [[ `conda env list | grep "$env_name"` != '' ]]; then
  echo "dada2 environment $env_name exists -- removing"
  conda env remove -n $env_name -y
else
  echo "no dada2 environment $env_name"
fi

echo "creating dada2 environment"
cmd="conda create -n $env_name -y"
echo "executing $cmd"
$cmd

echo "
channels:
  - conda-forge
  - bioconda
  - defaults
" 

echo "
channels:
  - conda-forge
  - bioconda
  - defaults
" > "$conda_env_dir/$env_name/.condarc"


echo "activating $env_name"
cmd="conda activate $env_name"
$cmd

echo "updating environment"
cmd="conda update --all -y"
echo "executing $cmd"
$cmd

dada2_pkg_list="
r-knitr
r-rmarkdown
r-ggplot2
r-gridextra
r-nlme
r-data.table
r-dplyr
krb5
curl
libssh2
zlib
bioconductor-dada2
bioconductor-biocinstaller
"

downstream_pkg_list="
bioconductor-phyloseq
r-phangorn
r-structssi
r-caret 
r-e1071
r-devtools
r-shiny
r-miniui
r-randomforest
r-dplyr
r-ggrepel
r-nlme
r-reshape2
r-pma
r-scales
bioconductor-genefilter
bioconductor-deseq2
r-data.table
r-httr
r-jsonlite
r-mockery
r-praise
r-rstudioapi
r-testthat
r-whisker
"

echo "installing dada2 pkgs: `echo $dada2_pkg_list | sed 's/ /\n  \* /g; s/^/\n  * /'`"
#echo "installing dada2 pkgs: `echo $dada2_pkg_list | sed 's/^/  * /'`"
cmd="conda install $dada2_pkg_list -y"
echo "executing $cmd"
$cmd

cmd="conda install krb5 -y"
echo "executing $cmd"
$cmd

echo "installing downstream pkgs: `echo $downstream_pkg_list | sed 's/ /\n  \* /g; s/^/\n  * /'`"
#echo "installing downstream pkgs: `echo $downstream_pkg_list | sed 's/^/  * /'`"
cmd="conda install $downstream_pkg_list -y"
echo "executing $cmd"
$cmd

echo "Installing items needed that are not in conda"

cmd="Rscript -e \"install.packages('ggnetwork', lib='~/R', repos='http://cran.us.r-project.org');\""
echo "executing $cmd"
$cmd
cmd="Rscript -e \"install.packages('intergraph', lib='~/R', repos='http://cran.us.r-project.org');\""
echo "executing $cmd"
$cmd
cmd="Rscript -e \"source('https://raw.githubusercontent.com/cran/phyloseqGraphTest/master/R/graphtest-functions.R')\""
echo "executing $cmd"
$cmd
cmd="Rscript -e \"source('https://raw.githubusercontent.com/cran/phyloseqGraphTest/master/R/package-doc.R')\""
echo "executing $cmd"
$cmd


exit 0
