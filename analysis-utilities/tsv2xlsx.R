#!/usr/bin/env Rscript

library(openxlsx)

args = commandArgs(trailingOnly=TRUE)

filename = normalizePath(args[1])
print(sprintf('input file: %s', filename))

base_name = basename(filename)
dir_name = dirname(filename)
print(sprintf('base name: %s', base_name))
print(sprintf('dir name: %s', dir_name))

new_filename = file.path(
  dir_name,
  gsub('(.*\\.)[^\\.]+', '\\1xlsx', base_name)
)
print(sprintf('output file: %s', new_filename))

data_in = read.delim(
  file=args[1],
  sep='\t',
  stringsAsFactors=F
)

#print(head(data_in))

write.xlsx(
  data_in,
  file=new_filename,
  col.names=T
)

