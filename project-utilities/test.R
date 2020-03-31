#!/usr/bin/env Rscript

library(tidyverse)

args = commandArgs(trailingOnly=TRUE)

print("args")
print(args)

getCurrentFileLocation <-  function()
{
    this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
    if (length(this_file)==0)
    {
      this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
}

this_file = getCurrentFileLocation()
print("this_file")
print(this_file)


print(sprintf("getwd() %s", getwd()))

sourceDir = getSrcDirectory(function(dummy) {dummy})
print(sprintf("sourceDir %s", sourceDir))

f = function(){getwd()}
print(sprintf("f %s", f()))

print(system.file('test.R'))

print('which')
print(Sys.which('test.R'))
print(system('which test.R', intern=T))


#source('./analysis_metadata.R')
#print(calculated_exp_vars)
