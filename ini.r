
#####################################################################

#### Use this file to run cmgo from scripts (not within library) ####
# this file helps to load all R files and the demo data workspaces

setwd("D:/") # path to cmgo files

# load libraries
library(zoo)
library(spatstat)
library(stringr)
library(sp)


# source functions and load data
for(function.file in list.files("./R"))    source(paste("./R",  function.file, sep="/"))
for(data.file     in list.files("./data")) load(paste("./data", data.file, sep="/"))



#####################################################################