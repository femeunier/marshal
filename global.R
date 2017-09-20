##################################################################
###### Load libraries
##################################################################

# packages <- c("ggplot2", "plyr", "plotly", "readr")
# for(p in packages){
#   if (!require(p,character.only = TRUE)){
#     install.packages(p, dep=TRUE)
#     if(!require(p,character.only = TRUE)) stop("Package not found")
#   }
# }


library("tidyverse")
library("plyr")
library("readr")
library("data.table")
library(Matrix)
source("www/getSUF.R")  
library(plotly)

# Different color scales for the different plots
cscale <- c("#ffffcc", "#a1dab4","#41b6c4", "#2c7fb8", "#253494")
cscale1 <- c("#b30000","#e34a33","#fc8d59","#fdcc8a")
cscale2 <- c("#f1eef6","#d7b5d8","#df65b0","#dd1c77","#980043")
cscale2 <- c("#a21d4d","lightgrey","#2a3e98")
cscale3 <- c("#a1dab4","#41b6c4", "#2c7fb8", "#253494")

psicollar_base <- -15000

cond_range <- data.frame(value=c(1e-4, 1e-8, 1e1, 1e-5), type=c("kr", "kr", "kx", "kx"))

