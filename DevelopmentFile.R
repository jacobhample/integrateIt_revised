library(devtools)
library(roxygen2)

setwd("~/Desktop/integrateItRevised")
#package.skeleton("integrateItRevised")

rm(list=ls())

current.code <- as.package("integrateItRevised")
load_all(current.code)
document(current.code)

build(current.code, path="~/Desktop/integrateItRevised")
install.packages("~/Desktop/integrateItRevised/integrateItRevised_1.0.tar.gz", repos = NULL, type="source")

library(integrateItRevised)
