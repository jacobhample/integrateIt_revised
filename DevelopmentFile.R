library(devtools)
library(roxygen2)

setwd("~/Desktop/integrateItRevised")
#package.skeleton("integrateItRevised")

rm(list=ls())

current.code <- as.package("integrateItRevised")
load_all(current.code)
document(current.code)

#build(current.code, path="~/Desktop/integrateItRevised")
#install.packages("~/Desktop/integrateItRevised/integrateItRevised_1.0.tar.gz", repos = NULL, type="source")

#library(integrateItRevised)

example.x <- 1:25
example.y <- (example.x - 10)^2 + 5
example.trap <- integrateIt("Trap", example.x, example.y, 4, 22)
example.simp <- integrateIt("Simp", example.x, example.y, 4, 22)

print(example.trap)
plot(example.trap)
print(example.simp)

print(integrateIt("Simp", example.x, example.y, 4, 21))
print(integrateIt("Trap", example.x, example.y, 4.5, 22.5))

tryx<-seq(-5, 5, by=.95)
tryy<-dnorm(tryx)

exampTrap<-integrateIt("Trap", tryx, tryy, -5, 4.5)
exampSimp<-integrateIt("Simp", tryx, tryy, -5, 4.5)
print(exampTrap)
plot(exampTrap)
print(exampSimp)

x <- c(1, 3, 5, 7, 9)
y <- x^2
y[3] <- NA

print(integrateIt("Simp", x, y, 1, 5))

