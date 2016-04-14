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


# Example 1
example.x <- 1:25
example.y <- (example.x - 10)^2 + 5
example.trap <- integrateIt("Trap", example.x, example.y, 4, 22)
example.simp <- integrateIt("Simp", example.x, example.y, 4, 22)

print(example.trap)
print(example.simp)
plot(example.trap)
plot(example.simp)


# Example 2
tryx<-seq(-5, 5, by = .95)
tryy<-dnorm(tryx)

exampTrap<-integrateIt("Trap", tryx, tryy, -5, 4.5)
exampSimp<-integrateIt("Simp", tryx, tryy, -5, 4.5)
print(exampTrap)
print(exampSimp)
plot(exampTrap)
plot(exampSimp)


# Validity Checks: The following statements should produce errors
x <- c(2, 4, 6, 8, 10, 12)
y <- c(3, 5, 7, 9, 11, 13)
print(integrateIt("Simp", x, y, 2, 12))

x <- c(2, 4, 6, 8, 10)
y <- c(3, 5, 7, 9, 11, 13)
print(integrateIt("Trap", x, y, 4, 8))
print(integrateIt("Simp", x, y, 4, 8))

x <- c(2, 4, 6, 8, 10, 12)
y <- c(3, 5, NA, 9, 11, 13)
print(integrateIt("Trap", x, y, 4, 8))
print(integrateIt("Simp", x, y, 4, 8))

x <- c(2, 4, 6, 8, 10, 12)
y <- c(3, 5, 7, 9, 11, 13)
print(integrateIt("Trap", x, y, 8, 4)) #These two actually crash before the validity test,
print(integrateIt("Simp", x, y, 8, 4)) #but would not pass the "boundsOrder" test regardless

x <- c(2, 4, 6, 10, 8, 12)
y <- c(3, 5, 7, 9, 11, 13)
print(integrateIt("Trap", x, y, 4, 8))
print(integrateIt("Simp", x, y, 4, 8))

x <- c(2, 4, 6, 8, 10, 12)
y <- c(3, 5, 7, 9, 11, 13)
print(integrateIt("Trap", x, y, 4, 16))
print(integrateIt("Simp", x, y, 4, 16))


