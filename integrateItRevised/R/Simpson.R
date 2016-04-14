#' A Simpson object
#'
#' Objects of class \code{Simpson} are integrals estimated using Simpson's rule
#'
#'
#' #' An object of the class Simpson has the following slots:
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @aliases Simpson-class initialize,Simpson-method plot,Simpson-method print,Simpson-method
#' @rdname Simpson

#' @export
setClass(Class = "Simpson",
         representation = representation(
           x = "numeric",
           y = "numeric",
           a = "numeric",
           b = "numeric",
           estInt = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           a = c(),
           b = c(),
           estInt=c()
         ))

setValidity("Simpson", function(object) {
  # Subsets x into X, a vector with x values between a and b
  n <- length(object@x) - 1
  h <- (object@x[length(object@x)] - object@x[1]) / n
  X <- seq(object@a, object@b, by = h)
  y.indices <- which(round(object@x, 5) %in% round(X, 5))
  Y <- object@y[y.indices]
  n <- length(X) - 1
  
  # Tests
  oddLength <- n %% 2 == 0
  sameLength <- length(object@x) == length(object@y)
  noNA <- all(!is.na(object@x)) & all(!is.na(object@y))
  boundsOrder <- object@a < object@b
  xValueOrder <- all(object@x == sort(object@x))
  xContainsBounds <- object@a %in% object@x & object@b %in% object@x 
  
  # Return statements
  if(!oddLength) {return("Simpson's rule requires that you integrate over an odd number of x values")}
  if(!sameLength) {return("x and y must be of the same length")}
  if(!noNA) {return("There are NAs present in your data. Please remove them to continue.")}
  if(!boundsOrder) {return("a must be less than b")}
  if(!xValueOrder) {return("Values within x must be in ascending order")}
  if(!xContainsBounds) {return("a and b must be values within x")}
  }
)

#' @export
setMethod("initialize", "Simpson", 
          function(.Object, ...) {
            value = callNextMethod()
            validObject(value)
            return(value)
          }
)

#' @export
setMethod("print", "Simpson",
          function(x) {
            cat("Estimated integral using Simpson's rule: \n")
            cat(x@estInt)
          }
)

#' @export
setMethod("plot", "Simpson", 
          function(x, y = NULL) {
            obj <- x
            
            # Subsets x into X, a vector with x values between a and b
            n <- length(obj@x) - 1
            h <- (obj@x[length(obj@x)] - obj@x[1]) / n
            X <- seq(obj@a, obj@b, by = h)
            y.indices <- which(round(obj@x, 5) %in% round(X, 5))
            Y <- obj@y[y.indices]
            n <- length(X) - 1
            
            # Extracts values from X and puts them into 3 vectors
            x1 <- X[seq(1, (n - 1), by = 2)]
            y1 <- Y[seq(1, (n - 1), by = 2)]
            x2 <- X[seq(2, n, by = 2)]
            y2 <- Y[seq(2, n, by = 2)]
            x3 <- X[seq(3, (n + 1), by = 2)]
            y3 <- Y[seq(3, (n + 1), by = 2)]
            
            # Function to make parabolas 
            parabolas <- function(x, index = 1, x1, x2, x3, y1, y2, y3) {
              part1 <- y1[index] * (x - x2[index]) * (x - x3[index]) / ((x1[index] - x2[index]) * (x1[index] - x3[index]))
              part2 <- y2[index] * (x - x1[index]) * (x - x3[index]) / ((x2[index] - x1[index]) * (x2[index] - x3[index]))
              part3 <- y3[index] * (x - x1[index]) * (x - x2[index]) / ((x3[index] - x1[index]) * (x3[index] - x2[index]))
              return(part1 + part2 + part3)
            }
            
            # Creates the plot
            plot(NULL, xlim = c(min(X), max(X)), ylim = c(min(Y), max(Y)),
                 main = "Simpson's Parabolas", xlab = "X Values", ylab = "Y Values")
            
            segments(X, Y, X, 0)
            segments(X[1], 0, X[n + 1], 0)
            
            for(i in 1:length(x1)) {
              plotX <- seq(x1[i], x3[i], length = 100)
              plotY <- parabolas(plotX, index = i, x1 = x1, x2 = x2, x3 = x3, y1 = y1, y2 = y2, y3 = y3)
              lines(plotX, plotY)
            }
            
            points(X, Y, pch = 18, col = "red", cex = 1.1)
          }
)

