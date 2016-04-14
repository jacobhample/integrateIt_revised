#' A Trapezoid object
#'
#' Objects of class \code{Trapezoid} are integrals estimated using the trapezoidal rule
#'
#'
#' An object of the class Trapezoid has the following slots:
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @aliases Trapezoid-class initialize,Trapezoid-method plot,Trapezoid-method print,Trapezoid-method
#' @rdname Trapezoid
#' @export

setClass(Class = "Trapezoid",
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

setValidity("Trapezoid", function(object) {
  # Tests
  sameLength <- length(object@x) == length(object@y)
  noNA <- all(!is.na(object@x)) & all(!is.na(object@y))
  boundsOrder <- object@a < object@b
  xValueOrder <- all(object@x == sort(object@x))
  xContainsBounds <- object@a %in% object@x & object@b %in% object@x
  
  # Return statements
  if(!sameLength) {return("x and y must be of the same length")}
  if(!noNA) {return("There are NAs present in your data. Please remove them to continue.")}
  if(!boundsOrder) {return("a must be less than b")}
  if(!xValueOrder) {return("values within x must be in ascending order")}
  if(!xContainsBounds) {return("a and b must be values within x")}
  }
)

#' @export
setMethod("initialize", "Trapezoid", 
          function(.Object, ...) {
            value = callNextMethod()
            validObject(value)
            return(value)
          }
)

#' @export
setMethod("print", "Trapezoid",
          function(x){
            cat("Estimated integral using the trapezoidal rule: \n")
            cat(x@estInt)
          }
)

#' @export
setMethod("plot", "Trapezoid", 
          function(x, y = NULL) {
            obj <- x
            
            # Subsets x into X, a vector with x values between a and b
            n <- length(obj@x) - 1
            h <- (obj@x[length(obj@x)] - obj@x[1]) / n
            X <- seq(obj@a, obj@b, by = h)
            y.indices <- which(round(obj@x, 5) %in% round(X, 5))
            Y <- obj@y[y.indices]
            n <- length(X) - 1
            
            # Creates the plot
            plot(NULL, xlim = c(min(X), max(X)), ylim = c(min(Y), max(Y)),
                 main = "Trapezoids", xlab = "X Values", ylab = "Y Values")
            sapply(1:n, function(i) segments(X[i], Y[i], X[i + 1], Y[i + 1]))
            segments(X[1], 0, X[n + 1], 0)
            sapply(1:(n + 1), function(i) segments(X[i], 0, X[i], Y[i]))
            points(X, Y, pch = 15, col = "blue", cex = 0.8)
          }
)


