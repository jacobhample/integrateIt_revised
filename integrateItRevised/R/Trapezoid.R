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
  sameLength <- length(object@x) == length(object@y) #x and y should be of the same length
  noNA <- all(!is.na(object@x)) & all(!is.na(object@y)) #there should be no NAs
  logicalBounds <- object@a < object@b #lower bound should be less than upper bound
  #correctOrderX <- for (i in 1:length(object@x - 1)) {
  #  ifelse(object@x[i] < object@x[i + 1], TRUE, FALSE)
  #}
  
  if(!sameLength) {return("x and y must be of the same length")}
  if(!noNA) {return("There are NAs present in your data. Please remove them to continue.")}
  if(!logicalBounds) {return("a must be less than b")}
  
  if(for (i in 1:length(object@x - 1)) {
    object@x[i] > object@x[i + 1]
    }) {return("Values of x must be in order from least to greatest")}
  
  if(for (i in 1:length(object@y - 1)) {
    object@y[i] > object@y[i + 1]
    }) {return("Values of y must be in order from least to greatest")}
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
            cat("Estimated integral using trapezoidal method: \n")
            cat(x@estInt)
          }
)

#' @export
setMethod("plot", "Trapezoid", 
          function(x, y, a, b) {
            obj <- x
            
            n <- length(obj@x) - 1
            h <- (obj@x[length(obj@x)] - obj@x[1]) / n
            X <- seq(obj@a, obj@b, by = h)
            y.indices <- which(round(obj@x, 5) %in% round(X, 5))
            Y <- y[y.indices]
            n <- length(X) - 1
            
            plot(NULL, xlim = c(min(X) - 1, max(X) + 1), ylim = c(min(Y) - 1, max(Y) + 1),
                 main = "Trapezoids", xlab = "X Values", ylab = "Y Values")
            sapply(1:X[n], function(i) segments(X[i], Y[i], X[i + 1], Y[i + 1]))
            segments(X[1], 0, X[n + 1], 0)
            sapply(1:X[n + 1], function(i) segments(X[i], 0, X[i], Y[i]))
            
          }
)


