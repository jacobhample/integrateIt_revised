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
  n <- length(object@x) - 1
  h <- (object@x[length(object@x)] - object@x[1]) / n
  X <- seq(object@a, object@b, by = h)
  y.indices <- which(round(object@x, 5) %in% round(X, 5))
  Y <- object@y[y.indices]
  n <- length(X) - 1
  
  oddLength <- n %% 2 == 0
  sameLength <- length(object@x) == length(object@y) #x and y should be of the same length
  noNA <- all(!is.na(object@x)) & all(!is.na(object@y)) #there should be no NAs
  boundsOrder <- object@a < object@b #lower bound should be less than upper bound
  xValueOrder <- all(object@x == sort(object@x))
  xContainsBounds <- object@a %in% object@x & object@b %in% object@x 
  
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
