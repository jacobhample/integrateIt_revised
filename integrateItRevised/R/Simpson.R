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
  sameLength <- length(object@x) == length(object@y) #x and y should be of the same length
  noNA <- all(!is.na(object@x)) & all(!is.na(object@y)) #there should be no NAs
  logicalBounds <- object@a < object@b #lower bound should be less than upper bound
  
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
setMethod("initialize", "Simpson", 
          function(.Object, ...) {
            value = callNextMethod()
            validObject(value)
            return(value)
          }
)

#' @export
setMethod("print", "Simpson",
          function(x){
            cat("Estimated integral using Simpson's rule: \n")
            cat(x@estInt)
          }
)
