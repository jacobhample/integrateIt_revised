#' Estimates the integral of a mathematical function
#'
#' Estimates the integral of a mathematical funciton using either the trapezoidal rule or Simpson's rule
#'
#' @param rule Which rule, either 'Trap' or 'Simp', one would like to use to estimate the integral
#' @param x A vector of values
#' @param y A vector of evaluated values
#' @param a Lower bound of the integral
#' @param b Upper bound of the integral
#'
#' @return An object of either class `Trapezoid' or class 'Simpson' that contains
#' \itemize{
#' \item \code{x} A vector of values
#' \item \code{y} A vector of evaluated values
#' \item \code{a} Lower bound of the integral
#' \item \code{b} Upper bound of the integral
#' \item \code{estInt} Estimated integral
#' }
#'
#' @author Jacob H. Hample: \email{jacob.hample@@wustl.edu}
#' @examples
#'
#' example.x <- 1:25
#' example.y <- (example.x)^2
#' integrateIt("Trap", example.x, example.y, 4, 22)
#' integrateIt("Simp", example.x, example.y, 4, 22)
#' 
#' @seealso \code{\link{Trapezoid}}, \code{\link{Simpson}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method

#' @export
integrateIt<-function(rule = "Trap", x, y, a, b) {
  # Subsets x into X, a vector with x values between a and b
  n <- length(x) - 1
  h <- (x[length(x)] - x[1]) / n
  X <- seq(a, b, by = h)
  y.indices <- which(round(x, 5) %in% round(X, 5))
  Y <- y[y.indices]
  n <- length(X) - 1
  
  if(rule == "Trap") {
    estInt <- h / 2 * (Y[1] + sum(2 * Y[2:n]) + Y[n + 1])
    out <- new("Trapezoid", x = x, y = y, a = a, b = b, estInt = estInt)
  }
  else if (n == 2) { #Simpson's rule when there are only 3 x values
    estInt <- Y[1] + 4 * Y[2] + Y[3]
    out <- new("Simpson", x = x, y = y, a = a, b = b, estInt = estInt)
    }
  else { #Simpson's rule when there are more than 3 x values
    estInt <- h / 3 * (Y[1] + 4 * sum(Y[seq(2, n, by = 2)]) + 2 * sum(Y[seq(3, n - 1, by = 2)]) + Y[n + 1])
    out <- new("Simpson", x = x, y = y, a = a, b = b, estInt = estInt)
  }
}


