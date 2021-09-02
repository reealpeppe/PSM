#' Extract Model Coefficients
#' @description Extracts model coefficients beta and delta
#' @param object an object of class 'PSM' returned by PartialSplines function
#'
#' @return a list containing the coefficients
#' @export
#'
#' @examples
#'
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#' coef(psm)
#'
coef.PSM <- function(object) {
  b <- object$beta
  d <- object$delta
  list(beta = b, delta = d)
}
