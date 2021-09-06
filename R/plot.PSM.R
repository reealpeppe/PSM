#' Plot for a PSM object
#'
#' @description There are two plots: in the first one there are the residuals while the second one shows the observed against fitted values
#' @param object an object of class 'PSM'
#' @param legend.pos position of the legend. Can be moved according to points position
#' @param ... further arguments passed to the generic 'plot' function
#'
#' @return
#' @export
#'
#' @examples
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#' plot(psm)

plot.PSM <- function(object, legend.pos = 'topleft', ...) {
  y <- object$y
  y.hat <- fitted(object)
  tx <- object$tx
  res <- y - y.hat
  plot(y.hat, res, xlab = expression(hat(y)), ylab = "Residuals")
  abline(h = 0, col = 2)
  readline(prompt = "Type <Enter> to go to the next plot: ")
  plot(tx, y, main = 'Fitted vs observed values', ...)
  points(tx, y.hat, col = 4, pch = 4)
  legend(legend.pos, pch = c(16, 4), col = c(1, 4), legend = c("Observed" ,'Fitted'))

}
