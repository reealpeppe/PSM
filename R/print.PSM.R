#' Print function for a Partial Splines model
#'
#' @description Print function of an object of class 'PSM' will give you the main details of the model such as linear coefficients and their significance t-test, delta coefficients and a few more things
#' @param object an object of class 'PSM'
#' @param ... further arguments passed to the generic function 'print'
#'
#' @return
#' @export
#'
#' @examples
#'
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#' print(psm)
print.PSM <- function(object, ...) {
  cat("Printing an object of class", sQuote("PSM"), "\n\n")
  cat("Call:", deparse(object$call), "\n\n")
  cat('Coefficients Beta: \n')
  d <- as.vector(object$delta)
  alpha <- object$alpha
  p.knots <- object$prop.knots
  mod <- object$details$mod
  b.inf <- data.frame(summary(mod)$coefficients)
  p.values <- b.inf[ ,4]
  b.out <- data.frame(Estimate = b.inf[ ,1])
  b.out['Std. Error'] <- b.inf[ ,2] ; b.out['t value'] <- b.inf[,3]
  b.out['Pr(>|t|)'] <- p.values
  b.out[' '] = ifelse(p.values < 0.001, "***", ifelse(p.values < 0.01, "**", ifelse(p.values < 0.05, "*", ifelse(p.values < 0.1, ".", " "))))
  rownames(b.out) <- names(coef(mod))

  print.data.frame(b.out, digits = 3, ...)
  cat('\n', "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", sep = '', '\n')

  cat('\nCoefficients Delta: \n')
  print(d, digits = 3, ...)
  cat('\nwith parameters:\nAlpha: ', alpha, '\nProportion of knots: ', p.knots, '\nNumber of basis: ', length(d))
}
