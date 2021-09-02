#' Auxiliary for control PartialSplines fitting
#' @description It returns an error if at least one condition is not satisfied, it has an internal use inside PartialSplines function
#'
#' @return
#' @export
#'
#' @examples
#' ## do not use
#'
PartialSplines.control <- function(y, x, tx, prop.knots, intercept, n.basis,
                                   alpha, tol, b.start, d.start) {
  if(!is.data.frame(x)) stop("x must be a data frame")
  y <- as.matrix(y)
  n <- dim(y)[1]

  if(n != dim(x)[1]) stop("Number of observation  of x and y are different")
  if(!is.vector(tx)) stop('tx must be a vector, only one spline-variable admitted')
  if(n != length(tx)) stop("Number of observation  of tx and y are different")
  if(!is.null(alpha)) {
    if(alpha > 1 | alpha < 0) stop('Alpha must be in (0, 1)') }
  if(!is.null(prop.knots)) {
    if(prop.knots > 1) stop("prop.knots can't be greater than 1")
  }
  if(intercept) MM <- model.matrix(y ~ ., data = x)
  else MM <- model.matrix(y ~ . -1, data = x)

  if(!is.null(b.start)) {
    if(intercept) MM <- model.matrix(y ~ ., data = x)
    else MM <- model.matrix(y ~ . -1, data = x)

    if(dim(MM)[2] != length(b.start)) stop("Wrong length of b.start")
  }

  if(!is.null(d.start)) {
    if(length(d.start) != n.basis) stop('Length of d.start must be equal to n.basis')
  }
}
