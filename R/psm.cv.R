#' Cross validation for Partial Splines Model
#' @description The aim of this function is to find the best combination of parameters 'prop.knots' and 'alpha' for Partial Splines function through a k-fold Cross Validation.
#' @param y response variable, must be a vector
#' @param x explanatory variables, must be a dataframe
#' @param tx spline variable, must be a vector
#' @param k.fold number of folds
#' @param alpha.v vector of alpha values to compare
#' @param knots.v vector of prop.knots values to compare
#' @param plot.it if TRUE, it plots a point for each combination of the parameters; the size of the point is related to the MSE on the test-set. The smaller point is circled in red
#' @param ... further arguments passed to 'PartialSplines'
#'
#' @return returns a list containing the optimal values for alpha and prop.knots and a dataframe with the mse for each combination of the parameters
#' @export
#'
#' @examples
#'
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' cv <- psm.cv(y, x, tx)
#' alpha <- cv$alpha.opt
#' prop.knots <- cv$knots.opt
#'
psm.cv <- function(y, x, tx, k.fold = 4, alpha.v = c(0.0001, 0.001, 0.01, 0.1),
                   knots.v = c(0.1, 0.2, 0.3), plot.it = F, ...) {

  n <- dim(as.matrix(y))[1]
  if (k.fold > n)
    stop("k.fold can't be greater than sample size")
  shuffle <- sample(1:n, size = n, r = F)
  y <- as.matrix(y)[shuffle, , drop = F]
  x <- x[shuffle, , drop = F]
  tx <- tx[shuffle]
  n.a <- length(alpha.v)
  n.k <- length(knots.v)

  groups <- rep(floor(n/k.fold), k.fold)
  sum.g <- k.fold * groups[1]
  if (sum.g < n){
    groups[1:(n-sum.g)] <- groups[1:(n-sum.g)] + 1
  }

  tbl <- expand.grid(alpha = alpha.v, knots = knots.v)

  kfold.fun <- function(row.tbl){
    i1 <- 1
    i2 <- 0
    eqm.i <- 0
    alpha <- row.tbl[1]
    knots <- row.tbl[2]
    for (i in 1:k.fold) {

      i2 <- i2 + groups[i]


      train.psm <- PartialSplines(y[-c(i1:i2), , drop = F],
                                  x[-c(i1:i2), , drop = F], tx[-c(i1:i2)], alpha = alpha,
                                  prop.knots = knots, trace = F, ...)
      pred <- predict(train.psm, x = x[i1:i2, , drop = F],
                      tx = tx[i1:i2])
      eqm.i <- eqm.i + crossprod(y[i1:i2] - pred)

      i1 <- i1 + groups[i]
    }

    return(eqm.i/n)
  }



  tbl$mse <- apply(tbl, MARGIN = 1, FUN = kfold.fun)

  best <- which.min(tbl$mse)
  out <- list(alpha.opt = tbl[best, 1], knots.opt = tbl[best,
                                                        2], table = tbl)
  if (plot.it) {
    cex.plot <- tbl$mse/max(tbl$mse) + .7
    plot(tbl[, 1], tbl[, 2], cex = cex.plot, pch = 16, xlab = "alpha",
         ylab = "Prop. knots")
    points(tbl[best, 1], tbl[best, 2], cex = (cex.plot[best] +
                                                1), col = 2)
  }
  out
}



