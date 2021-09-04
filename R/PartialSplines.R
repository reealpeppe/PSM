#' Estimates a partial splines model
#' @description PartialSplines function allows you to estimate a model where the linear relationship between the response variable and the explanatory ones is summarized by coefficients Beta; on the other hand, coefficients Delta explain the unspecified functional relationship between y and the spline variable 'tx'.
#'
#' @param y response variable, must be a vector
#' @param x data frame of explanatory variables
#' @param tx spline variable, must be a vector
#' @param prop.knots proportion of nodes to be interpolated, if NULL the value will be estimated through cross validation
#' @param intercept if model has to be estimated with an intercept
#' @param n.basis number of basis for estimating the B-spline
#' @param alpha tuning parameter for the bending of the spline, if NULL the value will be estimated through cross validation
#' @param tol tolerance parameter
#' @param b.start initial values for beta parameters
#' @param d.start initial values for delta parameters
#' @param trace if TRUE, information is printed during the running of optimization process
#' @param max.iter maximum number of iterations
#' @param r sequence of values form which the basis will be estimated. It mainly has an internal use, it's advisable to not change it
#'
#' @return PartialSplines return an object of class "PSM" which is a list containing several objects
#' \itemize{
#'   \item beta - optimal estimates of linear parameters
#'   \item delta - optimal estimates of spline parameters
#'   \item call - call of the function
#'   \item Z - matrix Z of spline coefficients
#'   \item y - response variable
#'   \item X - model matrix of linear variables
#'   \item tx - spline variable
#'   \item data - data frame of input explanatory varibales
#'   \item r - vector of points in which basis functions were calculated
#'   \item K - matrix used in backfitting
#'   \item time - execution time
#'   \item iter - number of iteration until convergence
#'   \item alpha - value of alpha used
#'   \item prop.knots - proportion of nodes interpolated
#'   \item details - list containing other elements for internal use
#'
#' }
#' @export
#'
#' @examples
#' x <- data.frame(x = rnorm(100))
#' tx <- rnorm(100)
#' eps <- rnorm(100)
#' y <- x[,1] + tx^4 + eps
#' psm <- PartialSplines(y, x, tx)
#'
#' @importFrom splines bs
#' @import graphics
PartialSplines <- function(y, x, tx, prop.knots = NULL, intercept = T, n.basis = 15, alpha = NULL, tol = 10^-4, b.start = NULL, d.start = NULL, trace = T, max.iter = 100, r = NULL) {
  require(splines)
  time.init <- Sys.time()
  cl <- match.call()
  PartialSplines.control(y = y, x = x, tx = tx, prop.knots = prop.knots, intercept = intercept, n.basis = n.basis, alpha = alpha, b.start = b.start, d.start = d.start)

  if(is.null(r)) {
    tx.range <- range(tx)[2] - range(tx)[1]
    r <- seq(min(tx) - tx.range * .15, max(tx) + tx.range * .5, length.out = 500)

  }

  nodi <- seq(min(r), max(r), length.out = n.basis - 2)

  rang <- range(r)

  basi <- bs(r, knots = nodi, Boundary.knots = rang)[, - (n.basis + 1)]
  deltax <- r[2] - r[1]
  basi.d2 <- apply(basi, MARGIN = 2, FUN = function(x) diff(diff(x))/(deltax)^2) # derivata seconda

  fun.k <- function(i, j) sum(basi.d2[, i] * basi.d2[, j]) * deltax
  K <- outer(1:n.basis, 1:n.basis, FUN = Vectorize(fun.k))

  n <- dim(x)[1]

  if(is.null(alpha)) {
    if(is.null(prop.knots)) {cv <- psm.cv(y, x, tx)
    alpha <- cv$alpha.opt ; prop.knots <- cv$knots.opt}
    else {cv <- psm.cv(y, x, tx, knots.v = prop.knots)
    alpha <- cv$alpha.opt ; prop.knots <- cv$knots.opt}
  }

  if(is.null(prop.knots)) {
    cv <- psm.cv(y, x, tx, alpha.v = alpha)
    alpha <- cv$alpha.opt ; prop.knots <- cv$knots.opt}


  Q <- round(prop.knots * n)
  if(Q == 1) Q <- 2

  index.interp.points <- order(tx)[seq(1, n, length.out = Q)]
  N <- mat.or.vec(n, Q)
  for (i in 1:Q) {
    N[index.interp.points[i], i] <- 1
  }

  Z <- bs(tx[index.interp.points], knots = nodi, Boundary.knots = rang)[, - (n.basis + 1)]
  if(intercept) modlin <- lm(y ~ ., data = x)
  else modlin <- lm(y ~ . -1, data = dati)

  if(is.null(b.start)) b.start <- coef(modlin)
  if(is.null(d.start)) d.start <- rep(1, n.basis)

  coef.names <- names(coef(modlin))
  b.old <- b.start
  d.old <- d.start

  y <- matrix(y)
  X <- model.matrix(modlin)
  NZ <- N %*% Z
  XXt.inv <- chol2inv(chol(crossprod(X,X)))
  alpha.loc <- alpha/(1 - alpha)
  ZNNZ.aK <- t(NZ) %*% NZ + alpha.loc * K
  d.matrix <- chol2inv(chol(ZNNZ.aK)) %*% t(NZ)
  b.matrix <- XXt.inv %*% t(X)

  iter <- 1
  curr.tol <- tol + 1
  while(curr.tol > tol & iter <= max.iter) {

    d.new <- d.matrix %*% (y - X %*% b.old)
    b.new <- b.matrix %*% (y - NZ %*% d.new)

    theta <- c(d.old - d.new, b.old - b.new)
    d.old <- d.new
    b.old <- b.new
    curr.tol <- sqrt(sum(theta^2))
    if(trace) cat('Iteration ', iter, ', Tol = ', curr.tol, '\n', sep = '')
    iter <- iter + 1
  }

  y.star <- y - NZ %*% d.new
  mod <- lm(y.star ~ ., data = data.frame(y.star, x))

  time.finish <- Sys.time()
  tempo <- time.finish - time.init

  details <- list(knots.basis = nodi, intercept = intercept, N = N,
                  coef.names = coef.names, mod = mod, n.obs = n)


  out = list(beta = coef(mod), delta = as.vector(d.new), call = cl, Z = Z, y = y, X = X, tx = tx,
             data = x, r = r, K = K, time = tempo, iter = iter, alpha = alpha,
             prop.knots = prop.knots, details = details)
  attr(out, which = 'class') <- 'PSM'
  out

}
