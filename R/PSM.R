#' PSM: A package to estimate Partial Splines model
#'
#' The PSM package provides the main function "PartialSplines" that allows the user to estimate a model through which the response variable is a explained by its linear relationship with a group of explanatory variables and a spline variable with which the functional relationship is unknown. The  package includes some method functions to simplify user's work.
#' Unfortunately the first version of PSM do not permit to specify the linear part through a formula, therefore the only way to add interaction between variables is by doing it manually inside the input dataframe 'x'. This is one of the first improvements that will be made.

#'
#' @section PSM functions:
#' \itemize{
#' \item PartialSplines - to estimate the model
#' \item psm.cv - to optimize parameters through cross validation
#' \item coef - method function
#' \item predict - method function
#' \item fitted - method function
#' \item print - method function
#' \item plot - method function
#' \item other functions for internal use
#'}
#'
#' @author
#' Giuseppe Reale, Salvatore Mirlocca
#' @docType package
#' @name PSM
NULL
#> NULL
