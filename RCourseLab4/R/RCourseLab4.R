#' A package for Lab 4 Assigments.
#'
#' This package contains several methods of linear regression model including vignette manual. 
#' We are using linear algebra to create the most basic functionality in the R package. The package contains
#' R script based on RC class where we first defined the fields and then methods. The methods which implemented for
#' calculating linear regression model is defined later.
#'
#' 
#' @section linreg class:
#' linreg() is a RC class which returns several methods for linear regression analysis.
#' @seealso \link[RCourseLab4]{linreg-class}
#' @docType package
#' 
#' @name RCourseLab4
#' 
#' @references \href{https://en.wikipedia.org/wiki/Linear_regression}{Linear Regression Model}
#' 
#' @examples linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)
NULL
