\name{RCourseLab4-package}
\docType{package}
\title{
\packageTitle{RCourseLab4}
}
\description{
\packageDescription{RCourseLab4}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{RCourseLab4}
\packageIndices{RCourseLab4}
This package contains several methods of linear regression model including vignette manual. 
}
\author{
\packageAuthor{RCourseLab4}

Maintainer: \packageMaintainer{RCourseLab4}
}
\references{
https://en.wikipedia.org/wiki/Linear_least_squares_(mathematics)#Orthogonal_decomposition_methods.
https://en.wikipedia.org/wiki/QR_decomposition.
}
\keyword{ Linear Regression, ggplot }

\examples{linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length,data=iris)}
