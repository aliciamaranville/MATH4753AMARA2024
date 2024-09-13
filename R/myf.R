#' Piecewise regression
#'
#' @param x independent variable
#' @param xk knot point
#' @param coef vector of coefficients
#'
#' @return value of piecewise linear regression at value x
#' @export
#'
#' @examples
#' myf(0, 18, coef=c(6.2188237,0.6741809,-0.5803229))
myf = function(x,xk,coef){
  coef[1]+coef[2]*(x) + coef[3]*(x-xk)*(x-xk>0)
}
