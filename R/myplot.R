#' Polynomial function
#'
#' @param x a quantitative value
#'
#' @return a function in terms of x or f(x)
#' @export
#'
#' @examples
#' myplot(1)
myplot <- function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
