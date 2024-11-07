#' Lab 11 Confidence Interval for mu given single sample x
#'
#' @param x single sample
#' @importFrom stats qt sd
#'
#' @return 95% confidence interval for mu
#' @export
#'
#' @examples myci(x = rnorm(30,mean=10,sd=12))
myci = function(x) {
  n=length(x)
  t=qt(0.975,n-1)
  ci=c()
  ci[1]=mean(x)-t*sd(x)/sqrt(n)
  ci[2]=mean(x)+t*sd(x)/sqrt(n)
  ci
}
