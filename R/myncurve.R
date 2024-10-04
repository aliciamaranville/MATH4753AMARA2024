#' Lab 6 Shaded Area Curve Function
#'
#' @param mu mean of distribution
#' @param sigma standard deviation of distribution
#' @param a probability value in P(X<=a)
#'
#' @return graph with shaded curve and list of mu, sigma, and probability
#' @export
#'
#' @examples myncurve(0,1,0)
myncurve = function(mu, sigma, a) {
  curve(dnorm(x,mean=mu, sd=sigma), xlim=c(mu-3*sigma, mu+3*sigma))
  xcurve <- seq(mu-3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0,ycurve,0), col='red')
  prob <- pnorm(a, mean=mu, sd=sigma)
  prob <- round(prob, 4)
  list(mu=mu, sigma=sigma, prob=prob)
}
