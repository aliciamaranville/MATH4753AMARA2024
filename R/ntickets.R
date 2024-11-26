#' Project 1 Overbooking Problem to find Optimal Tickets Sold
#'
#' @param N number of seats in the flight
#' @param gamma probability of plane being overbooked
#' @param p probability of a 'show'
#' @importFrom graphics abline layout
#' @importFrom stats pbinom uniroot
#'
#' @return named list and 2 plots giving optimal tickets sold (discrete and continuous)
#' @export
#'
#' @examples ntickets(N=200, gamma=0.02, p=0.95)
ntickets = function(N, gamma, p) {
  x=NULL
  # discrete calculation: use binomial
  fd <- function(n) 1-gamma-pbinom(q=N, size=n, prob=p)
  for (n in N:(N*1.1)) {
    if (fd(n) >= 0) {
      nd <- n # breaks at first positive value, closest to zero
      break
    }
  }

  # continuous calculation: use normal
  fc <- function(x) 1-gamma-pnorm(q=N+0.5, mean=x*p, sd=sqrt(x*p*(1-p)))
  nc <- uniroot(fc, interval=c(N,N*1.10))[[1]]

  # named list
  print(list(nd=nd, nc=nc, N=N, p=p, gamma=gamma))

  # discrete plot

  layout(matrix(1:2, nrow=2, ncol=1))

  x <- seq(N,N*1.1, by=1)
  plot(x, y=fd(x), type = "b", pch = 16, col = "navy", main = paste("Objective Vs n to find optimal tickets sold\n (",nd,") gamma=,",gamma," N=",N," discrete", sep=""), xlab = "n", ylab = "Objective")
  abline(h = 0, col = "red", lwd = 2)
  abline(v = nd, col = "red", lwd = 2)

  # continuous plot
  curve(1-gamma-pnorm(q=N+0.5, mean=x*p, sd=sqrt(x*p*(1-p))), xlim=c(N,N*1.1), main=paste("Objective Vs n to find optimal tickets sold\n (",nc,") gamma=",gamma," N=",N," continuous",sep=""), xlab = "n", ylab = "Objective")
  abline(h = 0)
  abline(v = nc)
}
