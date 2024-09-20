#' Lab 5 Function: Binomial experiment simulation
#'
#' @param iter number of experiments to perform
#' @param n number of trials to conduct per experiment
#' @param p probability of success
#'
#' @return prints a barplot and success table
#' @export
#'
#' @examples
#' mybin()
mybin=function(iter=100,n=10, p=0.7) {
  # make a empty matrix to hold the samples
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)

  # vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter) {
    # fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    # calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  # table of successes
  succ.tab=table(factor(succ,levels=0:n))
  # barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
