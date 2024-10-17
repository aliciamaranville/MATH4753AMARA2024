#' Lab 8 Central Limit Theorem for Binomial Distribution
#'
#' @param n sample size
#' @param iter number of iteratiions
#' @param p probability of success on a single trial
#' @importFrom stats rbinom density
#' @importFrom graphics hist lines
#'
#' @return vector of sample means and histogram of sample means distribution
#' @export
#'
#' @examples mycltb(n=10,iter=10000,p=0.3)
mycltb=function(n,iter,p=0.5){
  x=NULL
  y=rbinom(n*iter,size=n,prob=p) # create random sample of size n*iter
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE) # create matrix of n rows and iter columns to hold sample data
  w=apply(data,2,mean) # find mean of each iteration's sample
  h=hist(w,plot=FALSE) # create histogram of sample means
  ymax=max(h$density)
  ymax=1.1*ymax # use max sample mean to calculate ymax for good bounds
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean") # create histogram
  lines(density(w),col="Blue",lwd=3) # sample density curve
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3) # normal distribution curve according to CLT
  w
}
